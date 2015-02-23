(*
 * Copyright (c) 2014 Magnus Skjegstad <magnus@skjegstad.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt
open Dns
open Printf

module Client = Xs_client_lwt.Client(Xs_transport_lwt_unix_client)

type vm_stop_mode = VmStopDestroy | VmStopSuspend | VmStopShutdown

type vm_state =
  | Running of int (* domid *)
  | Suspended of string (* path *)
  | Halted

type vm_metadata = {
  vm_name: string;              (* Unique name of the VM *)
  vm_kernel : string;              (* Kernel file name *)
  memory_kb: int64;             (* VM memory in KiB *)
  nics: string list;            (* Name of the nics to connect VIF to *)
  vif_hotplug_scripts: string list;
  query_response_delay : float; (* in seconds, delay after startup before
                                   sending query response *)
  boot_options : string option; (* Extra parameters to pass to unikernel on boot *)
  vm_ttl : int;                 (* TTL in seconds. VM is stopped [vm_ttl]
                                   seconds after [requested_ts] *)
  how_to_stop : vm_stop_mode;   (* how to stop the VM on timeout *)
  mutable started_ts : int;     (* started timestamp *)
  mutable requested_ts : int;   (* last request timestamp *)
  mutable total_requests : int;
  mutable total_starts : int;

  (* key/value pari to watch on startup *)
  wait: (string * string) option;
}

type t = {
  db : Loader.db;                         (* DNS database *)
  log : string -> unit;                   (* Log function *) 
  debug : bool;
  forward_resolver : Dns_resolver_unix.t; (* DNS to forward request to if no
                                             local match *)
  domain_table : (Name.domain_name, vm_metadata) Hashtbl.t;
  (* vm hash table indexed by domain *)
  name_table : (string, vm_metadata) Hashtbl.t;
  (* vm hash table indexed by vm name *)

  context : Xenlight.ctx Lazy.t
}

(* All libxl calls need one of these to say where the logs should go.
   We delay the creation because it can fail (e.g. due to insufficient privileges)
   and we would still like people to be able to consult command-line arguments *)
let context dolog = lazy (
  Xenlight.register_exceptions ();
  Printexc.register_printer (function
      | Xenlight.Error(error, msg) ->
        Some (Printf.sprintf "Xenlight.Error(%s, %s)" (Xenlight.string_of_error error) msg)
      | _ ->
        None
    );
  let logger =
    let open Xentoollog in
    let vmessage _level errno ctx msg =
      let errno_str = match errno with None -> "" | Some s -> Printf.sprintf ": errno=%d" s
      and ctx_str = match ctx with None -> "" | Some s -> Printf.sprintf "%s" s in
      (*Printf.fprintf stderr "%s%s: %s\n%!" ctx_str errno_str msg in*)
      (if dolog then (Printf.printf "# %s%s: %s\n" ctx_str errno_str msg) else ()) in
    let progress _ctx what percent dne total =
      let nl = if dne = total then "\n" else "" in
      Printf.fprintf stderr "\rProgress %s %d%% (%Ld/%Ld)%s" what percent dne total nl in
    create "Xentoollog.logger" { vmessage; progress } in
  try
    Xenlight.ctx_alloc logger
  with e ->
    fprintf stderr "Unable to talk to Xen. Please check that:\n";
    fprintf stderr "- Xen is running (try cat /sys/hypervisor/type)\n";
    fprintf stderr "- xenfs is mounted\n";
    fprintf stderr "- Xenstore is running\n";
    fprintf stderr "- you have sufficient privileges\n";
    fprintf stderr "\nRaw error was: %s\n%!" (Printexc.to_string e);
    exit 1
)

let create log forward_resolver vm_count debug =
  { db = Loader.new_db ();
    log;
    debug;
    forward_resolver = forward_resolver;
    domain_table = Hashtbl.create ~random:true vm_count;
    name_table = Hashtbl.create ~random:true vm_count;
    context = context debug;
  }

(* fallback to external resolver if local lookup fails *)
let fallback t _class _type _name =
  Dns_resolver_unix.resolve t.forward_resolver _class _type _name
  >>= fun result ->
  return (Some (Dns.Query.answer_of_response result))


let string_of_info = function
  | Running _ -> "running"
  | Suspended _ -> "suspended"
  | Halted -> "halted"

let suspend_filename vm = vm.vm_kernel ^ "." ^ vm.vm_name ^ ".suspend"

let file_readable filename =
  Lwt.catch
    (fun () ->
       Lwt_unix.access filename [ Lwt_unix.R_OK ]
       >>= fun () ->
       return true
    ) (fun _ -> return false)

let get_vm_state t vm =
  let domids = List.map (fun di -> di.Xenlight.Dominfo.domid) (Xenlight.Dominfo.list (Lazy.force t.context)) in
  Xs.make ()
  >>= fun xsc ->
  Xs.(immediate xsc
        (fun h ->
           let rec loop = function
             | domid :: rest ->
               read h (Printf.sprintf "/local/domain/%d/name" domid)
               >>= fun name ->
               if name = vm.vm_name
               then return (Some domid)
               else loop rest
             | [] -> return_none in
           loop domids
        )
     ) >>= function
  | Some domid -> return (Running domid)
  | None ->
    let filename = suspend_filename vm in
    ( file_readable filename
      >>= function
      | true -> return (Suspended filename)
      | false -> return Halted )

let log t fmt = Printf.ksprintf (fun str -> t.log (str ^ "\n")) fmt

(* wait for the vm to write /local/domain/<domid>/status = "booted" *)
let wait_until_ready t vm =
  match vm.wait with
  | None  -> return_unit
  | Some (key, value) ->
  get_vm_state t vm >>= function
  | Suspended _ | Halted -> assert false
  | Running domid ->
    let path = Printf.sprintf "/local/domain/%d/%s" domid key in
    t.log (Printf.sprintf "Watching: %s...\n%!" path);
    let safe_read h k =
      Lwt.catch
        (fun () -> Xs.read h k >>= fun v -> return (Some v))
        (function Xs_protocol.Enoent _ -> return_none | e -> fail e)
    in
    Xs.make () >>= fun xsc ->
    Xs.wait xsc (fun h ->
      safe_read h path >>= function
      | None   -> fail Xs_protocol.Eagain
      | Some x -> if x = value then return_unit else fail Xs_protocol.Eagain
    ) >>= fun () ->
    log t "the VM is ready!";
    return_unit

let blocking_xenlight f =
  (* Xenlight wants to control SIGCHILD.
     TODO: can we do better than this? *)
  let old_handler = Sys.signal Sys.sigchld Sys.Signal_default in
  try
    let result = f () in
    Sys.set_signal Sys.sigchld old_handler;
    result
  with e ->
    Sys.set_signal Sys.sigchld old_handler;
    raise e

let stop_vm t vm =
  get_vm_state t vm
  >>= fun state ->
  match state, vm.how_to_stop with
  | Running domid, VmStopShutdown ->
    Xenlight.Domain.shutdown (Lazy.force t.context) domid;
    return_unit
  | Running domid, VmStopDestroy  ->
    Xenlight.Domain.destroy (Lazy.force t.context) domid ();
    return_unit
  | Running domid, VmStopSuspend  ->
    let filename = suspend_filename vm in
    Lwt_unix.openfile filename [ Unix.O_WRONLY; Unix.O_CREAT ] 0
    >>= fun fd ->
    blocking_xenlight
      (fun () ->
         try
           Xenlight.Domain.suspend (Lazy.force t.context) domid (Lwt_unix.unix_file_descr fd) ();
         with e ->
           t.log (Printf.sprintf "Failed to suspend domain: %s. Will destroy instead.\n%!" (Printexc.to_string e));
           Unix.unlink filename;
           ( try
               Xenlight.Domain.destroy (Lazy.force t.context) domid ()
             with e ->
               t.log (Printf.sprintf "Destroy failed too: %s. I'm out of bright ideas.\n%!" (Printexc.to_string e))
           );
      );
    Lwt_unix.close fd
  | (Halted | Suspended _), _ ->
    return_unit

let domain_config t vm =
  let memory_kb = vm.memory_kb in
  let nics = vm.nics in
  let context = Lazy.force t.context in
  let c_info = Xenlight.Domain_create_info.({ (default context ()) with
                                              Xenlight.Domain_create_info.xl_type = Xenlight.DOMAIN_TYPE_PV;
                                              name = Some vm.vm_name;
                                              run_hotplug_scripts = Some true;
                                            }) in
  let b_info = Xenlight.Domain_build_info.({ (default context ~xl_type:Xenlight.DOMAIN_TYPE_PV ()) with
                                             Xenlight.Domain_build_info.max_memkb = memory_kb;
                                             target_memkb = memory_kb;
                                           }) in
  let b_info_xl_type = match b_info.Xenlight.Domain_build_info.xl_type with
    | Xenlight.Domain_build_info.Pv x -> x
    | _ -> assert false in
  let b_info = Xenlight.Domain_build_info.({ b_info with
                                             xl_type = Pv { b_info_xl_type with
                                                            kernel = Some vm.vm_kernel;
                                                            cmdline = vm.boot_options;
                                                            ramdisk = None;
                                                          };
                                           }) in
  let nics = 
    let s = Array.of_list vm.vif_hotplug_scripts in
    let n = Array.of_list nics in 
    Array.init (Array.length n) 
      (fun i -> let bridge = Some (Array.get n i) in
        let script = (match s with [||] -> None | scripts -> Some (Array.get scripts (i mod Array.length scripts))) in
        Xenlight.Device_nic.({ (default context ()) with
                               Xenlight.Device_nic.mtu = 1500;
                               script;
                               bridge;
                             })) in
  Xenlight.Domain_config.({ (default context ()) with
                            c_info;
                            b_info;
                            nics;
                          })

let start_vm t vm =
  let context = Lazy.force t.context in
  get_vm_state t vm
  >>= fun state ->
  t.log (Printf.sprintf "Starting %s (%s)" vm.vm_name (string_of_info state));
  match state with
  | Running _ ->
    t.log (Printf.sprintf " --! VM is already running");
    return_unit
  | Suspended _ | Halted ->
    ( match state with
      | Suspended suspend_image ->
        t.log (Printf.sprintf " --> resuming vm...");
        Lwt_unix.openfile suspend_image [ Unix.O_RDONLY ] 0
        >>= fun fd ->
        let params = Xenlight.Domain_restore_params.default context () in
        blocking_xenlight
          (fun () ->
             try
               let domid = Xenlight.Domain.create_restore context (domain_config t vm) (Lwt_unix.unix_file_descr fd, params) () in
               Xenlight.Domain.unpause context domid
             with e ->
               fprintf stderr "Resume failed with: %s. Consider deleting suspend file %s.\n%!" (Printexc.to_string e) suspend_image
          );
        Lwt_unix.close fd
      | Halted ->
        t.log (Printf.sprintf " --> creating vm...");
        blocking_xenlight
          (fun () ->
             try
               let domid = Xenlight.Domain.create_new context (domain_config t vm) () in
               Xenlight.Domain.unpause context domid
             with e ->
               fprintf stderr "Create failed with: %s.\n%!" (Printexc.to_string e);
          );
        return_unit
      | _ -> assert false
    ) >>= fun () ->
    (* update stats *)
    vm.started_ts <- truncate (Unix.time());
    vm.total_starts <- vm.total_starts + 1;
    (* sleeping a bit *)
    Lwt_unix.sleep vm.query_response_delay

let get_vm_metadata_by_domain t domain =
  try Some (Hashtbl.find t.domain_table domain)
  with Not_found -> None

let get_vm_metadata_by_name t name =
  try Some (Hashtbl.find t.name_table name)
  with Not_found -> None

let get_stats vm =
  Printf.sprintf "VM: %s\n\
                 \ kernel: %s\n\
                 \ total requests: %d\n\
                 \ total starts: %d\n\
                 \ last start: %d\n\
                 \ last request: %d (%d seconds since started)\n\
                 \ vm ttl: %d\n"
    vm.vm_name vm.vm_kernel vm.total_requests vm.total_starts vm.started_ts vm.requested_ts
    (vm.requested_ts - vm.started_ts) vm.vm_ttl

(* Process function for ocaml-dns. Starts new VMs from DNS queries or
   forwards request to a fallback resolver *)
let process t ~src:_ ~dst:_ packet =
  let open Packet in
  match packet.questions with
  | [] -> return_none;
  | [q] -> begin
      let answer = Query.(answer q.q_name q.q_type t.db.Loader.trie) in
      match answer.Query.rcode with
      | Packet.NoError ->
        t.log (Printf.sprintf "Local match for domain %s\n"
                 (Name.domain_name_to_string q.q_name));
        (* look for vm in hash table *)
        let vm = get_vm_metadata_by_domain t q.q_name in
        begin match vm with
          | Some vm -> begin (* there is a match *)
              t.log (Printf.sprintf "Matching VM is %s\n" vm.vm_name);
              (* update stats *)
              vm.total_requests <- vm.total_requests + 1;
              vm.requested_ts <- int_of_float (Unix.time());
              start_vm t vm >>= fun () ->
              wait_until_ready t vm >>= fun () ->
              (* print stats *)
              t.log (get_stats vm);
              return (Some answer);
            end;
          | None -> (* no match, fall back to resolver *)
            t.log "No known VM. Forwarding to next resolver...\n";
            fallback t q.q_class q.q_type q.q_name
        end
      | _ ->
        t.log (Printf.sprintf "No local match for %s, forwarding...\n"
                 (Name.domain_name_to_string q.q_name));
        fallback t q.q_class q.q_type q.q_name
    end
  | _ -> return_none

(* Add domain SOA record. Called automatically from add_vm if domain
   is not registered in local DB as a SOA record *)
let add_soa t soa_domain ttl =
  Loader.add_soa_rr [] []
    (Int32.of_int (int_of_float (Unix.time())))
    (Int32.of_int ttl)
    (Int32.of_int 3)
    (Int32.of_int (ttl*2))
    (Int32.of_int (ttl*2))
    (Int32.of_int ttl)
    soa_domain
    t.db

(* true if a dns record exists locally for [domain] of [_type] *)
let has_local_domain t domain _type =
  let answer = Query.(answer domain _type t.db.Loader.trie) in
  match answer.Query.rcode with
  | Packet.NoError -> true
  | _ -> false

(* return base of domain. E.g. www.example.org = example.org, a.b.c.d = c.d *)
let get_base_domain domain =
  match domain with
  | _::domain::[tld] | domain::[tld] -> ([domain ; tld] :> Name.domain_name)
  | _ -> raise (Failure "Invalid domain name")

(* add vm to be monitored by jitsu *)
let add_vm t ~domain:domain_as_string ~name:vm_name ~kernel ~nics ~vif_hotplug_scripts ~memory_kb
    vm_ip stop_mode ~delay:response_delay ~ttl ~boot_options ~wait =
  ( file_readable kernel
    >>= function
    | false ->
      fprintf stderr "I could not read VM kernel %s\n%!" kernel;
      exit (-1)
    | true ->
      return_unit
  ) >>= fun () ->
  (* check if SOA is registered and domain is ok *)
  let domain_as_list = Name.string_to_domain_name domain_as_string in
  let base_domain = get_base_domain domain_as_list in
  let answer = has_local_domain t base_domain Packet.Q_SOA in
  if not answer then (
    t.log (Printf.sprintf "Adding SOA '%s' with ttl=%d\n"
             (Name.domain_name_to_string base_domain) ttl);
    (* add soa if not registered before *) (* TODO use same ttl? *)
    add_soa t base_domain ttl;
  );
  (* add dns record *)
  t.log (Printf.sprintf "Adding A PTR for '%s' with ttl=%d and ip=%s\n"
           (Name.domain_name_to_string domain_as_list) ttl (Ipaddr.V4.to_string vm_ip));
  Loader.add_a_rr vm_ip (Int32.of_int ttl) domain_as_list t.db;
  let existing_record = (get_vm_metadata_by_name t vm_name) in
  (* reuse existing record if possible *)
  let record = match existing_record with
    | None -> { how_to_stop = stop_mode;
                vm_name;
                vm_kernel=kernel;
                nics;
                vif_hotplug_scripts;
                memory_kb;
                boot_options = boot_options;
                vm_ttl = ttl * 2; (* note *2 here *)
                query_response_delay = response_delay;
                started_ts = 0;
                requested_ts = 0;
                total_requests = 0;
                total_starts = 0;
                wait;  }
    | Some existing_record -> existing_record
  in
  (* add/replace in both hash tables *)
  Hashtbl.replace t.domain_table domain_as_list record;
  Hashtbl.replace t.name_table vm_name record;
  return_unit

let stop_vm_by_name t name =
  match get_vm_metadata_by_name t name with 
  | None -> t.log (Printf.sprintf "VM %s not found. Not stopping..." name); return_unit
  | Some meta -> (stop_vm t meta)

let start_vm_by_name t name =
  match get_vm_metadata_by_name t name with 
  | None -> t.log (Printf.sprintf "VM %s not found. Not starting..." name); return_unit
  | Some meta -> (start_vm t meta)

(* iterate through t.name_table and stop VMs that haven't received
   requests for more than ttl*2 seconds *)
let stop_expired_vms t =
  let expired_vms = Array.make (Hashtbl.length t.name_table) None in
  (* TODO this should be run in lwt, but hopefully it is reasonably fast this way. *)
  let current_time = int_of_float (Unix.time ()) in
  let is_expired vm_meta =
    current_time - vm_meta.requested_ts > vm_meta.vm_ttl
  in
  let pos = ref (-1) in
  let put_in_array _ vm_meta =
    incr pos;
    match is_expired vm_meta with
    | true  -> expired_vms.(!pos) <- Some vm_meta
    | false -> expired_vms.(!pos) <- None
  in
  Hashtbl.iter put_in_array t.name_table;
  let rec loop i =
    if i >= (Array.length expired_vms)
    then return_unit
    else match expired_vms.(i) with
      | None -> loop (i + 1)
      | Some vm ->
        stop_vm t vm
        >>= fun () ->
        loop (i + 1) in
  loop 0
