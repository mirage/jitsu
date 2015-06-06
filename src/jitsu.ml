(*
 * Copyright (c) 2014-2015 Magnus Skjegstad <magnus@skjegstad.com>
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

module Make (Backend : Backends.VM_BACKEND) = struct
  module Synjitsu = Synjitsu.Make(Backend)

  type vm_metadata = {
    vm : Backend.vm;              (* Backend VM representation *)
    ip : Ipaddr.V4.t;             (* IP addr of this domain *)

    query_response_delay : float; (* in seconds, delay after startup before
                                     sending query response *)
    vm_ttl : int;                 (* TTL in seconds. VM is stopped [vm_ttl]
                                     seconds after [requested_ts] *)
    how_to_stop : Backends.vm_stop_mode;   (* how to stop the VM on timeout *)
    mutable started_ts : int;     (* started timestamp *)
    mutable requested_ts : int;   (* last request timestamp *)
    mutable total_requests : int;
    mutable total_starts : int;
  }

  type t = {
    db : Loader.db;                         (* DNS database *)
    log : string -> unit;                   (* Log function *) 
    backend : Backend.t;                          (* Backend type *)
    forward_resolver : Dns_resolver_unix.t option; (* DNS to forward request to if no
                                                      local match *)
    synjitsu : Synjitsu.t option;
    domain_table : (Name.t, vm_metadata) Hashtbl.t;
    (* vm hash table indexed by domain *)
    name_table : (string, vm_metadata) Hashtbl.t;
    (* vm hash table indexed by vm name *)
  }

  let create backend log forward_resolver ?vm_count:(vm_count=7) ?use_synjitsu:(use_synjitsu=None) () =
    let synjitsu = match use_synjitsu with
      | Some domain -> let t = (Synjitsu.create backend log domain "synjitsu") in
        ignore_result (Synjitsu.connect t);  (* connect in background *)
        Some t
      | None -> None
    in
    { db = Loader.new_db ();
      log = log; 
      backend;
      forward_resolver = forward_resolver;
      synjitsu;
      domain_table = Hashtbl.create ~random:true vm_count;
      name_table = Hashtbl.create ~random:true vm_count }

  (* fallback to external resolver if local lookup fails *)
  let fallback t _class _type _name =
    match t.forward_resolver with
    | Some f -> 
      Dns_resolver_unix.resolve f _class _type _name
      >>= fun result ->
      return (Some (Dns.Query.answer_of_response result))
    | None -> return None

  (* convert vm state to string *)
  let string_of_vm_state = function
    | Backends.VmInfoNoState -> "no state"
    | Backends.VmInfoRunning -> "running"
    | Backends.VmInfoBlocked -> "blocked"
    | Backends.VmInfoPaused -> "paused"
    | Backends.VmInfoShutdown -> "shutdown"
    | Backends.VmInfoShutoff -> "shutoff"
    | Backends.VmInfoCrashed -> "crashed"
    | Backends.VmInfoSuspended -> "suspended"

  let or_backend_error msg fn t =
    fn t >>= function
    | `Error e -> begin
        match e with 
        | `Not_found -> raise (Failure (Printf.sprintf "%s: Not found" msg))
        | `Disconnected -> raise (Failure (Printf.sprintf "%s: Disconnected" msg))
        | `Unknown s -> raise (Failure (Printf.sprintf "%s: %s" msg s))
      end
    | `Ok t -> return t


  let get_vm_name t vm =
    or_backend_error "Unable to get VM name from backend" (Backend.get_name t.backend) vm.vm

  let get_vm_state t vm =
    or_backend_error "Unable to get VM state from backend" (Backend.get_state t.backend) vm.vm

  let stop_vm t vm =
    get_vm_name t vm >>= fun vm_name ->
    get_vm_state t vm >>= fun vm_state ->
    match vm_state with
    | Backends.VmInfoRunning ->
      begin match vm.how_to_stop with
        | Backends.VmStopShutdown -> t.log (Printf.sprintf "VM shutdown: %s\n" vm_name);
          or_backend_error "Unable to shutdown VM" (Backend.shutdown_vm t.backend) vm.vm
        | Backends.VmStopSuspend  -> t.log (Printf.sprintf "VM suspend: %s\n" vm_name);
          or_backend_error "Unable to suspend VM" (Backend.suspend_vm t.backend) vm.vm
        | Backends.VmStopDestroy  -> t.log (Printf.sprintf "VM destroy: %s\n" vm_name) ; 
          or_backend_error "Unable to destroy VM" (Backend.destroy_vm t.backend) vm.vm
      end
    | _ -> Lwt.return_unit (* VM already stopped *)

  let start_vm t vm =
    get_vm_name t vm >>= fun vm_name ->
    get_vm_state t vm >>= fun vm_state ->
    t.log (Printf.sprintf "Starting %s (%s)" vm_name (string_of_vm_state vm_state));
    match vm_state with
    | Backends.VmInfoRunning -> (* Already running, exit *)
      t.log " --! VM is already running\n";
      Lwt.return_unit
    | Backends.VmInfoPaused 
    | Backends.VmInfoShutdown 
    | Backends.VmInfoShutoff ->
      (* Try to start VM *)
      begin
        match vm_state with
        | Backends.VmInfoPaused ->
          t.log " --> resuming vm...\n";
          or_backend_error "Unable to resume VM" (Backend.resume_vm t.backend) vm.vm 
        | _ ->
          t.log " --> creating vm...\n";
          or_backend_error "Unable to create VM" (Backend.start_vm t.backend) vm.vm
      end >>= fun () ->
      (* Notify Synjitsu *)
      or_backend_error "Unable to get MAC for VM" (Backend.get_mac t.backend) vm.vm >>= fun vm_mac ->
      begin
        match vm_mac with
        | Some m -> begin
            match t.synjitsu with
            | Some s -> begin
                t.log (Printf.sprintf "Notifying Synjitsu of MAC %s\n" (Macaddr.to_string m));
                try_lwt 
                  Synjitsu.send_garp s m vm.ip 
                with e -> 
                  t.log (Printf.sprintf "Got exception %s\n" (Printexc.to_string e)); 
                  Lwt.return_unit
              end
            | None -> Lwt.return_unit
          end
        | None -> Lwt.return_unit
      end >>= fun _ ->
      (* update stats *)
      vm.started_ts <- truncate (Unix.time());
      vm.total_starts <- vm.total_starts + 1;
      (* sleeping a bit *)
      Lwt_unix.sleep vm.query_response_delay
    | Backends.VmInfoSuspended ->
      t.log " --! Unsuspending VM...\n";
      or_backend_error "Unable to resume VM" (Backend.unsuspend_vm t.backend) vm.vm 
    | Backends.VmInfoBlocked 
    | Backends.VmInfoCrashed
    | Backends.VmInfoNoState ->
      t.log " --! VM cannot be started from this state.\n";
      Lwt.return_unit

  let get_vm_metadata_by_domain t domain =
    try Some (Hashtbl.find t.domain_table domain)
    with Not_found -> None

  let get_vm_metadata_by_name t name =
    try Some (Hashtbl.find t.name_table name)
    with Not_found -> None

  let get_stats t vm =
    get_vm_name t vm >>= fun vm_name ->
    get_vm_state t vm >>= fun vm_state ->
    let result_str = 
      Printf.sprintf "VM: %s\n\
                     \ state: %s\n\
                     \ total requests: %d\n\
                     \ total starts: %d\n\
                     \ last start: %d\n\
                     \ last request: %d (%d seconds since started)\n\
                     \ vm ttl: %d\n"
        vm_name (string_of_vm_state vm_state) vm.total_requests vm.total_starts vm.started_ts vm.requested_ts
        (vm.requested_ts - vm.started_ts) vm.vm_ttl
    in
    Lwt.return result_str

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
                   (Name.to_string q.q_name));
          (* look for vm in hash table *)
          let vm = get_vm_metadata_by_domain t q.q_name in
          begin match vm with
            | Some vm -> begin (* there is a match *)
                get_vm_name t vm >>= fun vm_name ->
                t.log (Printf.sprintf "Matching VM is %s\n" vm_name);
                (* update stats *)
                vm.total_requests <- vm.total_requests + 1;
                vm.requested_ts <- int_of_float (Unix.time());
                start_vm t vm >>= fun () ->
                (* print stats *)
                get_stats t vm >>= fun vm_stats_str ->
                t.log vm_stats_str;
                return (Some answer);
              end;
            | None -> (* no match, fall back to resolver *)
              t.log "No known VM. Forwarding to next resolver...\n";
              fallback t q.q_class q.q_type q.q_name
          end
        | _ ->
          t.log (Printf.sprintf "No local match for %s, forwarding...\n"
                   (Name.to_string q.q_name));
          fallback t q.q_class q.q_type q.q_name
      end
    | _ -> return_none

  (* Add domain SOA record. Called automatically from add_vm if domain
     is not registered in local DB as a SOA record *)
  let add_soa t soa_domain ttl =
    Loader.add_soa_rr Name.empty Name.empty
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
    Name.of_string_list (List.tl (Name.to_string_list domain))

  (* add vm to be monitored by jitsu *)
  let add_vm t ~domain:domain_as_string ~name:vm_name vm_ip stop_mode
      ~delay:response_delay ~ttl =
    (* check if vm_name exists and set up VM record *)

    or_backend_error "Unable to lookup VM by name" (Backend.lookup_vm_by_name t.backend) vm_name >>= fun vm_dom ->
    or_backend_error "Unable to get MAC for VM" (Backend.get_mac t.backend) vm_dom >>= fun vm_mac ->

    (match vm_mac with
     | Some m -> t.log (Printf.sprintf "Domain registered with MAC %s\n" (Macaddr.to_string m))
     | None -> t.log (Printf.sprintf "Warning: MAC not found for domain. Synjitsu will not be notified..\n"));
    (* check if SOA is registered and domain is ok *)
    let domain_t = Name.of_string domain_as_string in
    let base_domain = get_base_domain domain_t in
    let answer = has_local_domain t base_domain Packet.Q_SOA in
    if not answer then (
      t.log (Printf.sprintf "Adding SOA '%s' with ttl=%d\n"
               (Name.to_string base_domain) ttl);
      (* add soa if not registered before *) (* TODO use same ttl? *)
      add_soa t base_domain ttl;
    );
    (* add dns record *)
    t.log (Printf.sprintf "Adding A PTR for '%s' with ttl=%d and ip=%s\n"
             (Name.to_string domain_t) ttl (Ipaddr.V4.to_string vm_ip));
    Loader.add_a_rr vm_ip (Int32.of_int ttl) domain_t t.db;
    let existing_record = (get_vm_metadata_by_name t vm_name) in
    (* reuse existing record if possible *)
    let record = match existing_record with
      | None -> { vm=vm_dom;
                  ip=vm_ip;
                  how_to_stop = stop_mode;
                  vm_ttl = ttl * 2; (* note *2 here *)
                  query_response_delay = response_delay;
                  started_ts = 0;
                  requested_ts = 0;
                  total_requests = 0;
                  total_starts = 0 }
      | Some existing_record -> existing_record
    in
    (* add/replace in both hash tables *)
    Hashtbl.replace t.domain_table domain_t record;
    Hashtbl.replace t.name_table vm_name record;
    return_unit

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
    let stop_vm = function
      | None    -> Lwt.return_unit
      | Some vm -> stop_vm t vm
    in
    Lwt_stream.iter_s stop_vm (Lwt_stream.of_array expired_vms) 

end
