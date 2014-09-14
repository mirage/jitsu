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
open Libvirt

type vm_stop_mode = VmStopDestroy | VmStopSuspend | VmStopShutdown

type vm_metadata = {
  vm_name : string; (* Unique name of VM. Matches name in libvirt *)
  domain : rw Libvirt.Domain.t; (* Libvirt data structure for this VM *)
  query_response_delay : float; (* in seconds, delay after startup before sending query response *)
  vm_ttl : int; (* TTL in seconds. VM is stopped [vm_ttl] seconds after [requested_ts] *)
  how_to_stop : vm_stop_mode; (* how to stop the VM on timeout *)
  mutable started_ts : int; (* started timestamp *)
  mutable requested_ts : int; (* last request timestamp *)
  mutable total_requests : int;
  mutable total_starts : int;
}

type t = {
  db : Loader.db; (* DNS database *)
  connection : rw Libvirt.Connect.t; (* connection to libvirt *)
  forward_resolver : Dns_resolver_unix.t; (* DNS to forward request to if no local match *)
  domain_table : (Name.domain_name, vm_metadata) Hashtbl.t; (* vm hash table indexed by domain *)
  name_table : (string, vm_metadata) Hashtbl.t; (* vm hash table indexed by vm name *)
}

let create connstr forward_resolver how_to_stop vm_count =
  return {
    db = Loader.new_db ();
    connection = Libvirt.Connect.connect ~name:connstr ();
    forward_resolver = forward_resolver;
    domain_table = Hashtbl.create ~random:true vm_count;
    name_table = Hashtbl.create ~random:true vm_count}

(* fallback to external resolver if local lookup fails *)
let fallback t _class _type _name =
  Dns_resolver_unix.resolve t.forward_resolver _class _type _name
  >>= fun result ->
  return (Some (Dns.Query.answer_of_response result))

(* convert vm state to string *)
let string_of_vm_state = function
  | Libvirt.Domain.InfoNoState -> "no state"
  | Libvirt.Domain.InfoRunning -> "running"
  | Libvirt.Domain.InfoBlocked -> "blocked"
  | Libvirt.Domain.InfoPaused -> "paused"
  | Libvirt.Domain.InfoShutdown -> "shutdown"
  | Libvirt.Domain.InfoShutoff -> "shutoff"
  | Libvirt.Domain.InfoCrashed -> "crashed"

let get_vm_info vm =
  Libvirt.Domain.get_info vm.domain

let get_vm_state vm =
  let info = get_vm_info vm in
  info.Libvirt.Domain.state

let destroy_vm vm =
  Libvirt.Domain.destroy vm.domain

let shutdown_vm vm =
  Libvirt.Domain.shutdown vm.domain

let suspend_vm vm =
  Libvirt.Domain.suspend vm.domain

let resume_vm vm =
  Libvirt.Domain.resume vm.domain

let create_vm vm =
  Libvirt.Domain.create vm.domain

let stop_vm vm =
  match get_vm_state vm with
  | Libvirt.Domain.InfoRunning ->
    begin match vm.how_to_stop with
      | VmStopShutdown -> printf "VM shutdown: %s\n" vm.vm_name; shutdown_vm vm
      | VmStopSuspend  -> printf "VM suspend: %s\n" vm.vm_name ; suspend_vm vm
      | VmStopDestroy  -> printf "VM destroy: %s\n" vm.vm_name ; destroy_vm vm
    end
  | _ -> ()

let start_vm vm =
  let state = get_vm_state vm in
  printf "Starting %s (%s)" vm.vm_name (string_of_vm_state state);
  match state with
  | Libvirt.Domain.InfoPaused | Libvirt.Domain.InfoShutdown
  | Libvirt.Domain.InfoShutoff ->
    let () = match state with
      | Libvirt.Domain.InfoPaused ->
        printf " --> resuming vm...\n";
        resume_vm vm
      | _ ->
        printf " --> creating vm...\n";
        create_vm vm
    in
    (* update stats *)
    vm.started_ts <- int_of_float (Unix.time());
    vm.total_starts <- vm.total_starts + 1;
    (* sleeping a bit *)
    Lwt_unix.sleep vm.query_response_delay
  | Libvirt.Domain.InfoRunning ->
    printf " --! VM is already running\n";
    return_unit
  | Libvirt.Domain.InfoBlocked | Libvirt.Domain.InfoCrashed
  | Libvirt.Domain.InfoNoState ->
    printf " --! VM cannot be started from this state.\n";
    return_unit

let get_vm_metadata_by_domain t domain =
  try Some (Hashtbl.find t.domain_table domain)
  with Not_found -> None

let get_vm_metadata_by_name t name =
  try Some (Hashtbl.find t.name_table name)
  with Not_found -> None

let print_stats vm =
  Lwt_io.printf "VM: %s\n\
                \ total requests: %d\n\
                \ total starts: %d\n\
                \ last start: %d\n\
                \ last request: %d (%d seconds since started)\n\
                \ vm ttl: %d\n"
    vm.vm_name vm.total_requests vm.total_starts vm.started_ts vm.requested_ts
    (vm.requested_ts - vm.started_ts) vm.vm_ttl

(** Process function for ocaml-dns. Starts new VMs from DNS queries or
    forwards request to a fallback resolver *)
let process t ~src ~dst packet =
  let open Packet in
  match packet.questions with
  | [] -> return_none;
  | [q] -> begin
      let answer = Query.(answer q.q_name q.q_type t.db.Loader.trie) in
      match answer.Query.rcode with
      | Packet.NoError ->
        Lwt_io.printf "Local match for domain %s\n" (Name.domain_name_to_string q.q_name)
        >>= fun () ->
        (* look for vm in hash table *)
        let vm = get_vm_metadata_by_domain t q.q_name in
        begin match vm with
          | Some vm -> begin (* there is a match *)
              printf "Matching VM is %s\n" vm.vm_name;
              (* update stats *)
              vm.total_requests <- vm.total_requests + 1;
              vm.requested_ts <- int_of_float (Unix.time());
              start_vm vm >>= fun () ->
              (* print stats *)
              print_stats vm;
              return (Some answer);
            end;
          | None -> (* no match, fall back to resolver *)
            printf "No known VM. Forwarding to next resolver...\n";
            fallback t q.q_class q.q_type q.q_name
        end
      | _ ->
        printf "No local match for %s, forwarding...\n"
          (Name.domain_name_to_string q.q_name);
        fallback t q.q_class q.q_type q.q_name
    end
  | _ -> return_none

(** Add domain SOA record. Called automatically from add_vm if domain
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
let add_vm t domain_as_string vm_name vm_ip stop_mode response_delay ttl =
  (* check if vm_name exists and set up VM record *)
  let vm_dom = Libvirt.Domain.lookup_by_name t.connection vm_name in
  (* check if SOA is registered and domain is ok *)
  let domain_as_list = Name.string_to_domain_name domain_as_string in
  let base_domain = get_base_domain domain_as_list in
  let answer = has_local_domain t base_domain Packet.Q_SOA in
  if not answer then (
    printf "Adding SOA '%s' with ttl=%d\n"
      (Name.domain_name_to_string base_domain) ttl;
    (* add soa if not registered before *) (* TODO use same ttl? *)
    add_soa t base_domain ttl;
  );
  (* add dns record *)
  printf "Adding A PTR for '%s' with ttl=%d and ip=%s\n"
    (Name.domain_name_to_string domain_as_list) ttl (Ipaddr.V4.to_string vm_ip);
  Loader.add_a_rr vm_ip (Int32.of_int ttl) domain_as_list t.db;
  let existing_record = (get_vm_metadata_by_name t vm_name) in
  (* reuse existing record if possible *)
  let record = match existing_record with
    | None -> { domain = vm_dom;
                how_to_stop = stop_mode;
                vm_name = vm_name;
                vm_ttl = ttl * 2; (* note *2 here *)
                query_response_delay = response_delay;
                started_ts = 0;
                requested_ts = 0;
                total_requests = 0;
                total_starts = 0 }
    | Some existing_record -> existing_record
  in
  (* add/replace in both hash tables *)
  Hashtbl.replace t.domain_table domain_as_list record;
  Hashtbl.replace t.name_table vm_name record;
  return_unit

(* iterate through t.name_table and stop VMs that haven't received
   requests for more than ttl*2 seconds *)
let stop_expired_vms t =
  let expired_vms = Array.make (Hashtbl.length t.name_table) None in
  (* TODO this should be run in lwt, but hopefully it is reasonably fast this way.
     Extract expired first, then stop with lwt *)
  let current_time = int_of_float (Unix.time ()) in
  let is_expired vm_meta =
    current_time - vm_meta.requested_ts > vm_meta.vm_ttl
  in
  let pos = ref (-1) in
  let put_in_array key vm_meta =
    incr pos;
    match is_expired vm_meta with
    | true  -> expired_vms.(!pos) <- Some vm_meta
    | false -> expired_vms.(!pos) <- None
  in
  Hashtbl.iter put_in_array t.name_table;
  let stop_vm = function
    | None    -> return_unit
    | Some vm -> stop_vm vm
  in
  let expired_stream = Lwt_stream.of_array expired_vms in
  Lwt_stream.iter_s stop_vm expired_stream (* TODO could this run in _p ? *)
