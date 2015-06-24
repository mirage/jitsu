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

module Make (Vm_backend : Backends.VM_BACKEND) = struct
  module Synjitsu = Synjitsu.Make(Vm_backend)

  type t = {
    mutable dns_db : Loader.db;                         (* DNS database *)
    storage : Irmin_backend.t;
    log : string -> unit;                   (* Log function *) 
    vm_backend : Vm_backend.t;                          (* Backend type *)
    forward_resolver : Dns_resolver_unix.t option; (* DNS to forward request to if no
                                                      local match *)
    synjitsu : Synjitsu.t option;
  }

  let create vm_backend log forward_resolver ?use_synjitsu:(use_synjitsu=None) () =
    (* initialise synjitsu *)
    let synjitsu = match use_synjitsu with
      | Some domain -> let t = (Synjitsu.create vm_backend log domain "synjitsu") in
        ignore_result (Synjitsu.connect t);  (* connect in background *)
        Some t
      | None -> None
    in
    Irmin_backend.create ~persist:false ~root:"/tmp/jitsu" () >>= fun storage ->
    Lwt.return { 
      dns_db = Loader.new_db ();
      storage;
      log; 
      vm_backend;
      forward_resolver = forward_resolver;
      synjitsu ;
    }


  let or_vm_backend_error msg fn t =
    fn t >>= function
    | `Error e -> begin
        match e with 
        | `Not_found -> raise (Failure (Printf.sprintf "%s: Not found" msg))
        | `Disconnected -> raise (Failure (Printf.sprintf "%s: Disconnected" msg))
        | `Unknown s -> raise (Failure (Printf.sprintf "%s: %s" msg s))
      end
    | `Ok t -> return t

  let get_vm_name t vm =
    or_vm_backend_error "Unable to get VM name from backend" (Vm_backend.get_name t.vm_backend) vm

  let get_vm_state t vm =
    or_vm_backend_error "Unable to get VM state from backend" (Vm_backend.get_state t.vm_backend) vm

  let stop_vm t vm =
    get_vm_name t vm >>= fun vm_name ->
    get_vm_state t vm >>= fun vm_state ->
    match vm_state with
    | Vm_state.Running ->
      Irmin_backend.get_stop_mode t.storage ~vm_name >>= fun stop_mode ->
      begin match stop_mode with
        | Vm_stop_mode.Unknown -> t.log (Printf.sprintf "Unable to stop VM %s. Unknown stop mode requested\n" vm_name); 
          Lwt.return_unit
        | Vm_stop_mode.Shutdown -> t.log (Printf.sprintf "VM shutdown: %s\n" vm_name);
          or_vm_backend_error "Unable to shutdown VM" (Vm_backend.shutdown_vm t.vm_backend) vm
        | Vm_stop_mode.Suspend  -> t.log (Printf.sprintf "VM suspend: %s\n" vm_name);
          or_vm_backend_error "Unable to suspend VM" (Vm_backend.suspend_vm t.vm_backend) vm
        | Vm_stop_mode.Destroy  -> t.log (Printf.sprintf "VM destroy: %s\n" vm_name) ; 
          or_vm_backend_error "Unable to destroy VM" (Vm_backend.destroy_vm t.vm_backend) vm
      end
    | Vm_state.Off
    | Vm_state.Paused 
    | Vm_state.Suspended
    | Vm_state.Unknown -> Lwt.return_unit (* VM already stopped or nothing we can do... *)

  let start_vm t vm =
    get_vm_name t vm >>= fun vm_name ->
    get_vm_state t vm >>= fun vm_state ->
    t.log (Printf.sprintf "Starting %s (%s)" vm_name (Vm_state.to_string vm_state));
    let update_stats () =
      Irmin_backend.set_start_timestamp t.storage ~vm_name (Unix.time ()) >>= fun () ->
      Irmin_backend.inc_total_starts t.storage ~vm_name
    in
    let notify_synjitsu () =
      Irmin_backend.get_ip t.storage ~vm_name >>= fun r ->
      match r with
      | Some ip -> begin
          or_vm_backend_error "Unable to get MAC for VM" (Vm_backend.get_mac t.vm_backend) vm >>= fun vm_mac ->
          match vm_mac with
          | Some m -> begin
              match t.synjitsu with
              | Some s -> begin
                  t.log (Printf.sprintf "Notifying Synjitsu of MAC %s\n" (Macaddr.to_string m));
                  try_lwt 
                    Synjitsu.send_garp s m ip
                  with e -> 
                    t.log (Printf.sprintf "Got exception %s\n" (Printexc.to_string e)); 
                    Lwt.return_unit
                end
              | None -> Lwt.return_unit
            end
          | None -> Lwt.return_unit
        end
      | None -> t.log (Printf.sprintf "VM %s has no IP. Synjitsu not notified." vm_name); Lwt.return_unit
    in
    match vm_state with
    | Vm_state.Running -> (* Already running, exit *)
      t.log " --! VM is already running\n";
      Lwt.return_unit
    | Vm_state.Suspended ->
      t.log " --> resuming vm...\n";
      or_vm_backend_error "Unable to resume VM" (Vm_backend.resume_vm t.vm_backend) vm >>= fun () ->
      Lwt.async(notify_synjitsu);
      update_stats () >>= fun () ->
      Irmin_backend.get_response_delay t.storage ~vm_name >>= fun delay ->
      Lwt_unix.sleep delay
    | Vm_state.Paused ->
      t.log " --> unpausing vm...\n";
      or_vm_backend_error "Unable to unpause VM" (Vm_backend.unpause_vm t.vm_backend) vm >>= fun () ->
      Lwt.async(notify_synjitsu);
      update_stats () >>= fun () ->
      Irmin_backend.get_response_delay t.storage ~vm_name >>= fun delay ->
      Lwt_unix.sleep delay
    | Vm_state.Off ->
      t.log " --> creating vm...\n";
      or_vm_backend_error "Unable to create VM" (Vm_backend.start_vm t.vm_backend) vm >>= fun () ->
      Lwt.async(notify_synjitsu);
      update_stats () >>= fun () ->
      Irmin_backend.get_response_delay t.storage ~vm_name >>= fun delay ->
      Lwt_unix.sleep delay
    | Vm_state.Unknown ->
      t.log " --! VM cannot be started from this state.\n";
      Lwt.return_unit

  let output_stats t vm_names =
    let current_time = Unix.time () in
    let ip_option_to_string ip_option = 
      match ip_option with
      | None -> "None"
      | Some ip -> Ipaddr.V4.to_string ip
    in
    let ts f =
      match f with
      | None -> "Never"
      | Some f -> (string_of_float ( f -. current_time )) ^ " ago"
    in
    Lwt_list.iter_s (fun vm_name ->
        or_vm_backend_error "Unable to look up VM name" (Vm_backend.lookup_vm_by_name t.vm_backend) vm_name >>= fun vm ->
        t.log (Printf.sprintf "%15s %10s %10s %10s %10s %10s %30s %15s %8s %8s %8s\n" "VM" "state" "delay" "start_time" "tot_starts" "stop_mode" "DNS" "IP" "TTL" "tot_req" "last_req");
        get_vm_state t vm >>= fun vm_state ->
        Irmin_backend.get_ip t.storage ~vm_name >>= fun vm_ip ->
        Irmin_backend.get_response_delay t.storage ~vm_name >>= fun response_delay ->
        Irmin_backend.get_start_timestamp t.storage ~vm_name >>= fun start_ts ->
        Irmin_backend.get_total_starts t.storage ~vm_name >>= fun total_starts ->
        Irmin_backend.get_stop_mode t.storage ~vm_name >>= fun stop_mode ->
        let first_part = (Printf.sprintf "%15s %10s %10f %10s %10d %10s" 
                            vm_name 
                            (Vm_state.to_string vm_state)
                            response_delay
                            (ts start_ts)
                            total_starts
                            (Vm_stop_mode.to_string stop_mode)) 
        in
        (* Get list of DNS domains for this vm_name *)
        Irmin_backend.get_vm_dns_name_list t.storage ~vm_name >>= fun dns_name_list ->
        Lwt_list.iter_s (fun dns_name ->
            Irmin_backend.get_last_request_timestamp t.storage ~vm_name ~dns_name >>= fun last_request_ts ->
            Irmin_backend.get_total_requests t.storage ~vm_name ~dns_name >>= fun total_requests ->
            Irmin_backend.get_ttl t.storage ~vm_name ~dns_name >>= fun ttl ->
            t.log (first_part ^ (Printf.sprintf " %30s %15s %8d %8d %7s\n" 
                                   (Dns.Name.to_string dns_name)
                                   (ip_option_to_string vm_ip)
                                   ttl
                                   total_requests
                                   (ts last_request_ts)));
            Lwt.return_unit
          ) dns_name_list >>= fun () ->
        if dns_name_list = [] then
          t.log "\n";
        Lwt.return_unit) vm_names

  (* add vm to be monitored by jitsu *)
  let add_vm t ~vm_name ~vm_ip ~vm_stop_mode ~dns_names ~dns_ttl ~response_delay =
    (* check if vm_name exists and set up VM record *)

    or_vm_backend_error "Unable to lookup VM by name" (Vm_backend.lookup_vm_by_name t.vm_backend) vm_name >>= fun vm_dom ->
    or_vm_backend_error "Unable to get MAC for VM" (Vm_backend.get_mac t.vm_backend) vm_dom >>= fun vm_mac ->

    Irmin_backend.add_vm t.storage ~vm_name ~vm_mac ~vm_ip ~vm_stop_mode ~response_delay >>= fun () ->
    Lwt_list.iter_s (fun dns_name ->
        Irmin_backend.add_vm_dns t.storage ~vm_name ~dns_name ~dns_ttl
      ) dns_names


  (* iterate through t.name_table and stop VMs that haven't received
     requests for more than ttl*2 seconds *)
  let stop_expired_vms t =
    Irmin_backend.get_vm_list t.storage >>= fun vm_name_list ->
    (* Check for expired names *)
    Lwt_list.filter_map_s (fun vm_name ->
        or_vm_backend_error "Unable to look up VM name" (Vm_backend.lookup_vm_by_name t.vm_backend) vm_name >>= fun vm ->
        get_vm_state t vm >>= fun vm_state ->
        match vm_state with
        | Vm_state.Off 
        | Vm_state.Paused 
        | Vm_state.Suspended
        | Vm_state.Unknown -> Lwt.return_none (* VM already stopped/paused/crashed.. *)
        | Vm_state.Running -> 
          (* Get list of DNS domains that have been requested (has requested timestamp != None) and has NOT expired (timestamp is younger than ttl*2) *)
          Irmin_backend.get_vm_dns_name_list t.storage ~vm_name >>= fun dns_name_list ->
          Lwt_list.filter_map_s (fun dns_name ->
              Irmin_backend.get_last_request_timestamp t.storage ~vm_name ~dns_name >>= fun r ->
              match r with
              | None -> Lwt.return_none (* name not requested, can't expire *)
              | Some last_request_ts ->
                let current_time = Unix.time () in
                Irmin_backend.get_ttl t.storage ~vm_name ~dns_name >>= fun ttl ->
                if (current_time -. last_request_ts) <= (float_of_int (ttl * 2)) then
                  Lwt.return (Some dns_name)
                else
                  Lwt.return_none
            ) dns_name_list 
          >>= fun unexpired_dns_names -> 
          if (List.length unexpired_dns_names) > 0 then (* If VM has unexpired DNS domains, DON'T terminate *)
            Lwt.return_none
          else
            Lwt.return (Some vm) (* VM has no unexpired DNS domains, can be terminated *)
      ) vm_name_list >>= fun expired_vms ->
    Lwt_list.iter_s (stop_vm t) expired_vms (* Stop expired VMs *)

  (** Process function for ocaml-dns. Starts new VMs from DNS queries or
      forwards request to a fallback resolver *)
  let process t ~src:_ ~dst:_ packet =
    Dns_helpers.create_dns_db t.storage >>= fun dns_db ->
    let open Packet in
    match packet.questions with
    | [] -> return_none;
    | [q] -> begin
        let answer = Query.(answer q.q_name q.q_type dns_db.Loader.trie) in
        match answer.Query.rcode with
        | Packet.NoError ->
          t.log (Printf.sprintf "Local match for domain %s\n"
                   (Name.to_string q.q_name));
          (* look for vms in irmin that have the dns domain registered *)
          Irmin_backend.get_vm_list t.storage >>= fun vm_list ->
          Lwt_list.filter_map_s (fun vm_name ->
              Irmin_backend.get_vm_dns_name_list t.storage ~vm_name >>= fun dns_name_list ->
              Lwt_list.filter_map_s (fun dns_name ->
                  if dns_name = q.q_name then (* we found a match, update stats and add to list *)
                    Irmin_backend.inc_total_requests t.storage ~vm_name ~dns_name >>= fun () ->
                    Irmin_backend.set_last_request_timestamp t.storage ~vm_name ~dns_name (Unix.time()) >>= fun () ->
                    t.log (Printf.sprintf "Matching VM is %s with DNS name %s\n" vm_name (Dns.Name.to_string dns_name));
                    Lwt.return (Some dns_name)
                  else
                    Lwt.return_none
                ) dns_name_list >>= fun matching_dns_names ->
              if (List.length matching_dns_names) > 0 then
                Lwt.return (Some vm_name)
              else
                Lwt.return_none
            ) vm_list >>= fun matching_vm_names ->
          Lwt_list.filter_map_s (fun vm_name -> (* start VMs and return IPs *)
              Irmin_backend.get_ip t.storage ~vm_name >>= fun r ->
              match r with
              | None -> Lwt.return_none (* no ip, no result to return *)
              | Some ip ->
                or_vm_backend_error "Unable to look up VM name" (Vm_backend.lookup_vm_by_name t.vm_backend) vm_name >>= fun vm ->
                start_vm t vm >>= fun () ->
                output_stats t [vm_name] >>= fun () ->
                Lwt.return (Some ip)
            ) matching_vm_names >>= fun list_of_ips ->
          if (List.length list_of_ips) = 0 then begin
            t.log (Printf.sprintf "No valid match for %s. Forwarding.\n" (Dns.Name.to_string q.q_name));
            Dns_helpers.fallback t.forward_resolver q.q_class q.q_type q.q_name
          end else 
            (* TODO how to return results with multiple IPs - for now just return DNS answer *)
            return (Some answer)
        | _ ->
          t.log (Printf.sprintf "No local match for %s, forwarding...\n"
                   (Name.to_string q.q_name));
          Dns_helpers.fallback t.forward_resolver q.q_class q.q_type q.q_name
      end
    | _ -> return_none

end
