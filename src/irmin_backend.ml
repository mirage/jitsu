(*
 * Copyright (c) 2015 Magnus Skjegstad <magnus@skjegstad.com>
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
open Irmin_unix

(* Structure
 * Jitsu
 *      \-vm (vm/dns configuration)
 *          \-[vm_name]
 *              \-dns
 *                  \-[dns_name]
 *                      \-ttl
 *                      (+ other stats)
 *              \-config
 *                  \ ...
 *              \-stop_mode
 *              \-response_delay (+ other stats)
 *              \-ip
 *     \-stats (various dynamic stats)
 *          \-[vm-name]
 *              \-dns
 *          ...
*)

type t = {
  connection : string -> ([ `BC ], Irmin.Contents.String.Path.t, Irmin.Contents.String.t) Irmin.t;
  log : (string -> unit);
  mutable dns_cache : Dns.Loader.db;
  mutable dns_cache_dirty : bool;
}

type id = string

let get_float t path  =
  Irmin.read t path >>= fun r ->
  match r with
  | None -> Lwt.return_none
  | Some s -> Lwt.return (Some (float_of_string s))

let set_float t path f =
  Irmin.update t path (string_of_float f)

let default_log msg =
  Printf.printf "irmin_backend: %s\n" msg

let create ?persist:(persist=true) ?root:(root="irmin/test") ?log:(log=default_log) () =
  let config = Irmin_git.config ~root ~bare:true () in
  let store = match persist with
    | true -> Irmin.basic (module Irmin_git.FS) (module Irmin.Contents.String)
    | false -> Irmin.basic (module Irmin_git.Memory) (module Irmin.Contents.String)
  in
  Irmin.create store config task >>= fun connection ->
  let dns_cache = Dns.Loader.new_db () in (* Start with empty DNS db *)
  let dns_cache_dirty = false in
  Lwt.return { connection ; log ; dns_cache_dirty ; dns_cache }

let add_vm_dns t ~vm_name ~dns_name ~dns_ttl =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ; "dns" ; (Dns.Name.to_string dns_name) ] in
  Irmin.update (it "Registering domain ttl")      (path @ [ "ttl" ]) (string_of_int dns_ttl) >>= fun () ->
  t.dns_cache_dirty <- true;
  Lwt.return_unit

let add_vm t ~vm_name ~vm_mac ~vm_ip ~vm_stop_mode ~response_delay ~vm_config =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ] in
  Irmin.update (it "Registering VM stop mode")       (path @ [ "stop_mode" ]) (Vm_stop_mode.to_string vm_stop_mode) >>= fun () ->
  Irmin.update (it "Registering VM response delay")  (path @ [ "response_delay" ]) (string_of_float response_delay) >>= fun () ->
  Irmin.update (it "Registering VM IP")              (path @ [ "ip" ]) (Ipaddr.V4.to_string vm_ip) >>= fun () ->
  let path = path @ [ "config" ] in
  let config = Hashtbl.fold (fun k v l ->
      l @ [(k,v)]) vm_config [] in (* fold hashtbl to list *)
  Lwt_list.iter_s (fun (k,v) ->  (* add config to irmin db *)
      Irmin.update (it (Printf.sprintf "Registering extra config value %s" k)) (path @ [ k ]) v) config >>= fun () ->
  t.dns_cache_dirty <- true;
  let macs = List.map (fun v -> Macaddr.to_string v) vm_mac in
  (match macs with
   | [] -> Lwt.return_unit
   | _ :: _ -> Irmin.update (it "Registering MAC address") (path @ [ "mac" ]) (String.concat "," macs) )

let get_stop_mode t ~vm_name =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ] in
  Irmin.read (it "Get stop mode") (path @ [ "stop_mode" ]) >>= fun r ->
  match r with
  | None -> Lwt.return Vm_stop_mode.Unknown
  | Some s -> Lwt.return (Vm_stop_mode.of_string s)

let set_stop_mode t ~vm_name stop_mode =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ] in
  Irmin.update (it "Set stop mode") (path @ [ "stop_mode" ]) (Vm_stop_mode.to_string stop_mode)

let get_ip t ~vm_name =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ] in
  Irmin.read (it "Get VM IP") (path @ [ "ip" ]) >>= fun r ->
  match r with
  | None -> Lwt.return_none
  | Some s -> Lwt.return (Ipaddr.V4.of_string s)

let set_ip t ~vm_name ip =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ] in
  Irmin.update (it "Set VM IP") (path @ [ "ip" ]) (Ipaddr.V4.to_string ip) >>= fun () ->
  t.dns_cache_dirty <- true;
  Lwt.return_unit

let get_last_request_timestamp t ~vm_name ~dns_name =
  let it = t.connection in
  let path = [ "jitsu" ; "stats" ; vm_name ; "dns" ; (Dns.Name.to_string dns_name)] in
  get_float (it "Get last request timestamp") (path @ [ "last_request_ts" ])

let set_last_request_timestamp t ~vm_name ~dns_name last_request_ts =
  let it = t.connection in
  let path = [ "jitsu" ; "stats" ; vm_name ; "dns" ; (Dns.Name.to_string dns_name)] in
  set_float (it "Set last request timestamp") (path @ [ "last_request_ts" ]) last_request_ts

let get_start_timestamp t ~vm_name =
  let it = t.connection in
  let path = [ "jitsu" ; "stats" ; vm_name ] in
  get_float (it "Get start timestamp") (path @ [ "start_ts" ])

let set_start_timestamp t ~vm_name start_ts =
  let it = t.connection in
  let path = [ "jitsu" ; "stats" ; vm_name ] in
  set_float (it "Set start timestamp") (path @ [ "start_ts" ]) start_ts

let get_total_starts t ~vm_name =
  let it = t.connection in
  let path = [ "jitsu" ; "stats" ; vm_name ] in
  Irmin.read (it "Get total starts") (path @ [ "total_starts" ]) >>= fun r ->
  match r with
  | None -> Lwt.return 0
  | Some s -> Lwt.return (int_of_string s)

let inc_total_starts t ~vm_name =
  (* TODO Should use transaction / view *)
  let it = t.connection in
  let path = [ "jitsu" ; "stats" ; vm_name ] in
  get_total_starts t ~vm_name >>= fun starts ->
  Irmin.update (it "Increase total starts") (path @ [ "total_starts" ]) (string_of_int (starts + 1))

let get_total_requests t ~vm_name ~dns_name =
  let it = t.connection in
  let path = [ "jitsu" ; "stats" ; vm_name ; "dns" ; (Dns.Name.to_string dns_name)] in
  Irmin.read (it "Get total requests") (path @ [ "total_requests" ]) >>= fun r ->
  match r with
  | None -> Lwt.return 0
  | Some s -> Lwt.return (int_of_string s)

let inc_total_requests t ~vm_name ~dns_name =
  (* TODO Should use transaction / view *)
  let it = t.connection in
  let path = [ "jitsu" ; "stats" ; vm_name ; "dns" ; (Dns.Name.to_string dns_name) ] in
  get_total_requests t ~vm_name ~dns_name >>= fun total_requests ->
  Irmin.update (it "Increase total requests") (path @ [ "total_requests" ]) (string_of_int (total_requests + 1))

let get_ttl t ~vm_name ~dns_name =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ; "dns" ; (Dns.Name.to_string dns_name) ] in
  Irmin.read (it "Get DNS TTL") (path @ [ "ttl" ]) >>= fun r ->
  match r with
  | None -> Lwt.return 0
  | Some s -> Lwt.return (int_of_string s)

let set_ttl t ~vm_name ~dns_name ttl =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ; "dns" ; (Dns.Name.to_string dns_name) ] in
  Irmin.update (it "Set DNS TTL") (path @ [ "ttl" ]) (string_of_int ttl) >>= fun () ->
  t.dns_cache_dirty <- true;
  Lwt.return_unit

let get_response_delay t ~vm_name =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ; "response_delay" ] in
  get_float (it "Get VM response delay") path >>= fun d ->
  match d with
  | None -> Lwt.return 0.0
  | Some f -> Lwt.return f

let set_response_delay t ~vm_name response_delay =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ; "response_delay" ] in
  set_float (it "Set VM response delay") path response_delay

(** Get a list of sub-key names as strings from an Irmin path *)
let get_key_names t path =
  let it = t.connection in
  Irmin.list (it "Retrieving key list") path >>= fun key_list ->
  Lwt_list.filter_map_s (fun v ->
      match (Irmin.Path.String_list.rdecons v) with
      | None -> Lwt.return None
      | Some (_,key) -> Lwt.return (Some key)
    ) key_list

let get_vm_config t ~vm_name =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ; "config" ] in
  get_key_names t path >>= fun config_keys ->
  let h = Hashtbl.create (List.length config_keys) in
  Lwt_list.iter_s (fun k ->
      Irmin.read (it (Printf.sprintf "Read config value %s" k)) (path @ [ k ]) >>= fun r ->
      match r with
      | None -> Lwt.return_unit
      | Some s -> Lwt.return (Hashtbl.replace h k s)) config_keys >>= fun () ->
  Lwt.return h

let get_vm_list t =
  let path = [ "jitsu" ; "vm" ] in
  get_key_names t path

let get_vm_dns_name_list t ~vm_name =
  let path = [ "jitsu" ; "vm" ; vm_name ; "dns" ] in
  get_key_names t path >>= fun dns_names ->
  Lwt_list.map_s (fun name ->
      Lwt.return (Dns.Name.of_string name)
    ) dns_names

let create_dns_db t =
  let dns_db = Dns.Loader.new_db () in
  get_vm_list t >>= fun vm_list ->
  Lwt_list.iter_s (fun vm_name ->
      t.log (Printf.sprintf "create_dns_db: found vm %s" vm_name);
      get_ip t ~vm_name >>= fun r ->
      match r with
      | None -> t.log (Printf.sprintf "create_dns_db: VM %s has no IP, skipping" vm_name); Lwt.return_unit
      | Some ip ->
        get_vm_dns_name_list t ~vm_name >>= fun dns_name_list ->
        Lwt_list.iter_s (fun dns_name ->
            let base_domain = Dns_helpers.get_base_domain dns_name in
            let answer = Dns_helpers.has_local_domain dns_db base_domain Dns.Packet.Q_SOA in
            get_ttl t ~vm_name ~dns_name >>= fun ttl ->
            if not answer then (
              t.log (Printf.sprintf "Adding SOA '%s' with ttl=%d\n" (Dns.Name.to_string base_domain) ttl);
              (* add soa if not registered before *) (* TODO use same ttl? *)
              Dns_helpers.add_soa dns_db base_domain ttl;
            );
            (* add dns record *)
            t.log (Printf.sprintf "Adding A PTR for '%s' with ttl=%d and ip=%s\n" (Dns.Name.to_string dns_name) ttl (Ipaddr.V4.to_string ip));
            Dns.Loader.add_a_rr ip (Int32.of_int ttl) dns_name dns_db;
            Lwt.return_unit
          ) dns_name_list
        >>= fun () ->
        Lwt.return_unit
    )
    vm_list >>= fun () ->
  Lwt.return dns_db

let rec get_dns_db t =
  match t.dns_cache_dirty with
  | true -> create_dns_db t >>= fun new_db ->
    t.dns_cache <- new_db;
    t.dns_cache_dirty <- false;
    get_dns_db t
  | false -> Lwt.return t.dns_cache
