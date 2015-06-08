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
 *      \-vm
 *          \-[vm_name]
 *              \-dns
 *                  \-[dns_name]
 *                      \-ttl
 *                      (+ other stats)
 *              \-stop_mode
 *              \-response_delay (+ other stats)
 *              \-ip
*)

type t = {
  connection : string -> ([ `BC ], Irmin.Contents.String.Path.t, Irmin.Contents.String.t) Irmin.t;
  log : (string -> unit);
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
  let store = match persist with
    | true -> Irmin.basic (module Irmin_git.FS) (module Irmin.Contents.String) 
    | false -> Irmin.basic (module Irmin_git.Memory) (module Irmin.Contents.String)
  in
  let config = Irmin_git.config ~root ~bare:true () in
  Irmin.create store config task >>= fun connection ->
  Lwt.return { connection ; log } 

let add_vm_dns t ~vm_name ~dns_name ~dns_ttl =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ; "dns" ; (Dns.Name.to_string dns_name) ] in
  Irmin.update (it "Registering domain ttl")      (path @ [ "ttl" ]) (string_of_int dns_ttl) 

let add_vm t ~vm_name ~vm_mac ~vm_ip ~vm_stop_mode ~response_delay =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ] in
  Irmin.update (it "Registering VM stop mode")       (path @ [ "stop_mode" ]) (Vm_stop_mode.to_string vm_stop_mode) >>= fun () ->
  Irmin.update (it "Registering VM response delay")  (path @ [ "response_delay" ]) (string_of_float response_delay) >>= fun () ->
  Irmin.update (it "Registering VM IP")              (path @ [ "ip" ]) (Ipaddr.V4.to_string vm_ip) >>= fun () ->
  (match vm_mac with
   | None -> Lwt.return_unit
   | Some mac -> Irmin.update (it "Registering MAC address")     (path @ [ "mac" ]) (Macaddr.to_string mac) )

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
  Irmin.update (it "Set VM IP") (path @ [ "ip" ]) (Ipaddr.V4.to_string ip)

let get_last_request_timestamp t ~vm_name ~dns_name =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ; "dns" ; (Dns.Name.to_string dns_name)] in
  get_float (it "Get last request timestamp") (path @ [ "last_request_ts" ])

let set_last_request_timestamp t ~vm_name ~dns_name last_request_ts = 
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ; "dns" ; (Dns.Name.to_string dns_name)] in
  set_float (it "Set last request timestamp") (path @ [ "last_request_ts" ]) last_request_ts

let get_start_timestamp t ~vm_name =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ] in
  get_float (it "Get start timestamp") (path @ [ "start_ts" ])

let set_start_timestamp t ~vm_name start_ts = 
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ] in
  set_float (it "Set start timestamp") (path @ [ "start_ts" ]) start_ts

let get_total_starts t ~vm_name =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ] in
  Irmin.read (it "Get total starts") (path @ [ "total_starts" ]) >>= fun r ->
  match r with
  | None -> Lwt.return 0
  | Some s -> Lwt.return (int_of_string s)

let inc_total_starts t ~vm_name = 
  (* TODO Should use transaction / view *)
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ] in
  get_total_starts t ~vm_name >>= fun starts ->
  Irmin.update (it "Increase total starts") (path @ [ "total_starts" ]) (string_of_int (starts + 1))

let get_total_requests t ~vm_name ~dns_name =
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ; "dns" ; (Dns.Name.to_string dns_name)] in
  Irmin.read (it "Get total requests") (path @ [ "total_requests" ]) >>= fun r ->
  match r with
  | None -> Lwt.return 0
  | Some s -> Lwt.return (int_of_string s)

let inc_total_requests t ~vm_name ~dns_name = 
  (* TODO Should use transaction / view *)
  let it = t.connection in
  let path = [ "jitsu" ; "vm" ; vm_name ; "dns" ; (Dns.Name.to_string dns_name) ] in
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
  Irmin.update (it "Set DNS TTL") (path @ [ "ttl" ]) (string_of_int ttl)

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

let get_vm_list t =
  let path = [ "jitsu" ; "vm" ] in
  get_key_names t path

let get_vm_dns_name_list t ~vm_name =
  let path = [ "jitsu" ; "vm" ; vm_name ; "dns" ] in
  get_key_names t path >>= fun dns_names ->
  Lwt_list.map_s (fun name ->
      Lwt.return (Dns.Name.of_string name)
    ) dns_names

(*let get_vm_dns_data t ~vm_name =
    let it = t.connection in
    get_vm_dns_name_list t ~vm_name >>= fun dns_name_list ->
    Lwt_list.filter_map_s (fun n ->
        let path = [ "jitsu" ; "vm" ; vm_name ; "dns" ; n ] in
        Irmin.read (it "Read DNS TTL") (path @ [ "ttl" ]) >>= fun v ->
        match v with
        | None -> Lwt.return None
        | Some v -> 
                let ttl = int_of_string v in
                Irmin.read (it "Read DNS IP") (path @ [ "ip" ]) >>= fun v ->
                match v with
                | None -> Lwt.return None
                | Some v -> 
                        let ip = Ipaddr.V4.of_string_exn v in
                        let name = Name.of_string n in
                        Lwt.return (Some { dns_name = name ; ttl; ip } )
    ) dns_name_list
*)
