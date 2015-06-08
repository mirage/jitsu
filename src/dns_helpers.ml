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
open Dns

(* Add domain SOA record. Called automatically from add_vm if domain
   is not registered in local DB as a SOA record *)
let add_soa dns_db soa_domain ttl =
  Dns.(Loader.add_soa_rr Name.empty Name.empty
         (Int32.of_int (int_of_float (Unix.time())))
         (Int32.of_int ttl)
         (Int32.of_int 3)
         (Int32.of_int (ttl*2))
         (Int32.of_int (ttl*2))
         (Int32.of_int ttl)
         soa_domain
         dns_db)

(* true if a dns record exists locally for [domain] of [_type] *)
let has_local_domain dns_db domain _type =
  let answer = Query.(answer domain _type dns_db.Loader.trie) in
  match answer.Query.rcode with
  | Packet.NoError -> true
  | _ -> false


(** return base of domain. E.g. www.example.org = example.org, a.b.c.d = c.d *)
let get_base_domain domain =
  Name.of_string_list (List.tl (Name.to_string_list domain))

(** fallback to external resolver if local lookup fails *)
let fallback forwarder _class _type _name =
  match forwarder with
  | Some f -> 
    Dns_resolver_unix.resolve f _class _type _name
    >>= fun result ->
    Lwt.return (Some (Dns.Query.answer_of_response result))
  | None -> Lwt.return_none

let create_dns_db t =
  let dns_db = Dns.Loader.new_db () in
  Irmin_backend.get_vm_list t >>= fun vm_list ->
  Lwt_list.iter_s (fun vm_name ->
      Printf.printf "found vm %s" vm_name;
      Irmin_backend.get_ip t ~vm_name >>= fun r ->
      match r with
      | None -> Printf.printf "VM %s has no IP, skipping" vm_name; Lwt.return_unit
      | Some ip ->
        Irmin_backend.get_vm_dns_name_list t ~vm_name >>= fun dns_name_list ->
        Lwt_list.iter_s (fun dns_name ->
            let base_domain = get_base_domain dns_name in
            let answer = has_local_domain dns_db base_domain Packet.Q_SOA in
            Irmin_backend.get_ttl t ~vm_name ~dns_name >>= fun ttl ->
            if not answer then (
              Printf.printf "Adding SOA '%s' with ttl=%d\n" (Name.to_string base_domain) ttl;
              (* add soa if not registered before *) (* TODO use same ttl? *)
              add_soa dns_db base_domain ttl;
            );
            (* add dns record *)
            Printf.printf "Adding A PTR for '%s' with ttl=%d and ip=%s\n" (Name.to_string dns_name) ttl (Ipaddr.V4.to_string ip);
            Loader.add_a_rr ip (Int32.of_int ttl) dns_name dns_db;
            Lwt.return_unit
          ) dns_name_list 
        >>= fun () ->
        Lwt.return_unit
    )
    vm_list >>= fun () ->
  Lwt.return dns_db

