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

