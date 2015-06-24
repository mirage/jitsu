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

type t
type id

val create : ?persist:bool -> ?root:string -> ?log:(string -> unit) -> unit -> t Lwt.t 

val add_vm_dns : t -> vm_name:string -> dns_name:Dns.Name.t -> dns_ttl:int -> unit Lwt.t
val add_vm : t -> vm_name:string -> vm_mac:Macaddr.t option -> vm_ip:Ipaddr.V4.t -> vm_stop_mode:Vm_stop_mode.t -> response_delay:float -> unit Lwt.t

val get_stop_mode : t -> vm_name:string -> Vm_stop_mode.t Lwt.t
val set_stop_mode : t -> vm_name:string -> Vm_stop_mode.t -> unit Lwt.t

val get_start_timestamp : t -> vm_name:string -> float option Lwt.t
val set_start_timestamp : t -> vm_name:string -> float -> unit Lwt.t

val get_last_request_timestamp : t -> vm_name:string -> dns_name:Dns.Name.t -> float option Lwt.t
val set_last_request_timestamp : t -> vm_name:string -> dns_name:Dns.Name.t -> float -> unit Lwt.t

val get_total_starts : t -> vm_name:string -> int Lwt.t 
val inc_total_starts : t -> vm_name:string -> unit Lwt.t 

val get_response_delay : t -> vm_name:string -> float Lwt.t
val set_response_delay : t -> vm_name:string -> float -> unit Lwt.t

val get_total_requests : t -> vm_name:string -> dns_name:Dns.Name.t -> int Lwt.t 
val inc_total_requests : t -> vm_name:string -> dns_name:Dns.Name.t -> unit Lwt.t 

val get_ttl : t -> vm_name:string -> dns_name:Dns.Name.t -> int Lwt.t
val set_ttl : t -> vm_name:string -> dns_name:Dns.Name.t -> int -> unit Lwt.t

val get_ip : t -> vm_name:string -> Ipaddr.V4.t option Lwt.t
val set_ip : t -> vm_name:string -> Ipaddr.V4.t -> unit Lwt.t

val get_vm_list : t -> string list Lwt.t
val get_vm_dns_name_list : t -> vm_name:string -> Dns.Name.t list Lwt.t
(*val get_vm_dns_data : t -> vm_name:Irmin.Path.String_list.step -> dns_data list Lwt.t*)
