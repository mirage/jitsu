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
(** Add DNS record for VM *)

val add_vm : t -> vm_name:string -> vm_mac:Macaddr.t option -> vm_ip:Ipaddr.V4.t -> vm_stop_mode:Vm_stop_mode.t -> response_delay:float -> unit Lwt.t
(** Add a new VM *)

val get_stop_mode : t -> vm_name:string -> Vm_stop_mode.t Lwt.t
(** Get VM stop mode *)

val set_stop_mode : t -> vm_name:string -> Vm_stop_mode.t -> unit Lwt.t
(** Set VM stop mode *)

val get_start_timestamp : t -> vm_name:string -> float option Lwt.t
(** Get VM start timestamp *)

val set_start_timestamp : t -> vm_name:string -> float -> unit Lwt.t
(** Set VM start timestamp *)

val get_last_request_timestamp : t -> vm_name:string -> dns_name:Dns.Name.t -> float option Lwt.t
(** Get timestamp for last DNS request to VM/DNS name pair *)

val set_last_request_timestamp : t -> vm_name:string -> dns_name:Dns.Name.t -> float -> unit Lwt.t
(** Set timestamp for last DNS request to VM/DNS name pair *)

val get_total_starts : t -> vm_name:string -> int Lwt.t 
(** Get total number of times the VM has been started *)

val inc_total_starts : t -> vm_name:string -> unit Lwt.t 
(** Increment the number of times the VM has been started *)

val get_response_delay : t -> vm_name:string -> float Lwt.t
(** Get additional delay to add to the DNS response for a VM *)

val set_response_delay : t -> vm_name:string -> float -> unit Lwt.t
(** Set additional delay to add to DNS response for a VM *)

val get_total_requests : t -> vm_name:string -> dns_name:Dns.Name.t -> int Lwt.t 
(** Get total requests to a VM/DNS name pair *)

val inc_total_requests : t -> vm_name:string -> dns_name:Dns.Name.t -> unit Lwt.t 
(** Increment total requests to a VM/DNS name pair *)

val get_ttl : t -> vm_name:string -> dns_name:Dns.Name.t -> int Lwt.t
(** Get DNS TTL for a VM/DNS name pair *)

val set_ttl : t -> vm_name:string -> dns_name:Dns.Name.t -> int -> unit Lwt.t
(** Set DNS TTL for a VM/DNS name pair *)

val get_ip : t -> vm_name:string -> Ipaddr.V4.t option Lwt.t
(** Get VM IP *)

val set_ip : t -> vm_name:string -> Ipaddr.V4.t -> unit Lwt.t
(** Set VM IP *)

val get_vm_list : t -> string list Lwt.t
(** Get list of registered VMs *)

val get_vm_dns_name_list : t -> vm_name:string -> Dns.Name.t list Lwt.t
(** Get list of DNS names registered for a VM *)
