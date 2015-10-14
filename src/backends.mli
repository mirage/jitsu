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

type error = [ `Not_found | `Disconnected of string | `Unknown of string | `Unable_to_connect of string | `Not_supported | `Invalid_config of string ]
type config = (string, string) Hashtbl.t

module type VM_BACKEND =
sig
  type t

  val connect : ?log_f:(string -> unit) -> ?connstr:Uri.t -> unit -> [ `Ok of t | `Error of error ] Lwt.t
  (** Connect to backend *)

  val configure_vm : t -> config -> [ `Ok of Uuidm.t | `Error of error ] Lwt.t
  (** Read and check VM configuration, return VM uuid *)

  val get_config_option_list : (string * string) list
  (** Get list of supported configuration options *)

  val lookup_vm_by_name : t -> string -> [ `Ok of Uuidm.t | `Error of error ] Lwt.t
  (** Lookup UUID of a VM by name *)

  val get_state : t -> Uuidm.t -> [ `Ok of Vm_state.t | `Error of error ] Lwt.t
  (** Get VM state from [vm] type *)

  val get_name : t -> Uuidm.t -> [ `Ok of string option | `Error of error ] Lwt.t
  (** Get VM name from [vm] type *)

  val get_domain_id : t -> Uuidm.t -> [ `Ok of int | `Error of error ] Lwt.t
  (** Get VM domain ID *)

  val get_mac : t -> Uuidm.t -> [ `Ok of Macaddr.t list | `Error of error ] Lwt.t
  (** Get MAC addresses of this VM *)

  val shutdown_vm : t -> Uuidm.t -> [ `Ok of unit | `Error of error ] Lwt.t
  (** Shutdown VM *)

  val suspend_vm : t -> Uuidm.t -> [ `Ok of unit | `Error of error ] Lwt.t
  (** Suspend VM *)

  val destroy_vm : t -> Uuidm.t -> [ `Ok of unit | `Error of error ] Lwt.t
  (** Destroy VM *)

  val resume_vm : t -> Uuidm.t -> [ `Ok of unit | `Error of error ] Lwt.t
  (** Resume VM *)

  val unpause_vm : t -> Uuidm.t -> [ `Ok of unit | `Error of error ] Lwt.t
  (** Unpause VM *)

  val start_vm : t -> Uuidm.t -> config -> [ `Ok of unit | `Error of error] Lwt.t
  (** Start VM *)

end

module type STORAGE_BACKEND =
sig
  type t
  type id

  val create : ?root:string -> ?log:(string -> unit) -> unit -> t Lwt.t

  val get_dns_db : t -> Dns.Loader.db Lwt.t
  (** Get DNS DB representation for use with ocaml-dns *)

  val add_vm_dns : t -> vm_uuid:Uuidm.t -> dns_name:Dns.Name.t -> dns_ttl:int -> unit Lwt.t
  (** Add DNS record for VM *)

  val add_vm : t -> vm_uuid:Uuidm.t -> vm_ip:Ipaddr.V4.t -> vm_stop_mode:Vm_stop_mode.t -> response_delay:float -> wait_for_key:string option -> use_synjitsu:bool -> vm_config:(string,string) Hashtbl.t -> unit Lwt.t
  (** Add a new VM *)

  val get_vm_config : t -> vm_uuid:Uuidm.t -> (string, string) Hashtbl.t Lwt.t
  (** Additional configuration passed to the VM backend *)

  val get_stop_mode : t -> vm_uuid:Uuidm.t -> Vm_stop_mode.t Lwt.t
  (** Get VM stop mode *)

  val set_stop_mode : t -> vm_uuid:Uuidm.t -> Vm_stop_mode.t -> unit Lwt.t
  (** Set VM stop mode *)

  val get_start_timestamp : t -> vm_uuid:Uuidm.t -> float option Lwt.t
  (** Get VM start timestamp *)

  val set_start_timestamp : t -> vm_uuid:Uuidm.t -> float -> unit Lwt.t
  (** Set VM start timestamp *)

  val get_last_request_timestamp : t -> vm_uuid:Uuidm.t -> dns_name:Dns.Name.t -> float option Lwt.t
  (** Get timestamp for last DNS request to VM/DNS name pair *)

  val set_last_request_timestamp : t -> vm_uuid:Uuidm.t -> dns_name:Dns.Name.t -> float -> unit Lwt.t
  (** Set timestamp for last DNS request to VM/DNS name pair *)

  val get_total_starts : t -> vm_uuid:Uuidm.t -> int Lwt.t
  (** Get total number of times the VM has been started *)

  val inc_total_starts : t -> vm_uuid:Uuidm.t -> unit Lwt.t
  (** Increment the number of times the VM has been started *)

  val get_response_delay : t -> vm_uuid:Uuidm.t -> float Lwt.t
  (** Get additional delay to add to the DNS response for a VM *)

  val set_response_delay : t -> vm_uuid:Uuidm.t -> float -> unit Lwt.t
  (** Set additional delay to add to DNS response for a VM *)

  val get_total_requests : t -> vm_uuid:Uuidm.t -> dns_name:Dns.Name.t -> int Lwt.t
  (** Get total requests to a VM/DNS name pair *)

  val inc_total_requests : t -> vm_uuid:Uuidm.t -> dns_name:Dns.Name.t -> unit Lwt.t
  (** Increment total requests to a VM/DNS name pair *)

  val get_ttl : t -> vm_uuid:Uuidm.t -> dns_name:Dns.Name.t -> int Lwt.t
  (** Get DNS TTL for a VM/DNS name pair *)

  val set_ttl : t -> vm_uuid:Uuidm.t -> dns_name:Dns.Name.t -> int -> unit Lwt.t
  (** Set DNS TTL for a VM/DNS name pair *)

  val get_ip : t -> vm_uuid:Uuidm.t -> Ipaddr.V4.t option Lwt.t
  (** Get VM IP *)

  val set_ip : t -> vm_uuid:Uuidm.t -> Ipaddr.V4.t -> unit Lwt.t
  (** Set VM IP *)

  val get_wait_for_key : t -> vm_uuid:Uuidm.t -> string option Lwt.t
  (** Get key to wait for in Xenstore before returning DNS response *)

  val get_use_synjitsu : t -> vm_uuid:Uuidm.t -> bool Lwt.t
  (** Get synjitsu mode *)

  val get_vm_list : t -> Uuidm.t list Lwt.t
  (** Get list of registered VMs *)

  val get_vm_dns_name_list : t -> vm_uuid:Uuidm.t -> Dns.Name.t list Lwt.t
  (** Get list of DNS names registered for a VM *)

end
