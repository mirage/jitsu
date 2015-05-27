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

type vm_stop_mode = VmStopDestroy | VmStopSuspend | VmStopShutdown
type vm_state = VmInfoRunning | VmInfoPaused | VmInfoShutoff | VmInfoShutdown | VmInfoBlocked | VmInfoCrashed | VmInfoNoState
type uuid = string
type error = [ `Not_found | `Disconnected | `Unknown of string ]

module type VM_BACKEND =
sig
  type t
  type vm

  val lookup_vm_by_uuid : t -> uuid -> [ `Ok of vm | `Error of error ] Lwt.t
  (** Lookup a VM by UUID *)
  val lookup_vm_by_name : t -> string -> [ `Ok of vm | `Error of error ] Lwt.t
  (** Lookup UUID of a VM by name *)

  val get_state : t -> vm -> [ `Ok of vm_state | `Error of error ] Lwt.t
  (** Get VM state from [vm] type *)
  val get_name : t -> vm -> [ `Ok of string | `Error of error ] Lwt.t
  (** Get VM name from [vm] type *)
  val get_uuid : t -> vm -> [ `Ok of string | `Error of error ] Lwt.t
  (** Get VM UUID from [vm] type *)
  val get_domain_id : t -> vm -> [ `Ok of int | `Error of error ] Lwt.t
  (** Get VM domain ID *)

  val get_mac : t -> vm -> [ `Ok of Macaddr.t option | `Error of error ] Lwt.t
  (** Get MAC address of this VM *)

  val shutdown_vm : t -> vm -> [ `Ok of unit | `Error of error ] Lwt.t
  (** Shutdown VM *)
  val suspend_vm : t -> vm -> [ `Ok of unit | `Error of error ] Lwt.t
  (** Suspend VM *)
  val destroy_vm : t -> vm -> [ `Ok of unit | `Error of error ] Lwt.t
  (** Destroy VM *)
  val resume_vm : t -> vm -> [ `Ok of unit | `Error of error ] Lwt.t
  (** Resume VM *)
  val start_vm : t -> vm -> [ `Ok of unit | `Error of error] Lwt.t
  (** Start VM *)

end
