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

(** Just-In-Time Summoning of Unikernels.

    Jitsu is a forwarding DNS server that automatically starts
    unikernel VMs when their domain is requested.  The DNS response is
    sent to the client after the unikernel has started, enabling the
    client to use unmodified software to communicate with unikernels
    that are started on demand. If no DNS requests are received for
    the unikernel within a given timeout period, the VM is
    automatically stopped. *)

module Make :
  functor (Backend : Backends.VM_BACKEND) ->
  sig
    type t
    (** The type of Jitsu states. *)

    val create: Backend.t -> (string -> unit) -> Dns_resolver_unix.t option -> ?synjitsu:(Uuidm.t option) -> unit -> t Lwt.t
    (** [create backend log_function resolver vm_count use_synjitsu] creates a new Jitsu instance,
        where vm_count is the initial size of the hash table and use_synjitsu is the optional
        name or uuid of a synjitsu unikernel. *)

    val process: t -> Dns.Packet.t Dns_server.process
    (** Process function for ocaml-dns. Starts new VMs from DNS queries or
        forwards request to a fallback resolver *)

    val add_vm: t ->
      vm_ip:Ipaddr.V4.t -> vm_stop_mode:Vm_stop_mode.t ->
      dns_names:(Dns.Name.t list) -> dns_ttl:int ->
      response_delay:float ->
      vm_config:(string, string) Hashtbl.t ->
      unit Lwt.t
    (** [add_vm t vm_name vm_stop_mode dns_name dns_ip dns_ttl response_delay vm_config] adds a VM to be
        monitored by jitsu.  FIXME. *)

    val stop_expired_vms: t -> unit Lwt.t
    (** Iterate through the internal VM table and stop VMs that haven't
        received requests for more than [ttl*2] seconds. *)

    val string_of_error: Backends.error -> string
    (** Convert backend error to string *)

  end
