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

type t = {
  connection : Libvirt.rw Libvirt.Connect.t; (* Libvirt connection *)
}

type vm = {
  uuid : Libvirt.uuid; (* always lookup by uuid to avoid stale identifiers *)
  (*domain : Libvirt.rw Libvirt.Domain.t;  (* Libvirt VM *)*)
}

let try_libvirt msg f =
  try 
    Lwt.return (`Ok (f () ))
  with
  | Libvirt.Virterror e -> Lwt.return (`Error (`Unknown (Printf.sprintf "%s: %s" msg (Libvirt.Virterror.to_string e))))

let connect connstr = 
  try_libvirt "Unable to connect" (fun () -> 
      { connection = Libvirt.Connect.connect ~name:connstr () }
    )

(* convert vm state to string *)
let libvirt_state_to_vm_state = function
  | Libvirt.Domain.InfoNoState 
  | Libvirt.Domain.InfoBlocked 
  | Libvirt.Domain.InfoCrashed -> Vm_state.Unknown
  | Libvirt.Domain.InfoRunning -> Vm_state.Running
  | Libvirt.Domain.InfoPaused -> Vm_state.Paused
  | Libvirt.Domain.InfoShutdown 
  | Libvirt.Domain.InfoShutoff -> Vm_state.Off

let lookup_vm_by_uuid t vm_uuid =
  (* We use UUID for internal representation, but call lookup anyway to make sure it exists *)
  try_libvirt "Unable lookup VM UUID" (fun () -> 
      let domain = Libvirt.Domain.lookup_by_uuid t.connection vm_uuid in
      let uuid = Libvirt.Domain.get_uuid domain in
      { uuid }
    )

let lookup_vm_by_name t vm_name =
  try_libvirt "Unable lookup VM name" (fun () -> 
      let domain = Libvirt.Domain.lookup_by_name t.connection vm_name in
      let uuid = Libvirt.Domain.get_uuid domain in
      { uuid }
    )

let get_state t vm =
  try_libvirt "Unable to get VM state" (fun () -> 
      let domain = Libvirt.Domain.lookup_by_uuid t.connection vm.uuid in
      let info = Libvirt.Domain.get_info domain in
      libvirt_state_to_vm_state info.Libvirt.Domain.state
    )

let destroy_vm t vm =
  try_libvirt "Unable to destroy VM" (fun () -> 
      let domain = Libvirt.Domain.lookup_by_uuid t.connection vm.uuid in
      Libvirt.Domain.destroy domain
    )

let shutdown_vm t vm =
  try_libvirt "Unable to shutdown VM" (fun () -> 
      let domain = Libvirt.Domain.lookup_by_uuid t.connection vm.uuid in
      Libvirt.Domain.shutdown domain
    )

let suspend_vm t vm =
  try_libvirt "Unable to suspend VM" (fun () -> 
      let domain = Libvirt.Domain.lookup_by_uuid t.connection vm.uuid in
      Libvirt.Domain.suspend domain
    )

let resume_vm t vm =
  try_libvirt "Unable to resume VM" (fun () -> 
      let domain = Libvirt.Domain.lookup_by_uuid t.connection vm.uuid in
      Libvirt.Domain.resume domain
    )

let unpause_vm t vm =
  resume_vm t vm (* suspend/pause is the same in libvirt *)

let pause_vm t vm =
  suspend_vm t vm (* suspend/pause is the same in libvirt *)

let start_vm t vm =
  try_libvirt "Unable to start VM" (fun () -> 
      let domain = Libvirt.Domain.lookup_by_uuid t.connection vm.uuid in
      Libvirt.Domain.create domain
    )

(* get mac address for domain - TODO only supports one interface *)
let get_mac t vm =
  try_libvirt "Unable to get MAC address for VM" (fun () ->
      let domain = Libvirt.Domain.lookup_by_uuid t.connection vm.uuid in
      let dom_xml_s = Libvirt.Domain.get_xml_desc domain in
      try
        let (_, dom_xml) = Ezxmlm.from_string dom_xml_s in
        let (mac_attr, _) = Ezxmlm.member "domain" dom_xml |> Ezxmlm.member "devices" |> Ezxmlm.member "interface" |> Ezxmlm.member_with_attr "mac" in
        let mac_s = Ezxmlm.get_attr "address" mac_attr in
        Macaddr.of_string mac_s
      with
      | Not_found -> None
      | Ezxmlm.Tag_not_found _ -> None
    )

let get_uuid _ vm =
  Lwt.return (`Ok vm.uuid)

let get_name t vm =
  try_libvirt "Unable to get VM name" (fun () ->
      let domain = Libvirt.Domain.lookup_by_uuid t.connection vm.uuid in
      Libvirt.Domain.get_name domain
    )

let get_domain_id t vm =
  try_libvirt "Unable to get VM dom id" (fun () ->
      let domain = Libvirt.Domain.lookup_by_uuid t.connection vm.uuid in
      Libvirt.Domain.get_id domain
    )

