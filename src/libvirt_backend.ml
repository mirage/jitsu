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
  log_f : string -> unit;
}

exception Invalid_config of string

let try_libvirt msg f =
  try
    Lwt.return (`Ok (f () ))
  with
  | Invalid_config msg -> Lwt.return (`Error (`Invalid_config msg))
  | Not_found -> Lwt.return (`Error (`Unknown "Uncaught Not_found exception (internal error)"))
  | Libvirt.Virterror e -> Lwt.return (`Error (`Unknown (Printf.sprintf "%s: %s" msg (Libvirt.Virterror.to_string e))))

let default_log s =
  Printf.printf "libvirt_backend: %s\n" s

let connect ?log_f:(log_f=default_log) ?connstr () =
  match connstr with
  | None -> Lwt.return (`Error (`Unable_to_connect "Empty connect string"))
  | Some uri -> 
    let error_msg = 
        Printf.sprintf "Unable to connect to Libvirt backend. Verify that the connect string is correct (%s) and that you have the right permissions." (Uri.to_string uri)
    in
    try_libvirt error_msg (fun () ->
      { connection = Libvirt.Connect.connect ~name:(Uri.to_string uri) () ; log_f }
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

let parse_uuid_exn uuid =
    match (Uuidm.of_string uuid) with
    | None -> raise (Invalid_config (Printf.sprintf "unable to parse UUID %s" uuid))
    | Some uuid -> uuid

let configure_vm t config =
  (* Tries to find the UUID for the VM config
   - Fails if both name and uuid are missing
   - Fails if uuid is missing and unable to lookup name
   - Fails if uuid is specified and unable to lookup uuid *)
  try_libvirt "Unable to configure VM" (fun () ->
      let get p =
          try 
              Some (Hashtbl.find config p)
          with
          | Not_found -> None
      in 
      match (get "uuid") with
      | Some uuid -> begin (* uuid set, parse and check *)
          let uuidm = parse_uuid_exn uuid in
          let domain = Libvirt.Domain.lookup_by_uuid t.connection (Uuidm.to_bytes uuidm) in
          let uuidb = Libvirt.Domain.get_uuid_string domain in 
          parse_uuid_exn uuidb
        end
      | None -> begin (* uuid not set, try to lookup name *)
          match (get "name") with
          | None -> raise (Invalid_config "uuid or name has to be set")
          | Some name -> begin
               let domain = Libvirt.Domain.lookup_by_name t.connection name in
               let uuid = parse_uuid_exn (Libvirt.Domain.get_uuid_string domain) in
               uuid
          end
        end 
  )

let lookup_vm_by_name t name =
  try_libvirt "Unable lookup VM name" (fun () ->
      let domain = Libvirt.Domain.lookup_by_name t.connection name in
      let uuid = Libvirt.Domain.get_uuid_string domain in
      parse_uuid_exn uuid 
    )

let get_state t uuid =
  try_libvirt "Unable to get VM state" (fun () ->
      let domain = Libvirt.Domain.lookup_by_uuid t.connection (Uuidm.to_bytes uuid) in
      let info = Libvirt.Domain.get_info domain in
      libvirt_state_to_vm_state info.Libvirt.Domain.state
    )

let destroy_vm t uuid =
  try_libvirt "Unable to destroy VM" (fun () ->
      let domain = Libvirt.Domain.lookup_by_uuid t.connection (Uuidm.to_bytes uuid) in
      Libvirt.Domain.destroy domain
    )

let shutdown_vm t uuid =
  try_libvirt "Unable to shutdown VM" (fun () ->
      let domain = Libvirt.Domain.lookup_by_uuid t.connection (Uuidm.to_bytes uuid) in
      Libvirt.Domain.shutdown domain
    )

let suspend_vm t uuid =
  try_libvirt "Unable to suspend VM" (fun () ->
      let domain = Libvirt.Domain.lookup_by_uuid t.connection (Uuidm.to_bytes uuid) in
      Libvirt.Domain.suspend domain
    )

let resume_vm t uuid =
  try_libvirt "Unable to resume VM" (fun () ->
      let domain = Libvirt.Domain.lookup_by_uuid t.connection (Uuidm.to_bytes uuid) in
      Libvirt.Domain.resume domain
    )

let unpause_vm t uuid =
  resume_vm t uuid (* suspend/pause is the same in libvirt *)

let pause_vm t uuid =
  suspend_vm t uuid (* suspend/pause is the same in libvirt *)

let start_vm t uuid _ =
  try_libvirt "Unable to start VM" (fun () ->
      let domain = Libvirt.Domain.lookup_by_uuid t.connection (Uuidm.to_bytes uuid) in
      Libvirt.Domain.create domain;
    )

(* get mac address for domain - TODO only supports one interface *)
let get_mac t uuid =
  try_libvirt "Unable to get MAC address for VM" (fun () ->
      let domain = Libvirt.Domain.lookup_by_uuid t.connection (Uuidm.to_bytes uuid) in
      let dom_xml_s = Libvirt.Domain.get_xml_desc domain in
      try
        let (_, dom_xml) = Ezxmlm.from_string dom_xml_s in
        let (mac_attr, _) = Ezxmlm.member "domain" dom_xml |> Ezxmlm.member "devices" |> Ezxmlm.member "interface" |> Ezxmlm.member_with_attr "mac" in
        let mac_s = Ezxmlm.get_attr "address" mac_attr in
        match (Macaddr.of_string mac_s) with
        | None -> []
        | Some mac -> [mac]
      with
      | Not_found -> []
      | Ezxmlm.Tag_not_found _ -> []
    )

let get_name t uuid =
  try_libvirt "Unable to get VM name" (fun () ->
      let domain = Libvirt.Domain.lookup_by_uuid t.connection (Uuidm.to_bytes uuid) in
      Some (Libvirt.Domain.get_name domain)
    )

let get_domain_id t uuid =
  try_libvirt "Unable to get VM dom id" (fun () ->
      let domain = Libvirt.Domain.lookup_by_uuid t.connection (Uuidm.to_bytes uuid) in
      Libvirt.Domain.get_id domain
    )

let get_config_option_list =
  [ ("name", "Name of VM defined in libvirt (ignored if uuid is set)") ;
    ("uuid", "UUID of VM defined in libvirt (required, but optional if name is set)") ;
    ("dns", "DNS name (required)") ;
    ("ip", "IP to return in DNS reply (required)") ]

