(*
 * Copyright (c) 2015 Citrix Inc <dave.scott@citrix.com>
 * Copyright (c) 2015 Magnus Skjegstad <magnus@skjegstad.com>
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

module Client = Xs_client_lwt.Client(Xs_transport_lwt_unix_client)

let debug = false

exception Uuid_error of string
exception Lookup_error of string
exception Invalid_config of string
exception Mac_error of string

type t = {
  context : Xenlight.ctx;
  log_f : (string -> unit);
}

type vm = {
  uuid : Uuidm.t;
}

let bytes_of_int_array a =
  (* TODO: Probably a better way to do this... *)
  let s = String.create (Array.length a) in
  Array.iteri (fun i v ->
      String.set s i (Char.chr v)) a;
  s

let bytes_of_xen_uuid (uuid:(int array)) =
  if ((Array.length uuid) = 15) then
    begin
      (bytes_of_int_array uuid)
    end else
    raise (Uuid_error (Printf.sprintf "wrong length %d (should be 15)" (Array.length uuid)))

let uuidm_of_xen_uuid uuid =
  Uuidm.of_bytes (bytes_of_xen_uuid uuid)

let dominfo_to_vm dominfo =
  let xen_uuid = dominfo.Xenlight.Dominfo.uuid in
  match (uuidm_of_xen_uuid xen_uuid) with
  | None -> raise (Uuid_error "uuidm couldn't parse xen uuid")
  | Some uuid -> { uuid }

let try_call msg f =
  try
    f ()
  with
  | Mac_error msg -> begin
      Lwt.return (`Error (`Unknown (Printf.sprintf "Invalid MAC address: %s" msg)))
    end
  | Invalid_config msg -> begin
      Lwt.return (`Error (`Invalid_config (Printf.sprintf "Invalid config: %s" msg)))
    end
  | Lookup_error msg -> begin
      Lwt.return (`Error (`Unknown (Printf.sprintf "Lookup error: %s" msg)))
    end
  | Uuid_error msg -> begin
      Lwt.return (`Error (`Unknown (Printf.sprintf "Unable to parse UUID from Xen: %s" msg)))
    end
  | Xenlight.Error (e, xenlight_msg) -> begin
      let xenlight_error_s = (Xenlight.string_of_error e) in
      let error = `Unknown (Printf.sprintf "%s (%s): %s" xenlight_msg xenlight_error_s msg) in
      Lwt.return (`Error error)
    end

(* All libxl calls need one of these to say where the logs should go.
   We delay the creation because it can fail (e.g. due to insufficient privileges)
   and we would still like people to be able to consult command-line arguments *)
let context log_f =
  Xenlight.register_exceptions ();
  Printexc.register_printer (function
      | Xenlight.Error(error, msg) ->
        Some (Printf.sprintf "Xenlight.Error(%s, %s)" (Xenlight.string_of_error error) msg)
      | _ ->
        None
    );
  let logger =
    let open Xentoollog in
    let vmessage _level errno ctx msg =
      let errno_str = match errno with None -> "" | Some s -> Printf.sprintf ": errno=%d" s
      and ctx_str = match ctx with None -> "" | Some s -> Printf.sprintf "%s" s in
      log_f (Printf.sprintf "# %s%s: %s\n%!" ctx_str errno_str msg) in
    let progress _ctx what percent dne total =
      let nl = if dne = total then "\n" else "" in
      log_f (Printf.sprintf "\rProgress %s %d%% (%Ld/%Ld)%s" what percent dne total nl) in
    create "Xentoollog.logger" { vmessage; progress } in
  try
    Xenlight.ctx_alloc logger
  with e ->
    log_f (Printf.sprintf "Unable to talk to Xen. Please check that:\n");
    log_f (Printf.sprintf "- Xen is running (try cat /sys/hypervisor/type)\n");
    log_f (Printf.sprintf "- xenfs is mounted\n");
    log_f (Printf.sprintf "- Xenstore is running\n");
    log_f (Printf.sprintf "- you have sufficient privileges\n");
    log_f (Printf.sprintf "\nRaw error was: %s\n%!" (Printexc.to_string e));
    exit 1 (* TODO: Handle this better? *)

let default_log s =
  Printf.fprintf stderr "libxl_backend: %s" s

let connect ?log_f:(log_f=default_log) ?connstr () =
  match connstr with
  | Some _ -> Lwt.return (`Error (`Unknown "Connect string not supported"))
  | None -> try_call "Unable to connect" (fun () ->
      Lwt.return (`Ok { context=(context log_f) ; log_f })
    )

(* Helper functions *)

let lookup_vm_by_predicate t p =
  let domains = List.filter p (Xenlight.Dominfo.list t.context) in
  match domains with
  | [hd] -> Some hd
  | _::_::_ -> raise (Lookup_error "Lookup returned more than one result (internal error)")
  | [] -> None

let get_dominfo_by_uuid t vm_uuid =
  let uuid_check di =
    let uuid = uuidm_of_xen_uuid (di.Xenlight.Dominfo.uuid) in
    match uuid with
    | Some x -> (Uuidm.compare x vm_uuid) = 0
    | None -> false
  in
  lookup_vm_by_predicate t uuid_check

let get_dominfo_by_domid t vm_domid =
  let domid_check di =
    di.Xenlight.Dominfo.domid = vm_domid
  in
  lookup_vm_by_predicate t domid_check

let safe_xs_read_by_domid domid key =
  let path = Printf.sprintf "/local/domain/%d/%s" domid key in
  let safe_read h k =
    Lwt.catch
      (fun () -> Xs.read h k >>= fun v -> Lwt.return (Some v))
      (function Xs_protocol.Enoent _ -> Lwt.return_none | e -> raise e)
  in
  Xs.make () >>= fun xsc ->
  Xs.(immediate xsc (fun h ->
      safe_read h path
    )) >>= function
  | None -> Lwt.return_none
  | Some s -> Lwt.return (Some s)

let safe_xs_read_by_vm t vm key =
  match (get_dominfo_by_uuid t vm.uuid) with
  | None -> Lwt.return_none
  | Some dominfo -> safe_xs_read_by_domid (dominfo.Xenlight.Dominfo.domid) key

let blocking_xenlight f =
  (* Xenlight wants to control SIGCHILD.
   * TODO: can we do better than this? *)
  let old_handler = Sys.signal Sys.sigchld Sys.Signal_default in
  try
    let result = f () in
    Sys.set_signal Sys.sigchld old_handler;
    result
  with e ->
    Sys.set_signal Sys.sigchld old_handler;
    raise e


let domain_config t config =
  let get_option key =
    try
      Some (Hashtbl.find config key)
    with
    | Not_found -> None
  in
  let get_required key =
    try
      Hashtbl.find config key
    with
    | Not_found -> raise (Invalid_config (Printf.sprintf "Missing required config key %s" key))
  in
  let name = get_required "name" in
  let memory_kb = Int64.of_string (get_required "mem") in
  let kernel = get_required "kernel" in
  let cmdline = get_option "cmdline" in
  let scripts = (match (get_option "scripts") with
      | None -> []
      | Some s -> [s]) in (* TODO: Support list options... *)
  let nics = (match (get_option "nics") with
      | None -> []
      | Some s -> [s]) in (* TODO: Support list options *)
  let context = t.context in
  let c_info = Xenlight.Domain_create_info.({ (default context ()) with
                                              Xenlight.Domain_create_info.xl_type = Xenlight.DOMAIN_TYPE_PV;
                                              name = Some name;
                                              run_hotplug_scripts = Some true;
                                            }) in
  let b_info = Xenlight.Domain_build_info.({ (default context ~xl_type:Xenlight.DOMAIN_TYPE_PV ()) with
                                             Xenlight.Domain_build_info.max_memkb = memory_kb;
                                             target_memkb = memory_kb;
                                           }) in
  let b_info_xl_type = match b_info.Xenlight.Domain_build_info.xl_type with
    | Xenlight.Domain_build_info.Pv x -> x
    | _ -> assert false in
  let b_info = Xenlight.Domain_build_info.({ b_info with
                                             xl_type = Pv { b_info_xl_type with
                                                            kernel = Some kernel;
                                                            cmdline = cmdline;
                                                            ramdisk = None;
                                                          };
                                           }) in
  let nics =
    let s = Array.of_list scripts in
    let n = Array.of_list nics in
    Array.init (Array.length n)
      (fun i -> let bridge = Some (Array.get n i) in
        let script = (match s with [||] -> None | scripts -> Some (Array.get scripts (i mod Array.length scripts))) in
        Xenlight.Device_nic.({ (default context ()) with
                               Xenlight.Device_nic.mtu = 1500;
                               script;
                               bridge;
                             })) in
  Xenlight.Domain_config.({ (default context ()) with
                            c_info;
                            b_info;
                            nics;
                          })

(* Module signature implementations *)

let lookup_vm_by_uuid t vm_uuid =
  try_call "Unable lookup VM UUID" (fun () ->
      match (get_dominfo_by_uuid t vm_uuid) with
      | Some dominfo -> Lwt.return (`Ok (dominfo_to_vm dominfo))
      | None -> Lwt.return (`Error `Not_found)
    )

let lookup_vm_by_name t vm_name =
  let domids = List.map (fun di -> di.Xenlight.Dominfo.domid) (Xenlight.Dominfo.list t.context) in
  let rec loop = function
    | domid :: rest -> begin
        safe_xs_read_by_domid domid "name" >>= fun name ->
        match name with
        | None -> loop rest
        | Some name -> if name = vm_name then
            Lwt.return (get_dominfo_by_domid t domid)
          else
            loop rest
      end
    | [] -> Lwt.return_none in
  loop domids >>= fun res ->
  match res with
  | None -> Lwt.return (`Error `Not_found)
  | Some dominfo -> Lwt.return (`Ok (dominfo_to_vm dominfo))

let get_state t vm =
  try_call "Unable to get VM state" (fun () ->
      match (get_dominfo_by_uuid t vm.uuid) with
      | None -> Lwt.return (`Ok Vm_state.Off) (* TODO Check suspended by checking if suspend filename is readable, see also https://github.com/mirage/jitsu/blob/dev/src/jitsu.ml#L150 *)
      | Some _ -> Lwt.return (`Ok Vm_state.Running)
    )

let destroy_vm t vm =
  try_call "Unable to destroy VM" (fun () ->
      match (get_dominfo_by_uuid t vm.uuid) with
      | None -> Lwt.return (`Error `Not_found)
      | Some dominfo -> Xenlight.Domain.destroy t.context dominfo.Xenlight.Dominfo.domid ();
        Lwt.return (`Ok ())
    )

let shutdown_vm t vm =
  try_call "Unable to shutdown VM" (fun () ->
      match (get_dominfo_by_uuid t vm.uuid) with
      | None -> Lwt.return (`Error `Not_found)
      | Some dominfo -> Xenlight.Domain.shutdown t.context dominfo.Xenlight.Dominfo.domid;
        Lwt.return (`Ok ())
    )

let suspend_vm _ _ =
  (* TODO Suspend not supported - code may be adapted from https://github.com/mirage/jitsu/blob/dev/src/jitsu.ml#L201 *)
  Lwt.return (`Error `Not_supported)

let resume_vm _ _ =
  (* TODO Resume not supported - code may be adapted from https://github.com/mirage/jitsu/blob/dev/src/jitsu.ml#L271 *)
  Lwt.return (`Error `Not_supported)

let pause_vm t vm =
  try_call "Unable to pause VM" (fun () ->
      match (get_dominfo_by_uuid t vm.uuid) with
      | None -> Lwt.return (`Error `Not_found)
      | Some dominfo -> Xenlight.Domain.pause t.context dominfo.Xenlight.Dominfo.domid;
        Lwt.return (`Ok ())
    )

let unpause_vm t vm =
  try_call "Unable to unpause VM" (fun () ->
      match (get_dominfo_by_uuid t vm.uuid) with
      | None -> Lwt.return (`Error `Not_found)
      | Some dominfo -> Xenlight.Domain.pause t.context dominfo.Xenlight.Dominfo.domid;
        Lwt.return (`Ok ())
    )

let get_mac t vm =
  try_call "Unable to get MAC addresses for VM" (fun () ->
      match (get_dominfo_by_uuid t vm.uuid) with
      | None -> Lwt.return (`Error `Not_found)
      | Some dominfo -> begin
          let nics = Xenlight.Device_nic.list t.context dominfo.Xenlight.Dominfo.domid in
          let macs = List.map (fun v ->
              let xen_mac = (bytes_of_int_array v.Xenlight.Device_nic.mac) in
              let mac = Macaddr.of_bytes xen_mac in
              match mac with
              | Some m -> m
              | None -> raise (Mac_error (Printf.sprintf "Unable to parse MAC %s" xen_mac))) nics in
          Lwt.return (`Ok macs)
        end
    )

let start_vm t _ config =
  try_call "Unable to start VM" (fun () ->
      blocking_xenlight (fun () ->
          try
            let domid = Xenlight.Domain.create_new t.context (domain_config t config) () in
            Lwt.return (`Ok (Xenlight.Domain.unpause t.context domid))
          with
            e -> Lwt.return (`Error (`Unknown (Printf.sprintf "Create failed with: %s.\n%!" (Printexc.to_string e))))
        )
    )

let get_uuid _ vm =
  Lwt.return (`Ok vm.uuid)

let get_name t vm =
  try_call "Unable to get VM name" (fun () ->
      safe_xs_read_by_vm t vm "name" >>= function
      | Some n -> Lwt.return (`Ok n)
      | None -> Lwt.return (`Error `Not_found)
    )

let get_domain_id t vm =
  try_call "Unable to get VM dom id" (fun () ->
      match (get_dominfo_by_uuid t vm.uuid) with
      | None -> Lwt.return (`Error `Not_found)
      | Some n -> Lwt.return (`Ok (n.Xenlight.Dominfo.domid))
    )

let get_config_option_list =
  [ ("name", "VM name (required)") ;
    ("kernel", "VM kernel file name (required)") ;
    ("mem", "VM memory in kb (required)") ;
    ("cmdline", "Extra parameters passed to kernel (optional)") ;
    ("nics", "Network devices (br0, eth0 etc) (optional, only one supported)") ;
    ("scripts", "VIF script(s) (optional, only one supported)") ]
