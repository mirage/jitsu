(*
 * Copyright (c) 2014 Magnus Skjegstad <magnus@skjegstad.com>
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

type t = {
    log : string -> unit;
    backend_conn : Libvirt.rw Libvirt.Connect.t;
    synjitsu_domain : string; (* Synjitsu domain name or uuid *)
    synjitsu_port : string; (* Vchan port name to use *)
    is_connecting : bool ref;
    ic : Lwt_io.input_channel option ref;
    oc : Lwt_io.output_channel option ref;
}

let create backend_conn log synjitsu_domain synjitsu_port =
    {log; 
    backend_conn;
    synjitsu_domain; 
    synjitsu_port; 
    is_connecting = ref false;
    ic = ref None; 
    oc = ref None}

let try_libvirt msg f =
  try f () with
  | Libvirt.Virterror e -> raise (Failure (Printf.sprintf "%s: %s" msg (Libvirt.Virterror.to_string e)))

let disconnect t =
    (match !(t.ic) with
    | None -> Lwt.return_unit
    | Some x -> Lwt_io.abort x >>= fun () -> t.ic := None; Lwt.return_unit) 
    >>= fun () ->
    match !(t.oc) with
    | None -> Lwt.return_unit
    | Some x -> Lwt_io.abort x >>= fun () -> t.oc := None; Lwt.return_unit

let connect t =
    (* TODO clean up exception mess *)
    try_lwt 
        if !(t.is_connecting) = false then
        begin
            t.is_connecting := true;
            (try_lwt (disconnect t) with _ -> Lwt.return_unit) >>= fun () -> (* disconnect just in case, ignore result *)
            Lwt_unix.sleep 1.0 >>= fun () -> (* wait, just in case *)
            t.log "synjitsu: Connecting...\n";
            let domain = (
                try
                    try_libvirt "synjitsu: could not find domain by uuid\n" (fun () -> Libvirt.Domain.lookup_by_uuid t.backend_conn t.synjitsu_domain)
                with _ -> 
                    try_libvirt "synjitsu: could not find domain by name (or uuid)\n" (fun () -> Libvirt.Domain.lookup_by_name t.backend_conn t.synjitsu_domain)
            ) in
            let client = `Vchan_direct (`Domid (Libvirt.Domain.get_id domain), `Port t.synjitsu_port) in
            Conduit_lwt_unix.init () >>= fun ctx ->
            Conduit_lwt_unix.connect ~ctx client >>= fun (_, ic, oc) ->
            t.log "synjitsu: Connected\n";
            t.ic := Some ic;
            t.oc := Some oc;
            t.is_connecting := false; 
            Lwt.return_unit
        end else
            Lwt.return_unit
    with 
        exn -> t.log (Printf.sprintf "synjitsu: Unable to connect: %s\n" (Printexc.to_string exn)); 
        t.is_connecting := false; 
        Lwt.return_unit

let send t buf =
    match !(t.oc) with
    | None -> t.log "synjitsu: Unable to send - message dropped. Trying to connect...\n"; ignore_result (connect t); Lwt.return_unit
    | Some oc -> try_lwt 
                    Lwt_io.write_from_string_exactly oc (Cstruct.to_string buf) 0 (Cstruct.len buf)
                 with 
                    End_of_file -> t.log "synjitsu: disconnected.\n" ; ignore_result (connect t); Lwt.return_unit
    
let send_garp t mac ip =
    (* TODO support ipv6 *)
    let mac_buf = Macaddr.to_bytes mac in
    let ip_buf = Ipaddr.V4.to_bytes ip in
    let buf = Cstruct.of_string (mac_buf ^ ip_buf) in
    send t buf
