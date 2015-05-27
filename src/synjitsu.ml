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

module Make (Backend : Backends.VM_BACKEND) = struct

    type t = {
        log : string -> unit;
        backend : Backend.t;
        vm_uuid : string; (* Synjitsu domain name or uuid *)
        vchan_port : string; (* Vchan port name to use *)
        is_connecting : bool ref;
        ic : Lwt_io.input_channel option ref;
        oc : Lwt_io.output_channel option ref;
    }

    let or_error msg fn t =
        fn t >>= function
        | `Error e -> begin
            match e with 
            | `Not_found -> raise (Failure (Printf.sprintf "%s: Not found" msg))
            | `Disconnected -> raise (Failure (Printf.sprintf "%s: Disconnected" msg))
            | `Unknown s -> raise (Failure (Printf.sprintf "%s: %s" msg s))
        end
        | `Ok t -> return t

    let create backend log vm_uuid vchan_port =
        {log; 
        backend;
        vm_uuid; 
        vchan_port; 
        is_connecting = ref false;
        ic = ref None; 
        oc = ref None}

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
                or_error "synjitsu: Could not find synjitsu VM by uuid" (Backend.lookup_vm_by_uuid t.backend) t.vm_uuid >>= fun vm_domain ->
                or_error "synjitsu: Unable to find synjitsu VM dom id for vchan connection" (Backend.get_domain_id t.backend) vm_domain >>= fun domid ->
                let client = `Vchan_direct (`Domid domid, `Port t.vchan_port) in
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

end
