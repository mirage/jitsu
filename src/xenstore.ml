(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

let read path =
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


let read_by_domid domid key =
  let path = Printf.sprintf "/local/domain/%d/%s" domid key in
  read path

(* wait for key/value to appear in xenstore*)
let wait path ?value:(value=None) ?timeout:(timeout=5.0) () =
  let wait_for_key () =
    let safe_read h k =
      Lwt.catch
        (fun () -> Xs.read h k >>= fun v -> Lwt.return (Some v))
        (function Xs_protocol.Enoent _ -> Lwt.return_none | e -> raise e)
    in
    Xs.make () >>= fun xsc ->
    Xs.(wait xsc (fun h ->
        safe_read h path >>= fun r ->
        match r, value with
        | None,_   -> raise Xs_protocol.Eagain
        | Some x, Some v -> if x = v then Lwt.return `Ok else raise Xs_protocol.Eagain
        | Some _, None -> Lwt.return `Ok
      ))
  in
  let wait_for_timeout () =
    Lwt_unix.sleep timeout >>= fun () ->
    Lwt.return `Timeout
  in
  Lwt.pick [
    wait_for_key () ;
    wait_for_timeout () ;
  ]

let wait_by_domid domid key =
  wait (Printf.sprintf "/local/domain/%d/%s" domid key)

