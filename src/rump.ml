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

open Lwt.Infix

let configure ~domid ~config =
  let path = Printf.sprintf "/local/domain/%d/rumprun/cfg" domid in
  let safe_write h k v =
    Lwt.catch
      (fun () -> Xs.write h k v >>= fun () -> Lwt.return `Ok)
      (function Xs_protocol.Enoent s -> Lwt.return (`Error s) | e -> raise e)
  in
  Xs.make () >>= fun xsc ->
  Xs.(immediate xsc (fun h ->
      safe_write h path config
    ))

let configure_from_file ~domid ~file =
  Lwt.catch
    (fun () -> let stream = Lwt_io.chars_of_file file in
      Lwt_stream.to_string stream >>= fun config ->
      configure ~domid ~config)
    (function Unix.Unix_error (error, f, s) -> Lwt.return (`Error (Printf.sprintf "%s in %s: %s" (Unix.error_message error) f s)) | e -> raise e)
