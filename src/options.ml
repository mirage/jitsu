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

type error = [ `Required_key_not_found of string | `Invalid_format of string | `Invalid_value of string ]
exception Invalid_format of string
exception Invalid_value of string

let string_of_error = function
  | `Required_key_not_found s -> (Printf.sprintf "Required key not found: %s" s)
  | `Invalid_value s -> (Printf.sprintf "Invalid value: %s" s)
  | `Invalid_format s -> (Printf.sprintf "Invalid format: %s" s)

let get config key parse_fn =
  try
    let v = Hashtbl.find config key in
    `Ok (parse_fn v key)
  with
  | Invalid_value s -> (`Error (`Invalid_value s))
  | Invalid_format s -> (`Error (`Invalid_format s))
  | Not_found -> (`Error (`Required_key_not_found key))

let get_list config key parse_fn =
  try
    let lst = List.rev (Hashtbl.find_all config key) in (* find_all returns inserted keys in reverse order, so reverse again *)
    `Ok (List.map (fun v -> parse_fn v key) lst)
  with
  | Invalid_value s -> (`Error (`Invalid_value s))
  | Invalid_format s -> (`Error (`Invalid_format s))
  | Not_found -> (`Error (`Required_key_not_found key))

let get_tuple_list config key ?sep:(sep='@') parsel_fn parser_fn =
  try
    let lst = List.rev (Hashtbl.find_all config key) in (* find_all returns inserted keys in reverse order, so reverse again *)
    `Ok (List.map (fun v ->
        try
          let sep_pos = String.index v sep in (* exception if not found *)
          let left =
            if (sep_pos = 0) then
              None
            else
              let s = String.sub v 0 sep_pos in
              Some (parsel_fn s key)
          in
          let right =
            if (sep_pos+1 = String.length v) then (* right side is empty *)
              None
            else
              let s = (String.sub v (sep_pos+1) ((String.length v) - sep_pos - 1)) in
              Some (parser_fn s key)
          in
          (left, right)
        with
        | Not_found ->
          if (String.length v = 0) then
            (None, None)
          else
            (Some (parsel_fn v key), None)) lst)
  with
  | Invalid_value s -> (`Error (`Invalid_value s))
  | Invalid_format s -> (`Error (`Invalid_format s))
  | Not_found -> (`Error (`Required_key_not_found key))

let optional = function
  | `Ok v -> Some v
  | `Error _ -> None

let int_of_string_exn v key_name =
  try
    let i64 = Int64.of_string v in
    (Int64.to_int i64)
  with
  | Failure _ -> raise (Invalid_format (Printf.sprintf "%s: '%s'. Int expected." key_name v))

let float_of_string_exn v key_name =
  try
    (float_of_string v)
  with
  | Failure _ -> raise (Invalid_format (Printf.sprintf "%s: '%s'. Float expected." key_name v))

let ipaddr_of_string_exn v key_name =
  try
    (Ipaddr.of_string_exn v)
  with
  | Ipaddr.Parse_error (msg, _) -> raise (Invalid_format (Printf.sprintf "%s: %s '%s'. IPv4 or IPv6 address expected." key_name msg v))

let dns_name_of_string_exn v key_name =
  try
    (Dns.Name.of_string v)
  with
  | Dns.Name.BadDomainName _ -> raise (Invalid_format (Printf.sprintf "%s: '%s'. Domain name expected." key_name v))

let file_name_of_string_exn v key_name =
  let exists = Sys.file_exists v in
  let dir = Sys.is_directory v in
  match exists, dir with
  | true, false -> v
  | true, true -> raise (Invalid_value (Printf.sprintf "%s: '%s'. Kernel must be a file, not a directory." key_name v))
  | false, _ -> raise (Invalid_value (Printf.sprintf "%s: '%s'. File does not exist." key_name v))

let bool_of_string_exn v key_name =
  let f = [ "false" ; "0" ] in
  let t = [ "true" ; "1" ] in
  let v_lower = String.lowercase v in
  if (List.mem v_lower t) then true else
  if (List.mem v_lower f) then false else
    raise (Invalid_format (Printf.sprintf "%s: '%s'. Invalid boolean value. Only true/false or 1/0 accepted." key_name v))

let get_str config key =
  get config key (fun s _ -> s)

let get_str_list config key =
  get_list config key (fun s _ -> s)

let get_str_tuple_list config key ?sep () =
  get_tuple_list config key ?sep (fun s _ -> s) (fun s _ -> s)

let get_ipaddr config key =
  get config key ipaddr_of_string_exn

let get_ipaddr_list config key =
  get_list config key ipaddr_of_string_exn

let get_int config key =
  get config key int_of_string_exn

let get_int_list config key =
  get_list config key int_of_string_exn

let get_float config key =
  get config key float_of_string_exn

let get_float_list config key =
  get_list config key float_of_string_exn

let get_dns_name config key =
  get config key dns_name_of_string_exn

let get_dns_name_list config key =
  get_list config key dns_name_of_string_exn

let get_file_name config key =
  get config key file_name_of_string_exn

let get_file_name_list config key =
  get_list config key file_name_of_string_exn

let get_bool config key =
  get config key bool_of_string_exn

let get_bool_list config key =
  get_list config key bool_of_string_exn
