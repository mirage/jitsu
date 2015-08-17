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

let hashtbl =
  let t = Hashtbl.create 15 in
  Hashtbl.add t "int" "100";
  Hashtbl.add t "not_int" "xxx";
  Hashtbl.add t "string" "hello world";
  Hashtbl.add t "dns_name" "www.example.org";
  Hashtbl.add t "str_list" "val1";
  Hashtbl.add t "str_list" "val2";
  Hashtbl.add t "str_list" "val3";
  Hashtbl.add t "str_list" "val4";
  Hashtbl.add t "tuple_str_list" "left1";
  Hashtbl.add t "tuple_str_list" "left2:";
  Hashtbl.add t "tuple_str_list" "left3:r";
  Hashtbl.add t "tuple_str_list" "left4:right4";
  Hashtbl.add t "tuple_str_list" ":right5";
  Hashtbl.add t "tuple_str_list" "l:r";
  Hashtbl.add t "tuple_str_list" ":";
  Hashtbl.add t "tuple_str_list" "";
  Hashtbl.add t "bool" "0";
  Hashtbl.add t "bool_list" "true";
  Hashtbl.add t "bool_list" "false";
  Hashtbl.add t "bool_list" "0";
  Hashtbl.add t "bool_list" "1";
  t

let test_get_int () =
  match (Options.get_int hashtbl "int") with
  | `Ok i -> Alcotest.(check int) "int" 100 i
  | `Error e -> Alcotest.fail (Options.string_of_error e)

let test_get_int_str () =
  match (Options.get_int hashtbl "not_int") with
  | `Ok _ -> Alcotest.fail "get_int succeeded for string value"
  | `Error _ -> ()

let test_get_str () =
  match (Options.get_str hashtbl "string") with
  | `Ok s -> Alcotest.(check string) "string" "hello world" s
  | `Error e -> Alcotest.fail (Options.string_of_error e)

let test_get_str_list () =
  let expected = ["val1";"val2";"val3";"val4"] in
  match (Options.get_str_list hashtbl "str_list") with
  | `Ok s -> Alcotest.(check (list string)) "string list" expected s
  | `Error e -> Alcotest.fail (Options.string_of_error e)

let test_get_bool () =
  match (Options.get_bool hashtbl "bool") with
  | `Ok s -> if not s = false then Alcotest.fail "expected false"
  | `Error e -> Alcotest.fail (Options.string_of_error e)

let test_get_bool_list () =
  let expected = [ true ; false ; false ; true ] in
  match (Options.get_bool_list hashtbl "bool_list") with
  | `Ok s -> let _ = List.map2 (fun a b ->
      if not a=b then
        Alcotest.fail (Printf.sprintf "Bool lists are not equal. Expected %B, got %B" a b)
      else
        ()) expected s in
    ()
  | `Error e -> Alcotest.fail (Options.string_of_error e)
  
let test_get_dns_name () =
  match (Options.get_dns_name hashtbl "dns_name") with
  | `Ok i -> Alcotest.(check string) "dns_name" "www.example.org" (Dns.Name.to_string i)
  | `Error e -> Alcotest.fail (Options.string_of_error e)


let test_get_str_tuple_list () =
  let is_eq a b = (* compare string option *)
    match a,b with
    | Some a, Some b -> String.compare a b = 0
    | None, None -> true
    | _ -> false
  in
  let is_eq_tup a b = (* compare (string option * string option) *)
    let a_l,a_r = a in
    let b_l,b_r = b in
    (is_eq a_l b_l) && (is_eq a_r b_r)
  in
  let opt_s a = (* string option to string *)
    match a with
    | Some s -> (Printf.sprintf "Some '%s'" s)
    | None -> "None"
  in
  let t_s t = (* (string option * string option) to string *)
    let left, right = t in
    Printf.sprintf "(%s, %s)" (opt_s left) (opt_s right)
  in
  let expected = [ (Some "left1", None) ;
                   (Some "left2", None) ;
                   (Some "left3", Some "r") ;
                   (Some "left4", Some "right4") ;
                   (None, Some "right5") ;
                   (Some "l", Some "r") ;
                   (None, None) ;
                   (None, None) ] in
  match (Options.get_str_tuple_list hashtbl "tuple_str_list" ~sep:':' ()) with
  | `Ok s -> let _ = List.map2 (fun a b ->
      if not (is_eq_tup a b) then
        Alcotest.fail (Printf.sprintf "Tuple lists are not equal. Expected %s, got %s" (t_s a) (t_s b))
      else
        ()) expected s in
    ()
  | `Error e -> Alcotest.fail (Options.string_of_error e)

(* Run it *)
let test_options =
  ["Test options",
   [ "get_int on int", `Quick, test_get_int ;
     "get_int on string", `Quick, test_get_int_str ;
     "get_str", `Quick, test_get_str ;
     "get_str_list", `Quick, test_get_str_list ;
     "get_bool", `Quick, test_get_bool ;
     "get_bool_list", `Quick, test_get_bool_list ;
     "get_dns_name", `Quick, test_get_dns_name ;
     "get_str_tuple_list", `Quick, test_get_str_tuple_list ;
   ]
  ]
