open Sexplib.Std
open Printf

type 'a point = { x : float; y : float; label : string; content : 'a }
[@@deriving sexp]

(* This is from chapter 3 *)
let string_of_point p =
  p.label
  ^ " = ("
  ^ string_of_float p.x
  ^ ", "
  ^ string_of_float p.y
  ^ ")"
[@@ocamlformat "disable"]

let string_of_point' p = sprintf "%s = (%f, %f)" p.label p.x p.y

let%expect_test _ =
  let print content =
    let open Core in
    print_s [%sexp (content : char list point)]
  in
  let p = { x = 0.1; y = 0.2; label = "hello"; content = [ 'A'; 'B'; 'C' ] } in
  ()
  ; print p
  ; [%expect {| ((x 0.1) (y 0.2) (label hello) (content (A B C))) |}]
  ; ()
  ; print_string @@ string_of_point p
  ; [%expect {| hello = (0.1, 0.2) |}]
  ; print_string @@ string_of_point' p
  ; [%expect {| hello = (0.100000, 0.200000) |}]
;;

(* We can partially apply printf *)

module Partial_sprintf = struct
  let string_of_cust = sprintf "Custom value: (%d, %d)"
  let%test _ = string_of_cust 1 2 = "Custom value: (1, 2)"
  let one = string_of_cust 1
  let%test _ = one 2 = "Custom value: (1, 2)"
  let%test _ = one 3 = "Custom value: (1, 3)"
end

(*

The format of a conversion specifier is in fact:

%(flags)(width)<.precision>type

Fields enclosed in brackets are optional. We have only used `type` so far.

*)

module Tabular_data = struct
  let data = [ (1, 6, 5); (2, 18, 4); (3, 31, 12); (4, 16, 2) ]

  let print_header () =
    ()
    ; printf "A     | B     | C     \n"
    ; printf "------+-------+-------\n"
  ;;

  let print_line (a, b, c) = printf "%6i| %6i| %6i\n" a b c

  let print lst =
    ()
    ; print_header ()
    ; List.iter print_line lst
  ;;

  let%expect_test _ =
    ()
    ; print_newline ()
    ; print data
    ; print_newline ()
    ; [%expect
        {|
      A     | B     | C
      ------+-------+-------
           1|      6|      5
           2|     18|      4
           3|     31|     12
           4|     16|      2
        |}]
  ;;
end

(*

Talking about the format of the conversion specifier again:

%(flags)(width)<.precision>type

The possible values for the `flags` field are:

- : left-justify within width (the default is to right-justify)
0 : pad number within width with zeros instead of spaces
+ : prefix positive number with +

The optional `precision` field specifies the number of digits after the decimal point.

*)

module Tabular_data2 = struct
  let data =
    [ (1, 6.123, 5.4321)
    ; (2, 18.245, 4.4443)
    ; (3, 31.456, 12.246)
    ; (4, 16.789, 2.229)
    ]
  ;;

  let print_header () =
    ()
    ; printf "A     | B     | C     \n"
    ; printf "------+-------+-------\n"
  ;;

  let print_line (a, b, c) = printf "%06i| %-6.2f| %-6.2f\n" a b c

  let print lst =
    ()
    ; print_header ()
    ; List.iter print_line lst
  ;;

  let%expect_test _ =
    ()
    ; print_newline ()
    ; print data
    ; print_newline ()
    ; [%expect
        {|
      A     | B     | C
      ------+-------+-------
      000001| 6.12  | 5.43
      000002| 18.25 | 4.44
      000003| 31.46 | 12.25
      000004| 16.79 | 2.23
        |}]
  ;;
end

(*

While we could simply convert data to string with `sprintf`, the Printf module provides
other means of outputting the data, for convenience and efficiency:

- printf  -> writes to stdout
- eprint  -> writes to stderr
- bprintf -> writes to `Buffer.t`
- fprintf -> writes to a given `out_channel`

*)

(*
 * Q1: print a list of pair of integers in a particular way.
 *)

(* My solution *)
let print lst =
  lst
  |> List.map (fun (a, b) -> sprintf "(%d, %d)" a b)
  |> String.concat " --> "
  |> print_string
;;

let%expect_test _ =
  ()
  ; print [ (1, 2); (5, 6); (6, 6); (7, 5) ]
  ; [%expect {| (1, 2) --> (5, 6) --> (6, 6) --> (7, 5) |}]
;;

(* Inspired by the book's solution *)
let print lst =
  let buf = Buffer.create 16 in
  let rec build_buf = function
    | [] -> ()
    | (a, b) :: t ->
        ()
        ; bprintf buf "(%d, %d)" a b
        ; if t <> [] then bprintf buf " --> "
        ; build_buf t
  in
  ()
  ; build_buf lst
  ; Buffer.contents buf |> print_string
;;

let%expect_test _ =
  ()
  ; print [ (1, 2); (5, 6); (6, 6); (7, 5) ]
  ; [%expect {| (1, 2) --> (5, 6) --> (6, 6) --> (7, 5) |}]
;;

(*
 * Q2: Write a function which returns a string in its equivalent hexadecimal representation.
 *     'H' has ASCII code 0x48 so we should print 48
 *)

(* My solution *)
let print_hexa str =
  str |> String.to_seq |> Seq.iter (fun c -> printf "%x" @@ Char.code c)
;;

let%expect_test _ =
  ()
  ; print_hexa "Hello"
  ; [%expect {| 48656c6c6f |}]
;;

(* Inspired by the book's solution *)
let print_hexa str =
  (* Unlike in question 1, this time, we can calculate the buffer's size exactly.
   * We use the width specifier 2 and the 0 flag to make sure the characters with
   * code 0..15 are padded with zero.
   *)
  let buf = Buffer.create (String.length str * 2) in
  ()
  ; str |> String.iter (fun c -> bprintf buf "%02X" @@ Char.code c)
  ; Buffer.contents buf |> print_string
;;

let%expect_test _ =
  ()
  ; print_hexa "Hello"
  ; [%expect {| 48656C6C6F |}]
;;

(*
 * Q3: Why does the following code cause a type error?
 *
 * Adding a type annotation was necessary, we must return a "format", not a string!
 *)

let q3 () =
  let mk_string () : ('a, out_channel, unit) format = "hello world" in
  printf (mk_string ())
;;

(* The book also suggestions we could have just done this: *)

let q3' () =
  let mk_string () = "hello world" in
  printf "%s" (mk_string ())
;;

(*
 * Q4: Use the "star" syntax described in the `Printf` documentation to write a function
 *     which can print a table (? list??) of integers to a given width
 *)

(*
The integer in a width or precision can also be specified as *, in which case an extra
integer argument is taken to specify the corresponding width or precision.
This integer argument precedes immediately the argument to print.
For instance, %.*f prints a float with as many fractional digits as the value of the argument given before the float.
*)

let%expect_test _ =
  let print width lst = lst |> List.iter (printf "(%*d)\n" width) in
  ()
  ; print 5 [ 1; 23; 33241; 0 ]
  ; [%expect {|
    (    1)
    (   23)
    (33241)
    (    0) |}]
  ; ()
  ; print 10 [ 1; 23; 33241; 0 ]
  ; [%expect
      {|
    (         1)
    (        23)
    (     33241)
    (         0) |}]
;;
