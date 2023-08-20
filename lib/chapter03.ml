[@@@warning "-32"]

(* Defining a record *)
type point_simple = { x : float; y : float; label : string }

(* Constructing a record *)
let p = { x = 4.5; y = 6.0; label = "P" }

(*
 * Just like lists and variants, we can parametize this type.
 * So we could have an [int point], a [string point], etc.
 *)
type 'a point = { x : float; y : float; label : string; content : 'a }

let p' = { x = 4.5; y = 6.0; label = "P"; content = [ 1; 3; 1 ] }
let make_point x y label content = { x; y; label; content }

(* We can extract the field with a dot *)
let string_of_point p =
  p.label
  ^ " = ("
  ^ string_of_float p.x
  ^ ", "
  ^ string_of_float p.y
  ^ ")"
[@@ocamlformat "disable"]

(* Alternatively, we can deconstruct the fields in the function's argument *)
let string_of_point { label; x; y; _ } =
  label
  ^ " = ("
  ^ string_of_float x
  ^ ", "
  ^ string_of_float y
  ^ ")"
[@@ocamlformat "disable"]

(* Return a new record, with an updated field *)
let relabel p label = { p with label }
let mirror p = { p with x = p.y; y = p.x }

(* Mutable records *)

type 'a mut_point =
  { x : float
  ; y : float
  ; label : string
  ; mutable content : 'a
  }
[@@ocamlformat "disable"]

(*

  NOTE: an OCaml reference type is just a record with a single mutable field.

  utop # let x = ref 0;;
  val x : int ref = {contents = 0}

  Here, [ref] is defined by:

  type 'a ref = { mutable contents : 'a }

 *)
let x = ref 0 (* Initialize an OCaml ref *)

let p : int list point =
  { x = 4.5; y = 6.0; label = "P"; content = [ 1; 2; 3 ] }
;;

let p' : int list mut_point =
  let init = { x = 4.5; y = 6.0; label = "P"; content = [ 1; 2; 3 ] } in
  (* Overwrite the content *)
  let () = init.content <- [ 4; 5; 6 ] in
  init
;;

(*
 * QUESTIONS
 *)

(*
 * 1. Show how to update a reference without using the `:=` operator
 *)
let%expect_test _ =
  ()
  ; print_int !x
  ; [%expect "0"]
  ; x := 1
  ; print_int !x
  ; [%expect "1"]
  ; let x = { contents = 2 } in
    ()
    ; print_int !x
    ; [%expect "2"]
      (* Book also suggests this possible construct: "there is no reason to use this rather than `:=` of course" *)
    ; x.contents <- 3
    ; print_int !x
    ; [%expect "3"]
;;

(*
 * 2. See: bin/chapter03/what_time_is_it.ml
 *)

(*
 * 3. What is the difference between [type t = { x : int ref }] and [type t = { mutable x : int}]?
 *    What are the advantages/disadvantages of each?
 *
 *    The difference is that using a ref, the x field contains "contents" which is itself mutable.
 *    Whereas with the mutable keyword the x field itself is mutable so this representation more direct
 *
 *    Advantages/disavantages:
 *      - I suppose using the mutable keyword is more efficient.
 *      - Not using mutable keyword forces one to dot into the field,
 *        to then use `!` to get at the mutable data (it's cumbersome)
 *)

(* START: using a ref field *)
type t = { x : int ref }

let a : t = { x = ref 0 }
let a_verbose : t = { x = { contents = 0 } }

let a' =
  ()
  ; print_int a.x.contents
  ; (a.x :=
       let x = a.x in
       !x + 1)
  ; a
;;

(* STOP: using a ref field *)

(* START: using a mutable field  *)
type t' = { mutable x : int }

let b : t' = { x = 0 }

let b' =
  ()
  ; print_int b.x
  ; b.x <- b.x + 1
  ; b
;;

(* STOP: using a mutable field  *)

(*
 * 4. Define a record of six items `a...f` where:
 *    `a` and `b` have the same type as one another,
 *    `c` and `d` have the same type as one another and
 *    `e` and `f` have the same type as one another.
 *
 *)

type ('one, 'two, 'three) my_rec =
  { a : 'one
  ; b : 'one
  ; c : 'two
  ; d : 'two
  ; e : 'three
  ; f : 'three
  }
[@@ocamlformat "disable"]

(*
 * 5. Records are used in the module [Gc] which controls OCaml's garbage collector
 *    Use the data structures and functions in the [Gc] module to write programs which:
 *      a) write a summary of the state of the garbage collector to a text file;and
 *      b) alter the verbosity of the garbage collector as defined in the [control] record.
 *
 *    See: bin/chapter03/gc_summary.ml
 *)
