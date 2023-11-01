(* Based on the book's solution (the overall problem being slightly more complex) *)

(*
 * >> "The same case holds a (length, value) pair"
 *
 * This is so simple in comparaison to what I've come up with!
 *)
let get_same x =
  let rec aux x n = function
    | h :: t when h = x -> aux x (n + 1) t
    | rest -> (n, rest)
  in
  aux x 0
;;

let%test_unit _ =
  let ( => ) =
    let open Base in
    [%test_eq: int * int list]
  in
  ()
  ; get_same 1 [ 5; 4; 3; 2; 1 ] => (0, [ 5; 4; 3; 2; 1 ])
  ; get_same 9 [ 5; 9; 3; 2; 1 ] => (0, [ 5; 9; 3; 2; 1 ])
  ; get_same 9 [ 9; 9; 3; 4; 5 ] => (2, [ 3; 4; 5 ])
;;

let get_different lst =
  let rec aux (acc_h, acc_t) = function
    | [] -> (List.rev (acc_h :: acc_t), [])
    | h :: t as rest ->
        if h <> acc_h then
          aux (h, acc_h :: acc_t) t
        else
          (List.rev acc_t, acc_h :: rest)
  in
  match lst with
  | [] -> ([], [])
  | h :: t -> aux (h, []) t
;;

let%test_unit _ =
  let ( => ) =
    let open Base in
    [%test_eq: int list * int list]
  in
  ()
  ; get_different [ 1; 2; 3; 4; 5 ] => ([ 1; 2; 3; 4; 5 ], [])
  ; get_different [ 1; 2; 4; 4; 5 ] => ([ 1; 2 ], [ 4; 4; 5 ])
  ; get_different [] => ([], [])
  ; get_different [ 1 ] => ([ 1 ], [])
  ; get_different [ 1; 2 ] => ([ 1; 2 ], [])
  ; get_different [ 1; 2; 2 ] => ([ 1 ], [ 2; 2 ])
  ; get_different [ 1; 1 ] => ([], [ 1; 1 ])
;;

open Sexplib.Std

let compare_int = Base.compare_int
let compare_list = Base.compare_list

type run = Same of int * int | Diff of int list [@@deriving sexp, compare]

let run_for lst =
  match lst with
  | [] -> raise (Invalid_argument "run_for")
  | h :: _ -> (
      match get_same h lst with
      | 1, _ ->
          let diff, rest = get_different lst in
          (Diff diff, rest)
      | n, rest -> (Same (n, h), rest))
;;

let%test_unit _ =
  let ( => ) =
    let open Base in
    [%test_eq: run * int list]
  in
  ()
  ; run_for [ 1; 2; 3; 4; 4 ] => (Diff [ 1; 2; 3 ], [ 4; 4 ])
  ; run_for [ 1; 1; 1; 2; 2 ] => (Same (3, 1), [ 2; 2 ])
  ; run_for [ 99 ] => (Diff [ 99 ], [])
;;

let consume lst =
  let rec aux acc lst =
    let x, remaining = run_for lst in
    let acc = x :: acc in
    if remaining = [] then
      acc
    else
      aux acc remaining
  in
  aux [] lst |> List.rev
;;

let%test_unit _ =
  let ( => ) =
    let open Base in
    [%test_eq: run list]
  in
  ()
  ; consume [ 1; 2; 3; 4; 4 ] => [ Diff [ 1; 2; 3 ]; Same (2, 4) ]
  ; consume [ 1; 2; 3; 3; 4 ] => [ Diff [ 1; 2 ]; Same (2, 3); Diff [ 4 ] ]
  ; consume [ 1; 2; 3; 3; 3 ] => [ Diff [ 1; 2 ]; Same (3, 3) ]
  ; consume [ 1; 3; 3; 3; 3 ] => [ Diff [ 1 ]; Same (4, 3) ]
  ; consume [ 3; 3; 3; 3; 3 ] => [ Same (5, 3) ]
  ; consume [ 3; 3; 3; 3; 4 ] => [ Same (4, 3); Diff [ 4 ] ]
  ; consume [ 3; 3; 3; 4; 5 ] => [ Same (3, 3); Diff [ 4; 5 ] ]
  ; consume [ 3; 3; 3; 5; 5 ] => [ Same (3, 3); Same (2, 5) ]
;;
