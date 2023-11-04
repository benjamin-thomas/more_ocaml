(* Labeled arguments *)

module Without_labels = struct
  (* Without labeled arguments, we can easily mix arguments of the same type ([beg] and [len] here). *)
  let fill arr beg len v =
    for x = beg to beg + len - 1 do
      arr.(x) <- v
    done
  ;;

  let filled () =
    let arr = Array.make 100 "x" in
    ()
    ; fill arr 20 40 "y"
    ; arr
  ;;
end

module With_labels = struct
  let fill arr ~beg ~len v =
    for x = beg to beg + len - 1 do
      arr.(x) <- v
    done
  ;;

  (* We can change the labled arguments order *)
  let filled () =
    let arr = Array.make 100 "x" in
    let start = 20 in
    let len = 40 in
    ()
    ; fill arr ~len ~beg:start "y"
    ; arr
  ;;
end

(* We can partial apply functions, in a flexible way. *)
let div ~x ~y = x / y

let%expect_test _ =
  let print lst = lst |> List.map string_of_int |> String.concat "; " |> print_string in
  ()
  ; print @@ (let f x = div ~x ~y:3 in [f 10; f 20; f 30])
  ; [%expect {| 3; 6; 10 |}]
  ; ()
  ; print @@ (let f y = div ~x:30 ~y in [f 3; f 5; f 10])
  ; [%expect {| 10; 6; 3 |}]
[@@ocamlformat "disable"]

(* Optional arguments *)

let rec split lst =
  match lst with
  | [] -> []
  | h :: t -> [ h ] :: split t
;;

let%expect_test _ =
  let print lst =
    let open Core in
    print_s [%sexp (lst : int list list)]
  in
  ()
  ; print @@ split [ 1; 2; 3 ]
  ; [%expect {| ((1) (2) (3)) |}]
;;

let rec take n = function
  | h :: t when n > 0 -> h :: take (n - 1) t
  | _ -> []
;;

let rec drop n = function
  | _ :: t when n > 0 -> drop (n - 1) t
  | rest -> rest
;;

(* Let's say we want to extend this function, to produce sub-lists of a given size. *)
let rec split ~chunksize = function
  | [] -> []
  | rest -> take chunksize rest :: split ~chunksize (drop chunksize rest)
;;

let%expect_test _ =
  let print lst =
    let open Core in
    print_s [%sexp (lst : int list list)]
  in
  ()
  ; print @@ split ~chunksize:3 [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
  ; [%expect {| ((1 2 3) (4 5 6) (7 8 9)) |}]
  ; ()
  ; print @@ split ~chunksize:3 [ 1; 2; 3; 4; 5; 6; 7; 8 ]
  ; [%expect {| ((1 2 3) (4 5 6) (7 8)) |}]
  ; ()
  ; print @@ split ~chunksize:3 [ 1; 2; 3; 4; 5; 6; 7 ]
  ; [%expect {| ((1 2 3) (4 5 6) (7)) |}]
  ; ()
  ; print @@ split ~chunksize:3 [ 1; 2; 3; 4; 5; 6 ]
  ; [%expect {| ((1 2 3) (4 5 6)) |}]
  ; () (* To get the old behavior, we must specify the new arg *)
  ; print @@ split ~chunksize:1 [ 1; 2; 3 ]
  ; [%expect {| ((1) (2) (3)) |}]
;;

let rec split ?(chunksize = 1) = function
  | [] -> []
  | rest -> take chunksize rest :: split ~chunksize (drop chunksize rest)
;;

let%expect_test _ =
  let print lst =
    let open Core in
    print_s [%sexp (lst : int list list)]
  in
  ()
  ; print @@ split ~chunksize:3 [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
  ; [%expect {| ((1 2 3) (4 5 6) (7 8 9)) |}]
  ; ()
  ; print @@ split ~chunksize:3 [ 1; 2; 3; 4; 5; 6; 7; 8 ]
  ; [%expect {| ((1 2 3) (4 5 6) (7 8)) |}]
  ; ()
  ; print @@ split ~chunksize:3 [ 1; 2; 3; 4; 5; 6; 7 ]
  ; [%expect {| ((1 2 3) (4 5 6) (7)) |}]
  ; ()
  ; print @@ split ~chunksize:3 [ 1; 2; 3; 4; 5; 6 ]
  ; [%expect {| ((1 2 3) (4 5 6)) |}]
  ; () (* To get the old behavior, we don't __have to__ specify the new arg *)
  ; print @@ split [ 1; 2; 3 ]
  ; [%expect {| ((1) (2) (3)) |}]
;;

(* If we don't specify a default value, the argument becomes an [Option] *)
let rec split ?chunksize =
  let chunk = Option.value chunksize ~default:1 in
  function
  | [] -> []
  | rest -> take chunk rest :: split ~chunksize:chunk (drop chunk rest)
;;

(*
  The standard library provides "labeled" versions of modules
 *)

let%test _ = List.map (fun n -> n * n) [ 1; 2; 3 ] = [ 1; 4; 9 ]
let%test _ = [ 1; 2; 3 ] |> List.map (fun n -> n * n) = [ 1; 4; 9 ]
let%test _ = ListLabels.map ~f:(fun n -> n * n) [ 1; 2; 3 ] = [ 1; 4; 9 ]
let%test _ = ListLabels.map [ 1; 2; 3 ] ~f:(fun n -> n * n) = [ 1; 4; 9 ]
let%test _ = [ 1; 2; 3 ] |> ListLabels.map ~f:(fun n -> n * n) = [ 1; 4; 9 ]
let%test _ = Array.map (fun n -> n * n) [| 1; 2; 3 |] = [| 1; 4; 9 |]
let%test _ = ArrayLabels.map ~f:(fun n -> n * n) [| 1; 2; 3 |] = [| 1; 4; 9 |]

(* Useful here! *)
let%test _ = [| 3; 4 |] = ArrayLabels.sub [| 1; 2; 3; 4; 5; 6 |] ~pos:2 ~len:2
let%test _ = [| 3; 4 |] = Array.sub [| 1; 2; 3; 4; 5; 6 |] 2 2

(*
 * Q1. The function [ArrayLabels.make] is not labelled.
 *     When might this cause confusion? -> when we create an int array
 *     Write a labelled version to correct this problem.
 *)

module Q1 = struct
  let make ~size = ArrayLabels.make size
  let%test _ = [| 0; 0; 0 |] = make ~size:3 0
end

(*
 * Q2. When we wrote our [fill] function with labelled arguments, we wanted to
 *     prevent someone mistakenly swapping the [start] and [length] values.
 *     Can you find a way to do this without labelled or optional arguments?
 *)

module Q2 = struct
  type fill_args = { beg : int; len : int }

  let fill arr { beg; len } v =
    for x = beg to beg + len - 1 do
      arr.(x) <- v
    done
  ;;
end

let%test _ =
  let arr = [| 0; 0; 0; 0; 0 |] in
  let () = Q2.fill arr { beg = 1; len = 3 } 1 in
  arr = [| 0; 1; 1; 1; 0 |]
;;

module Q2_book_solution = struct
  (* I forgot about this possiblity, I like it! *)
  type start = Start of int
  type length = Length of int

  let fill arr (Start beg) (Length len) v =
    for x = beg to beg + len - 1 do
      arr.(x) <- v
    done
  ;;
end

let%test _ =
  let arr = [| 0; 0; 0; 0; 0 |] in
  let () = Q2_book_solution.fill arr (Start 1) (Length 3) 1 in
  arr = [| 0; 1; 1; 1; 0 |]
;;

(*
 * Q3. Build labelled versions of functions from the [Buffer] module, choosing
 *     which functions and arguments to label as appropriate
 *)
module Q3 = struct
  let blit src ~srcoff dst ~dstoff ~len = Buffer.blit src srcoff dst dstoff len
  let add buf s ~ofs ~len = Buffer.add_substring buf s ofs len
end

(*
 * Q4. Frequently, we use an accumulator to make a function tail-recursive,
 *     wrapping it up in another function to give the initial value of the
 *     accumulator.
 *
 *     Gives an example of map_inner
 *     Use an optional argument to express this as a single function
 *)
module Q4_before = struct
  let rec map_inner acc f lst =
    match lst with
    | [] -> List.rev acc
    | h :: t -> map_inner (f h :: acc) f t
  ;;

  let map f lst = map_inner [] f lst
  let%test _ = [ 2; 4; 6 ] = map (( * ) 2) [ 1; 2; 3 ]
end

module Q4_after = struct
  let rec map ?(acc = []) f = function
    | [] -> List.rev acc
    | h :: t -> map ~acc:(f h :: acc) f t
  ;;

  let%test _ = [ 2; 4; 6 ] = map (( * ) 2) [ 1; 2; 3 ]

  (*
   Okay, but we give the caller unreasonable abilities to the caller to produce bad data that way:

   utop # Q4_after.map (( * ) 2) [1;2;3] ~acc:[0];;
   - : int list = [0; 2; 4; 6]
   *)
end
