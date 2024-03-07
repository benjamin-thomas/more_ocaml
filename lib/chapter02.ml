let println fmt = Printf.printf (fmt ^^ "\n%!")

(* Mimics the builtin `list` type *)

type 'a list = Nil | Cons of 'a * 'a list

(* This is how we would represent [1;2;3] *)
(* let one_til_three = (Cons 1, (Cons 2, Cons 3, Nil)) *)
let one_two_three = Cons (1, Cons (2, Cons (3, Nil)))

let one_two_three' = Cons ( 1
                          , Cons ( 2
                                 , Cons ( 3
                                        , Nil
                                        )
                                 )
                          ) [@@ocamlformat "disable"]

(*
  `lazylist` represents infinitely-long lists.
  It has no tail, but a "tail function".
  It has no `Nil` constructor because the list has no end.
*)
type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

(** Build an infinite (lazy) list starting from `n`, always increasing by 1.
  *)
let rec lseq n = Cons (n, fun () -> lseq (n + 1))

(* Implement lazy head, lazy tail and other lazy functions...
 * When we apply the unit, we force the evaluation of the tail.
 *)

(** Capture the head of the lazy list *)
let lhd (Cons (n, _)) = n

(** Compute the tail of the lazy list *)
let ltl (Cons (_, tf)) = tf ()

let%test _ = 8 = (lseq 8 |> lhd)
let%test _ = 9 = (lseq 8 |> ltl |> lhd)

(** Takes `n` items off the lazy list.
  * Returns a non-lazy list.
  *)
let rec ltake (Cons (h, tf)) n =
  match n with
  | 0 -> []
  | _ -> h :: ltake (tf ()) (n - 1)
;;

let%test _ = [ 0; 1; 2 ] = ltake (lseq 0) 3
let%test _ = [ 7; 8; 9 ] = ltake (lseq 7) 3

(**
  Drops `n` items from the lazy list
  It's a bit like "moving the head towards the right"
*)
let rec ldrop n (Cons (_, tf) as ll) =
  match n with
  | 0 -> ll
  | _ -> ldrop (n - 1) (tf ())
;;

let%test _ = 3 = (lseq 0 |> ldrop 3 |> lhd)
let%test _ = 9 = (lseq 8 |> ldrop 1 |> lhd)

(* "moving the head towards the right" *)
let%test _ = lseq 10 |> lhd = (lseq 0 |> ldrop 10 |> lhd)

(* lazy map *)
let rec lmap f (Cons (h, tf)) = Cons (f h, fun () -> lmap f (tf ()))
let%test _ = 18 = (lseq 9 |> lmap (( * ) 2) |> lhd)
let%test _ = 20 = (lseq 9 |> ldrop 1 |> lmap (( * ) 2) |> lhd)
let%test _ = 20 = (lseq 9 |> ltl |> lmap (( * ) 2) |> lhd)

(** Returns a new lazy list, where each item satisfy the condition `f h` to be true.
  * Care must be taken to ensure the condition returns true at least once, otherwise the recursive calls will never terminate.
  *
  * Example
  * =======
  * lfilter (fun n -> n mod 2 = 1) (lseq 0)  => Cons (1, <fun>)
  * lfilter (fun n -> n mod 2 = 1) (lseq 1)  => Cons (1, <fun>)
  * lfilter (fun n -> n mod 2 = 1) (lseq 2)  => Cons (3, <fun>)
  * lfilter (fun n -> n mod 2 = 1) (lseq 3)  => Cons (3, <fun>)
  *
  * lfilter (fun _ -> true)        (lseq 0)  => Cons (0, <fun>)
  * lfilter (fun _ -> false)       (lseq 0)  => will never terminate
  *
  * lfilter (fun n -> n mod 10 = 0) (lseq 0)  => Cons (0, <fun>)
  * lfilter (fun n -> n mod 10 = 0) (lseq 1)  => Cons (10, <fun>)
  * lfilter (fun n -> n mod 10 = 0) (lseq 10) => Cons (10, <fun>)
  * lfilter (fun n -> n mod 10 = 0) (lseq 11) => Cons (20, <fun>)
  *)
let rec lfilter f (Cons (h, tf)) =
  if f h then
    Cons (h, fun () -> lfilter f (tf ()))
  else
    lfilter f (tf ())
;;

(** Return an infinite list of even numbers *)
let evens = lfilter (fun n -> n mod 2 = 0) (lseq 0)

let%test _ = 0 = (evens |> ldrop 0 |> lhd)
let%test _ = 2 = (evens |> ldrop 1 |> lhd)
let%test _ = 4 = (evens |> ldrop 2 |> lhd)
let%test _ = 6 = (evens |> ldrop 3 |> lhd)

(** Return an infinite list of odd numbers *)
let odds = lfilter (fun n -> n mod 2 = 1) (lseq 0)

let%test _ = 1 = (odds |> ldrop 0 |> lhd)
let%test _ = 3 = (odds |> ldrop 1 |> lhd)
let%test _ = 5 = (odds |> ldrop 2 |> lhd)
let%test _ = 7 = (odds |> ldrop 3 |> lhd)

(** Another way to return an infinite list of odd numbers *)
let odds = lmap (fun n -> n + 1) evens

let%test _ = 1 = (odds |> ldrop 0 |> lhd)
let%test _ = 3 = (odds |> ldrop 1 |> lhd)
let%test _ = 5 = (odds |> ldrop 2 |> lhd)
let%test _ = 7 = (odds |> ldrop 3 |> lhd)
let fours = lmap (fun n -> n * 2) evens
let%test _ = 0 = (fours |> ldrop 0 |> lhd)
let%test _ = 4 = (fours |> ldrop 1 |> lhd)
let%test _ = 8 = (fours |> ldrop 2 |> lhd)
let%test _ = 12 = (fours |> ldrop 3 |> lhd)

(** Return a list returning the square of its doubles *)
let dbl_sq = lmap (fun n -> n * n) evens

let%test _ = 0 = (dbl_sq |> ldrop 0 |> lhd) (* 0 -> 0*)
let%test _ = 4 = (dbl_sq |> ldrop 1 |> lhd) (* 2 -> 4 *)
let%test _ = 16 = (dbl_sq |> ldrop 2 |> lhd) (* 4 -> 16*)
let%test _ = 36 = (dbl_sq |> ldrop 3 |> lhd) (* 6 -> 36 *)
let%test _ = 64 = (dbl_sq |> ldrop 4 |> lhd) (* 8 -> 64 *)
let%test _ = 100 = (dbl_sq |> ldrop 5 |> lhd) (* 10 -> 100*)

(** Returns a lazy list where each element is a multiple of 5, raised to the power of 3.
  *
  * [5^3, 10^3, 15^3] = [125, 1000, 3375]
  *)
let cubes = lfilter (fun x -> x mod 5 = 0) (lmap (fun x -> x * x * x) (lseq 1))

let%test _ = [ 125; 1000; 3375 ] = ltake cubes 3

let rec threes (Cons (h, tf)) =
  Cons (h
       , fun () ->
           threes
             (lmap
               (fun x -> x + 2)
               (tf ())
             )
       )
[@@ocamlformat "disable"]

let%test _ =
  [ 0; 3; 6; 9; 12; 15; 18; 21; 24; 27 ] = ltake (threes @@ lseq 0) 10
;;

let%test _ =
  [ 0; 3; 6; 9; 12; 15; 18; 21; 24; 27 ]
  = ltake (lmap (fun n -> n * 3) (lseq 0)) 10
;;

let rec fours (Cons (h, tf)) =
  Cons (h
       , fun () ->
           fours
             (lmap
               (fun x -> x + 3)
               (tf ())
             )
       )
[@@ocamlformat "disable"]

let%test _ =
  [ 0; 4; 8; 12; 16; 20; 24; 28; 32; 36 ] = ltake (fours @@ lseq 0) 10
;;

let rec fives (Cons (h, tf)) =
  Cons (h
       , fun () ->
           fives
             (lmap
               (fun x -> x + 4)
               (tf ())
             )
       )
[@@ocamlformat "disable"]

let%test _ =
  [ 0; 5; 10; 15; 20; 25; 30; 35; 40; 45 ] = ltake (fives @@ lseq 0) 10
;;

let%test_unit _ =
  let test = [%test_result: Base.int Base.list] in
  ()
  ; test
      (ltake (fours (lseq 0)) 10)
      ~expect:[ 0; 4; 8; 12; 16; 20; 24; 28; 32; 36 ]
;;

let comma_print lst =
  lst |> List.map string_of_int |> String.concat ", " |> print_string
;;

let%expect_test _ =
  ltake (fours (lseq 0)) 10 |> comma_print
  ; [%expect {| 0, 4, 8, 12, 16, 20, 24, 28, 32, 36 |}]
;;

let%expect_test _ =
  comma_print @@ ltake (fours (lseq 0)) 10
  ; [%expect {| 0, 4, 8, 12, 16, 20, 24, 28, 32, 36 |}]
;;

let%expect_test "ones" =
  let rec ones (Cons (h, tf)) =
    (* The lazy list is already increasing its values (from [lseq]) *)
    Cons (h, fun () -> ones (lmap (( + ) 0) (tf ())))
  in
  ()
  ; comma_print @@ ltake (ones (lseq 0)) 10
  ; [%expect {| 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 |}]

  ; comma_print @@ ltake (ones (lseq 1)) 10
  ; [%expect {| 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 |}]

  ; comma_print @@ ltake (ones (lseq 2)) 10
  ; [%expect {| 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 |}]

  ; comma_print @@ ltake (ones (lseq 3)) 10
  ; [%expect {| 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 |}]
;;

let%expect_test "twos" =
  let rec twos (Cons (h, tf)) =
    (* We add 1 to the already increasing values (from [lseq]) *)
    Cons (h, fun () -> twos (lmap (( + ) 1) (tf ())))
  in
  ()
  ; comma_print @@ ltake (twos (lseq 0)) 10
  ; [%expect {| 0, 2, 4, 6, 8, 10, 12, 14, 16, 18 |}]

  ; comma_print @@ ltake (twos (lseq 1)) 10
  ; [%expect {| 1, 3, 5, 7, 9, 11, 13, 15, 17, 19 |}]

  ; comma_print @@ ltake (twos (lseq 2)) 10
  ; [%expect {| 2, 4, 6, 8, 10, 12, 14, 16, 18, 20 |}]

  ; comma_print @@ ltake (twos (lseq 3)) 10
  ; [%expect {| 3, 5, 7, 9, 11, 13, 15, 17, 19, 21 |}]
;;

let%expect_test "threes" =
  let rec lseq n = Cons (n, fun () -> lseq (n + 1)) in
  let rec threes (Cons (h, tf)) =
    (* We add 2 to the already increasing values (from [lseq]) *)
    Cons (h, fun () -> threes (lmap (( + ) 2) (tf ())))
  in
  ()
  ; comma_print @@ ltake (threes (lseq 0)) 10
  ; [%expect {| 0, 3, 6, 9, 12, 15, 18, 21, 24, 27 |}]

  ; comma_print @@ ltake (threes (lseq 1)) 10
  ; [%expect {| 1, 4, 7, 10, 13, 16, 19, 22, 25, 28 |}]

  ; comma_print @@ ltake (threes (lseq 2)) 10
  ; [%expect {| 2, 5, 8, 11, 14, 17, 20, 23, 26, 29 |}]

  ; comma_print @@ ltake (threes (lseq 3)) 10
  ; [%expect {| 3, 6, 9, 12, 15, 18, 21, 24, 27, 30 |}]
;;

let%expect_test "threes, again" =
  (* We could simply defined our own [lseq] equivalent for a simpler construct. *)
  let rec lseq3 n = Cons (n, fun () -> lseq3 (n + 3)) in

  ()
  ; comma_print @@ ltake (lseq3 0) 10
  ; [%expect {| 0, 3, 6, 9, 12, 15, 18, 21, 24, 27 |}]

  ; comma_print @@ ltake (lseq3 1) 10
  ; [%expect {| 1, 4, 7, 10, 13, 16, 19, 22, 25, 28 |}]

  ; comma_print @@ ltake (lseq3 2) 10
  ; [%expect {| 2, 5, 8, 11, 14, 17, 20, 23, 26, 29 |}]

  ; comma_print @@ ltake (lseq3 3) 10
  ; [%expect {| 3, 6, 9, 12, 15, 18, 21, 24, 27, 30 |}]
;;

let%expect_test "same" =
  (* This one always returns the same value *)
  let rec lseq0 n = Cons (n, fun () -> lseq0 n) in

  ()
  ; comma_print @@ ltake (lseq0 0) 10
  ; [%expect {| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 |}]

  ; comma_print @@ ltake (lseq0 1) 10
  ; [%expect {| 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 |}]

  ; comma_print @@ ltake (lseq0 2) 10
  ; [%expect {| 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 |}]

  ; comma_print @@ ltake (lseq0 3) 10
  ; [%expect {| 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 |}]
    (* ; let lseq3 (x : int) = lmap (( + ) 3) (lseq0 x) in 6 6 6 6 *)

  ; comma_print @@ ltake (lmap (( + ) 1) (lseq0 3)) 10
  ; [%expect {| 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 |}]

  ; (* But we could also turn it again into a seq3, as above, via a recursive + map *)
    let rec make_lseq3 (Cons (h, tf)) =
      Cons (h, fun () -> make_lseq3 (lmap (( + ) 3) (tf ())))
    in
    ()
    ; comma_print @@ ltake (make_lseq3 (lseq0 0)) 10
    ; [%expect {| 0, 3, 6, 9, 12, 15, 18, 21, 24, 27 |}]

    ; comma_print @@ ltake (make_lseq3 (lseq0 1)) 10
    ; [%expect {| 1, 4, 7, 10, 13, 16, 19, 22, 25, 28 |}]

    ; comma_print @@ ltake (make_lseq3 (lseq0 2)) 10
    ; [%expect {| 2, 5, 8, 11, 14, 17, 20, 23, 26, 29 |}]

    ; comma_print @@ ltake (make_lseq3 (lseq0 3)) 10
    ; [%expect {| 3, 6, 9, 12, 15, 18, 21, 24, 27, 30 |}]

    ; let lseq3 n = make_lseq3 (lseq0 n) in
      (*
        We could also (re)define `lseq3`, as above.
        let rec lseq3 n = Cons (n, fun () -> lseq3 (n + 3)) in
      *)
      ()
      ; comma_print @@ ltake (lseq3 0) 10
      ; [%expect {| 0, 3, 6, 9, 12, 15, 18, 21, 24, 27 |}]

      ; comma_print @@ ltake (lseq3 1) 10
      ; [%expect {| 1, 4, 7, 10, 13, 16, 19, 22, 25, 28 |}]

      ; comma_print @@ ltake (lseq3 2) 10
      ; [%expect {| 2, 5, 8, 11, 14, 17, 20, 23, 26, 29 |}]

      ; comma_print @@ ltake (lseq3 3) 10
      ; [%expect {| 3, 6, 9, 12, 15, 18, 21, 24, 27, 30 |}]
;;

let%expect_test "fours" =
  (* We add 3 to the already increasing values (from [lseq]) *)
  let rec fours (Cons (h, tf)) =
    Cons (h, fun () -> fours (lmap (( + ) 3) (tf ())))
  in
  ()
  ; comma_print @@ ltake (fours (lseq 0)) 10
  ; [%expect {| 0, 4, 8, 12, 16, 20, 24, 28, 32, 36 |}]

  ; comma_print @@ ltake (fours (lseq 1)) 10
  ; [%expect {| 1, 5, 9, 13, 17, 21, 25, 29, 33, 37 |}]

  ; comma_print @@ ltake (fours (lseq 2)) 10
  ; [%expect {| 2, 6, 10, 14, 18, 22, 26, 30, 34, 38 |}]

  ; comma_print @@ ltake (fours (lseq 3)) 10
  ; [%expect {| 3, 7, 11, 15, 19, 23, 27, 31, 35, 39 |}]
;;

let%expect_test _ =
  comma_print @@ ltake (fives (lseq 0)) 10
  ; [%expect {| 0, 5, 10, 15, 20, 25, 30, 35, 40, 45 |}]
;;

let%test _ =
  [ 0; 5; 10; 15; 20; 25; 30; 35; 40; 45 ]
  = ltake (lmap (fun n -> n * 5) (lseq 0)) 10
;;

let rec make_primes (Cons (h, tf)) =
  (* n mod 0      -> Division by zero error
   * n mod 1 <> 0 -> Infinite loop because (n mod 1 = 0)
   *)
  let () = assert (h > 1) in
  let next_primes = lfilter (fun x -> x mod h <> 0) (tf ()) in
  Cons (h, fun () -> make_primes next_primes)
;;

let primes = make_primes (lseq 2)
let%test _ = [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29 ] = ltake primes 10

(* We cannot append to a lazy (infinite) list. But we can interleave items from 2 lazy lists *)
let rec interleave (Cons (ha, tfa)) llb =
  Cons (ha, fun () -> interleave llb (tfa ()))
;;

let%expect_test _ =
  let rec zeros = Cons (0, fun () -> zeros) in
  let rec ones  = Cons (1, fun () -> ones) in
  ()
  ; comma_print @@ ltake zeros 10
  ; [%expect {| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 |}]
  ; comma_print @@ ltake ones 10
  ; [%expect {| 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 |}]
  ; comma_print @@ ltake (interleave zeros ones) 10
  ; [%expect {| 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 |}]
[@@ocamlformat "disable"]

let rec allfrom lst =
  Cons (lst, fun () -> interleave (allfrom (0 :: lst)) (allfrom (1 :: lst)))
;;

(* This one is crazy:

   It take a list and returns a lazy list, prepended (interleaved) with 0 and 1.

   utop # ltake (allfrom []) (1+2+4+8+16);;
   - : int list list =
    [ []
    ; [0]; [1]
    ; [0; 0]; [0; 1]; [1; 0]; [1; 1]
    ; [0; 0; 0]; [0; 0; 1]; [0; 1; 0]; [0; 1; 1]; [1; 0; 0]; [1; 0; 1]; [1; 1; 0]; [1; 1; 1]
    ; [0; 0; 0; 0]; [0; 0; 0; 1]; [0; 0; 1; 0]; [0; 0; 1; 1]; [0; 1; 0; 0]; [0; 1; 0; 1]; [0; 1; 1; 0]; [0; 1; 1; 1]; [1; 0; 0; 0]; [1; 0; 0; 1]; [1; 0; 1; 0]; [1; 0; 1; 1]; [1; 1; 0; 0]; [1; 1; 0; 1]; [1; 1; 1; 0]
    ]


                   []
                   /\
                  /  \
                 /    \
                /      \
               /        \
              /          \
             /            \
           [0]            [1]
           / \            / \
          /   \          /   \
       [0;0] [1;0]    [0;1] [1;1]
*)

(*
 * EXERCISES
 *)

(*
   1. Write the lazy list whose elements are the numbers 1,2,4,8,16...
      What is its type? => int lazylist

   [2^0, 2^1, 2^2, 2^3, 2^4, 2^5] = [1, 2, 4,  8, 16,  32]
   [3^0, 3^1, 3^2, 3^3, 3^4, 3^5] = [1, 3, 9, 27, 81, 243]
 *)

(* First draft, not tail recursive
   let rec pow n exp =
     let () = assert (n >= 0) in
     match exp with
     | 0 -> 1
     | _ -> n * pow n (exp - 1)
   ;;
*)

let pow n exp =
  let () = assert (n >= 0) in
  let rec loop exp acc =
    match exp with
    | 0 -> acc
    | _ -> loop (exp - 1) (acc * n)
  in
  loop exp 1
;;

(*

Break down of how the [pow] function works:

[2^0, 2^1, 2^2, 2^3, 2^4, 2^5] = [1, 2, 4,  8, 16,  32]

pow n=2 exp=5
  loop exp=5 acc=(1 * 2)
    loop exp=4 acc=(1 * 2 * 2)
      loop exp=3 acc=(1 * 2  * 2 * 2)
        loop exp=2 acc=(1 * 2  * 2 * 2 * 2)
          loop exp=1 acc=(1 * 2  * 2 * 2 * 2 * 2)
            loop exp=0 acc=32
            32
            pow 2 5 = 32
*)

let%test_unit _ =
  let ( => ) = [%test_eq: Base.int] in
  ()
  (* pow2 *)
  ; pow 2 0 => 1
  ; pow 2 1 => 2
  ; pow 2 2 => 4
  ; pow 2 3 => 8
  ; pow 2 4 => 16
  ; pow 2 5 => 32
  (* pow3 *)
  ; pow 3 0 => 1
  ; pow 3 1 => 3
  ; pow 3 2 => 9
  ; pow 3 3 => 27
  ; pow 3 4 => 81
  ; pow 3 5 => 243
[@@ocamlformat "disable"]

let rec make_pow n (Cons (h, tf)) = Cons (pow n h, fun () -> make_pow n (tf ()))

(* My solution was more complicated than needed, see below. *)
(* let pow2 = make_pow 2 (lseq 0) *)

let pow3 = make_pow 3 (lseq 0)

(* BEG: book solution *)
let rec ldouble n = Cons (n, fun () -> ldouble (n * 2))
let pow2 = ldouble 1
(* END: book solution *)

let%expect_test _ =
  ()
  ; comma_print @@ ltake pow2 10
  ; [%expect {| 1, 2, 4, 8, 16, 32, 64, 128, 256, 512 |}]
  ; comma_print @@ ltake pow3 10
  ; [%expect {| 1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683 |}]
;;

(*
 * 2. Write a function to return the nth element of a lazy list where the element zero is the head of the list
 *)

let take_at n ll = ldrop n ll |> lhd
let%test _ = 5 = take_at 5 (lseq 0)

(* BEG: book solution *)
let rec lnth (Cons (h, tf)) n =
  match n with
  | 0 -> h
  | _ -> lnth (tf ()) (n - 1)
;;

let%test _ = 5 = lnth (lseq 0) 5
(* END: book solution *)

(*
 * 3. Write a function which, given a list, returns the lazy list forming a repeated sequence taken from that list.
 *    For example, given the list [1;2;3]  , it should return a lazy list with elements 1,2,3,1,2,3,1,2...
 *)

let rec repeat lst =
  let () = assert (lst <> []) in
  let rec loop = function
    | [] -> repeat lst
    | x :: xs -> Cons (x, fun () -> loop xs)
  in
  loop lst
;;

let%expect_test _ =
  ()
  ; comma_print @@ ltake (repeat [ 1; 2; 3 ]) 9
  ; [%expect {| 1, 2, 3, 1, 2, 3, 1, 2, 3 |}]
;;

(*
 * 4. Write a lazy list whose elements are the fibonacci numbers 0,1,1,2,3,5,8... whose first two elements
 *    are zero and one by definition, and each ensuing element is the sum of the previous two.
 *)

(* Simple version *)
let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
[@@warning "-32"]
;;

(* Optimized version *)
let fib n =
  let not_set = -1 in
  let rec loop n memo =
    let use_memo n =
      let cache = memo.(n) in
      if cache = not_set then
        let new_val = loop n memo in
        let () = memo.(n) <- new_val in
        new_val
      else
        cache
    in

    match n with
    | 0 -> 0
    | 1 -> 1
    | _ ->
        let a = use_memo (n - 1) in
        let b = use_memo (n - 2) in
        a + b
  in
  let init_memo =
    (* By making the fixed-sized array's length based on the user input,
       we ensure it'll always be the correct length! *)
    Array.make n not_set
  in
  loop n init_memo
;;

let rec make_fib (Cons (h, tf)) = Cons (fib h, fun () -> make_fib @@ tf ())
let lfib = make_fib (lseq 0)

let%expect_test _ =
  ()
  ; comma_print @@ ltake lfib 10
  ; [%expect {| 0, 1, 1, 2, 3, 5, 8, 13, 21, 34 |}]
;;

(* START: Book solution *)
let lfib =
  let rec inner x y = Cons (x, fun () -> inner y (x + y)) in
  inner 0 1
;;

let%expect_test _ =
  ()
  ; comma_print @@ ltake lfib 10
  ; [%expect {| 0, 1, 1, 2, 3, 5, 8, 13, 21, 34 |}]
;;

(* END: Book solution *)

(* Another solution, many months later.

   Interesting to note that such a simple solution is very **much** more performant
   that the "naive" implementation of fib (computing n-1 + n-2 recursively).
*)

(** unfold takes a function [f] that will generate the next state,
    and the initial state [init] *)
let rec unfold f init =
  let (a, b) = f init in
  Cons (a, fun () -> unfold f b)
;;

let lfib = unfold (fun (a, b) -> (a, (b, a + b))) (0, 1)

(* Using the standard lib, for comparaison *)
let fibs = Seq.unfold (fun (a, b) -> Some (a, (b, a + b))) (0, 1)

let%expect_test _ =
  ()
  ; comma_print @@ ltake lfib 10
  ; [%expect {| 0, 1, 1, 2, 3, 5, 8, 13, 21, 34 |}]
  ; ()
  ; comma_print @@ List.of_seq @@ Seq.take 10 fibs
  ; [%expect {| 0, 1, 1, 2, 3, 5, 8, 13, 21, 34 |}]
;;

(*
 * 5. Write the function [unleave] which, given a lazy list, returns two lazy lists,
 *    one containing elements at positions 0,2,4,6... of the original list, and the other
 *    containing elements at positions 1,3,5,7...
 *)

(* The reverse of [interleave] *)
let unleave ll =
  let rec inner ll want_evens =
    let (Cons (h1, tf1)) = ll in
    let (Cons (h2, tf2)) = tf1 () in
    let h =
      if want_evens then
        h1
      else
        h2
    in
    Cons (h, fun () -> inner (tf2 ()) want_evens)
  in

  let lla = inner ll true in
  let llb = inner ll false in

  (lla, llb)
;;

let%expect_test _ =
  let (a, b) = unleave (lseq 0) in
  ()
  ; comma_print @@ ltake a 10
  ; [%expect {| 0, 2, 4, 6, 8, 10, 12, 14, 16, 18 |}]
  ; comma_print @@ ltake b 10
  ; [%expect {| 1, 3, 5, 7, 9, 11, 13, 15, 17, 19 |}]
;;

(* START: Book solution *)
let rec unleave (Cons (h1, tf1)) =
  let (Cons (h2, tf2)) = tf1 () in
  let t = tf2 () in
  ( Cons (h1, fun () -> fst (unleave t))
  , Cons (h2, fun () -> snd (unleave t))
  )
[@@ocamlformat "disable"]

let%expect_test _ =
  let (a, b) = unleave (lseq 0) in
  ()
  ; comma_print @@ ltake a 10
  ; [%expect {| 0, 2, 4, 6, 8, 10, 12, 14, 16, 18 |}]
  ; comma_print @@ ltake b 10
  ; [%expect {| 1, 3, 5, 7, 9, 11, 13, 15, 17, 19 |}]
;;

(* END: Book solution *)

(*
 * 6. Alphanumeric labels in documents go A,B,C,...,X,Y,Z,AA,AB,...,BA,BB,...AAA,...
 *    Write the lazy list containing strings representing this sequence.
 *    You may (mis)use the Standard Library function [Char.escaped] to convert a character to a string
 *)
(* let alpha = [ 'A'; 'B'; 'C' ] *)

let rec alpha_from lst =
  Cons
    ( lst
    , fun () ->
        interleave (alpha_from ('A' :: lst))
        @@ interleave (alpha_from ('B' :: lst)) (alpha_from ('C' :: lst)) )
;;

let rec letter i =
  match i with
  | 0 -> "A"
  | 1 -> "B"
  | 2 -> "C"
  | 3 -> "D"
  | 4 -> "E"
  | 5 -> "F"
  (* | 6 -> "G"
     | 7 -> "H"
     | 8 -> "I"
     | 9 -> "J"
     | 10 -> "K"
     | 11 -> "L"
     | 12 -> "M"
     | 13 -> "N"
     | 14 -> "O"
     | 15 -> "P"
     | 16 -> "Q"
     | 17 -> "R"
     | 18 -> "S"
     | 19 -> "T"
     | 20 -> "U"
     | 21 -> "V"
     | 22 -> "W"
     | 23 -> "X"
     | 24 -> "Y"
     | 25 -> "Z" *)
  | _ ->
      let base = i / 6 in
      let remaining = i mod 6 in
      letter (base - 1) ^ letter remaining
;;

let labels = lmap letter (lseq 0)

let%expect_test _ =
  let comma_print lst = lst |> String.concat ", " |> print_string in
  ()
  ; comma_print @@ ltake labels (1 + pow 6 3 + pow 6 2 + pow 6 1)
  ; [%expect
      "A, B, C, D, E, F, AA, AB, AC, AD, AE, AF, BA, BB, BC, BD, BE, BF, CA, \
       CB, CC, CD, CE, CF, DA, DB, DC, DD, DE, DF, EA, EB, EC, ED, EE, EF, FA, \
       FB, FC, FD, FE, FF, AAA, AAB, AAC, AAD, AAE, AAF, ABA, ABB, ABC, ABD, \
       ABE, ABF, ACA, ACB, ACC, ACD, ACE, ACF, ADA, ADB, ADC, ADD, ADE, ADF, \
       AEA, AEB, AEC, AED, AEE, AEF, AFA, AFB, AFC, AFD, AFE, AFF, BAA, BAB, \
       BAC, BAD, BAE, BAF, BBA, BBB, BBC, BBD, BBE, BBF, BCA, BCB, BCC, BCD, \
       BCE, BCF, BDA, BDB, BDC, BDD, BDE, BDF, BEA, BEB, BEC, BED, BEE, BEF, \
       BFA, BFB, BFC, BFD, BFE, BFF, CAA, CAB, CAC, CAD, CAE, CAF, CBA, CBB, \
       CBC, CBD, CBE, CBF, CCA, CCB, CCC, CCD, CCE, CCF, CDA, CDB, CDC, CDD, \
       CDE, CDF, CEA, CEB, CEC, CED, CEE, CEF, CFA, CFB, CFC, CFD, CFE, CFF, \
       DAA, DAB, DAC, DAD, DAE, DAF, DBA, DBB, DBC, DBD, DBE, DBF, DCA, DCB, \
       DCC, DCD, DCE, DCF, DDA, DDB, DDC, DDD, DDE, DDF, DEA, DEB, DEC, DED, \
       DEE, DEF, DFA, DFB, DFC, DFD, DFE, DFF, EAA, EAB, EAC, EAD, EAE, EAF, \
       EBA, EBB, EBC, EBD, EBE, EBF, ECA, ECB, ECC, ECD, ECE, ECF, EDA, EDB, \
       EDC, EDD, EDE, EDF, EEA, EEB, EEC, EED, EEE, EEF, EFA, EFB, EFC, EFD, \
       EFE, EFF, FAA, FAB, FAC, FAD, FAE, FAF, FBA, FBB, FBC, FBD, FBE, FBF, \
       FCA, FCB, FCC, FCD, FCE, FCF, FDA, FDB, FDC, FDD, FDE, FDF, FEA, FEB, \
       FEC, FED, FEE, FEF, FFA, FFB, FFC, FFD, FFE, FFF, AAAA"]
;;
