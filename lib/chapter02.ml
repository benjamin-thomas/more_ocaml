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
  It `Nil` constructor because the list has no end.
*)
type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

(* Build a lazylist up to `n` integer *)
let rec lseq n = Cons (n, fun () -> lseq (n + 1))

(*
   Implement lazy head, lazy tail and other lazy functions...

   When we apply the unit, we force the evaluation of the tail.
*)
let lhd (Cons (n, _)) = n
let ltl (Cons (_, tf)) = tf ()

let%test _ = 8 = (lseq 8 |> lhd)
let%test _ = 9 = (lseq 8 |> ltl |> lhd)

let rec ltake n (Cons (h, tf)) =
  match n with
  | 0 -> []
  | _ -> h :: ltake (n - 1) (tf ())
;;

let%test _ = [ 0; 1; 2 ] = (lseq 0 |> ltake 3)
let%test _ = [ 7; 8; 9 ] = (lseq 7 |> ltake 3)

let rec ldrop n (Cons (_, tf) as ll) =
  match n with
  | 0 -> ll
  | _ -> ldrop (n - 1) (tf ())
;;

let%test _ = 3 = (lseq 0 |> ldrop 3 |> lhd)
let%test _ = 9 = (lseq 8 |> ldrop 1 |> lhd)

let rec lmap f (Cons (h, tf)) = Cons (f h, fun () -> lmap f (tf ()))

let%test _ = 18 = (lseq 9 |> lmap (( * ) 2) |> lhd)
let%test _ = 20 = (lseq 9 |> ldrop 1 |> lmap (( * ) 2) |> lhd)
let%test _ = 20 = (lseq 9 |> ltl |> lmap (( * ) 2) |> lhd)

let rec lfilter f (Cons (h, tf)) =
  if f h then
    Cons (h, fun () -> lfilter f (tf ()))
  else
    lfilter f (tf ())
;;

let cubes = lfilter (fun x -> x mod 5 = 0) (lmap (fun x -> x * x * x) (lseq 1))
