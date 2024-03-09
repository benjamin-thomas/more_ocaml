[@@@warning "-27"]

(*
    dune runtest -w
*)

(** Places [x] at each position of the list [lst]

*)
let rec interleave x seen lst =
  match lst with
  | [] -> [ seen @ [ x ] ]
  | h :: t -> (seen @ (x :: h :: t)) :: interleave x (seen @ [ h ]) t
;;

let%expect_test _ =
  let print lst =
    let open Core in
    print_s [%sexp (lst : int list list)]
  in
  let interleave e seen lst = print @@ interleave e seen lst in
  ()
  ; interleave 1 [] []
  ; [%expect {| ((1)) |}]
  ; ()
  ; interleave 1 [] [ 2 ]
  ; [%expect {| ((1 2) (2 1)) |}]
  ; ()
  ; interleave 1 [] [ 2; 3 ]
  ; [%expect {| ((1 2 3) (2 1 3) (2 3 1)) |}]
;;

let combine x ps : 'a list = List.concat (List.map (interleave x []) ps)

let%expect_test _ =
  let print lst =
    let open Core in
    print_s [%sexp (lst : int list list)]
  in
  let combine e ps = print @@ combine e ps in
  ()
  ; combine 1 [ [] ]
  ; [%expect {| ((1)) |}]
  ; ()
  ; combine 1 [ [ 2 ] ]
  ; [%expect {| ((1 2) (2 1)) |}]
  ; ()
  ; combine 1 [ [ 2; 3 ] ]
  ; [%expect {| ((1 2 3) (2 1 3) (2 3 1)) |}]
;;

(** Calculate the n! permutations of a given list of length n *)
let rec perms p =
  match p with
  | [] -> [ [] ]
  | h :: t -> combine h (perms t)
;;

let%expect_test _ =
  let print lst =
    let open Core in
    print_s [%sexp (lst : int list list)]
  in
  let perms args = print @@ perms args in
  ()
  ; perms []
  ; [%expect {| (()) |}]
  ; ()
  ; perms [ 1 ]
  ; [%expect {| ((1)) |}]
  ; ()
  ; perms [ 1; 2 ]
  ; [%expect {| ((1 2) (2 1)) |}]
  ; ()
  ; perms [ 1; 2; 3 ]
  ; [%expect {| ((1 2 3) (2 1 3) (2 3 1) (1 3 2) (3 1 2) (3 2 1)) |}]
;;

(**
  The previous version of [interleave] would generate stack overflows quickly with prior OCaml versions.
  Here's an alternative version which is tail-recursive. It uses an accumulator.
*)
let rec interleave2 acc x seen lst =
  match lst with
  | [] -> (seen @ [ x ]) :: acc |> List.rev
  | h :: t -> interleave2 ((seen @ (x :: h :: t)) :: acc) x (seen @ [ h ]) t
;;

let%expect_test _ =
  let print lst =
    let open Core in
    print_s [%sexp (lst : int list list)]
  in
  let interleave2 e seen lst = print @@ interleave2 [] e seen lst in
  ()
  ; interleave2 1 [] []
  ; [%expect {| ((1)) |}]
  ; ()
  ; interleave2 1 [] [ 2 ]
  ; [%expect {| ((1 2) (2 1)) |}]
  ; ()
  ; interleave2 1 [] [ 2; 3 ]
  ; [%expect {| ((1 2 3) (2 1 3) (2 3 1)) |}]
;;

(*

Another method of finding permutations which gives a shorter solution, is to
choose each element of the input list and use that as the first element,
prepending it to each of the permutations of the remaiming list.

First, let us define a function to remove the (first) occurence of a given
element from a list.

It need not be tail-recursive, since the list will always be small.

*)

let rec without x = function
  | [] -> []
  | h :: t ->
      if h = x then
        t
      else
        h :: without x t
;;

(*
> This function has the advantage of producing the items in proper lexicographic (dictionary) order.

(It's also much simpler!)
*)
let rec perms = function
  | [] -> [ [] ]
  | xs ->
      List.concat
      @@ List.map
           (fun x -> List.map
                       (fun xs' -> x :: xs')
                       (perms (without x xs)))
           xs
[@@ocamlformat "disable"]

let%expect_test _ =
  let print lst =
    let open Core in
    print_s [%sexp (lst : int list list)]
  in
  let perms args = print @@ perms args in
  ()
  ; perms []
  ; [%expect {| (()) |}]
  ; ()
  ; perms [ 1 ]
  ; [%expect {| ((1)) |}]
  ; ()
  ; perms [ 1; 2 ]
  ; [%expect {| ((1 2) (2 1)) |}]
  ; ()
  ; perms [ 1; 2; 3 ]
    (* NOTE: the arrangment of the sub lists is slightly different from the original [perm] implementation *)
  ; [%expect {| ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)) |}]
;;

(*
There is a well-known algorithm for generating the lexicographically-next
permutation given the current one.

We can use this to build a function to generate all the permutations without
the heavy recursion of our previous programs and, more interestingly, to build
a lazy list of all the permutations, which requires minimal computation each
time a new permutation is needed.


(1 2 3) <- first permutation (must be sorted)

Step 1: find the smallest item on the right, call it "first"
Step 2: find the smallest item to the right of "first", call it "last"
Step 3: swap first and last
Step 4: sort everything to the right of "first"'s original position

(3 2 1) <- final permutation

---

(1  2ᶠ 3ˡ)
(1ᶠ 3  2ˡ)
(2  1ᶠ 3ˡ)
(2ᶠ 3ˡ 1 )
(3  1ᶠ 2ˡ)
(3  2  1 )

*)

(** The [first] function finds the "first" item in the array.
    That is, the right-most item which is smaller than the its next item.

*)
let first arr =
  let f = ref (Array.length arr - 1) in
  ()
  ; for x = 0 to Array.length arr - 2 do
      if arr.(x) < arr.(x + 1) then f := x
    done
  ; !f
;;

let%expect_test _ =
  let print arr =
    let open Core in
    print_s [%sexp (arr : int)]
  in
  let first arr = print @@ first arr in
  ()
  ; first [| 1; 2; 3 |] (* 1 smaller than 2 *)
  ; [%expect {| 1 |}]
  ; ()
  ; first [| 1; 3; 2 |]
  ; [%expect {| 0 |}]
  ; ()
  ; first [| 2; 1; 3 |]
  ; [%expect {| 1 |}]
  ; ()
  ; first [| 2; 3; 1 |]
  ; [%expect {| 0 |}]
  ; ()
  ; first [| 3; 1; 2 |]
  ; [%expect {| 1 |}]
  ; ()
  ; first [| 3; 2; 1 |]
  ; [%expect {| 2 |}]
;;

(** The corresponding [last] function is a little more awkward, needing to
    check two conditions and using a special initializer, rather than simply
    initializing with the last element as [first] does.

    "last" is the smallest item to the right of the "first" item.
  *)
let last arr f =
  let c = ref (-1) in
  ()
  ; for x = Array.length arr - 1 downto f + 1 do
      if arr.(x) > arr.(f) && (!c = -1 || arr.(x) < arr.(!c)) then c := x
    done
  ; !c
;;

let%expect_test _ =
  let print arr =
    let open Core in
    print_s [%sexp (arr : int)]
  in
  let last arr f = print @@ last arr f in
  ()
  ; last [| 1; 2; 3 |] 1
  ; [%expect {| 2 |}]
  ; ()
  ; last [| 1; 3; 2 |] 0
  ; [%expect {| 2 |}]
  ; ()
  ; last [| 2; 1; 3 |] 1
  ; [%expect {| 2 |}]
  ; ()
  ; last [| 2; 3; 1 |] 0
  ; [%expect {| 1 |}]
  ; ()
  ; last [| 3; 1; 2 |] 1
  ; [%expect {| 2 |}]
  ; ()
  ; last [| 3; 2; 1 |] 2
  ; [%expect {| -1 |}]
;;

(** The [swap] utility function swaps two items in an array. *)
let swap arr a b =
  let tmp = arr.(a) in
  ()
  ; arr.(a) <- arr.(b)
  ; arr.(b) <- tmp
;;

(** The [sort_sub] utility function sorts part of an array, from offset [pos] to length [len].  *)
let sort_sub arr pos len =
  let sub = Array.sub arr pos len in
  ()
  ; Array.sort compare sub
  ; Array.blit sub 0 arr pos len
;;

let next_permutation arr_in =
  let arr = Array.copy arr_in in
  let f = first arr in
  let l = last arr f in
  ()
  ; swap arr f l
  ; sort_sub arr (f + 1) (Array.length arr - 1 - f)
  ; arr
;;

let non_increasing arr =
  Array.length arr <= 1
  ||
  let r = ref true in
  ()
  ; for x = 0 to Array.length arr - 2 do
      if arr.(x + 1) > arr.(x) then r := false
    done
  ; !r
;;

let all_permutations arr =
  let copy = Array.copy arr in
  ()
  ; Array.sort compare copy
  ; let perm = ref copy in
    let perms = ref [ copy ] in
    ()
    ; while not (non_increasing !perm) do
        ()
        ; perm := next_permutation !perm
        ; perms := !perm :: !perms
      done
    ; Array.of_list (List.rev !perms)
;;

let%expect_test _ =
  let print arr =
    let open Core in
    print_s [%sexp (arr : int array array)]
  in
  let all_permutations arr = print @@ all_permutations arr in
  ()
  ; all_permutations [| 1; 2; 3 |]
  ; [%expect {| ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)) |}]
;;

type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

let rec ltake (Cons (h, tf)) = function
  | 0 -> []
  | n -> h :: ltake (tf ()) (n - 1)
;;

let rec perms x =
  Cons
    ( x
    , fun () ->
        if non_increasing x then (
          let c = Array.copy x in
          ()
          ; Array.sort compare c
          ; perms c
        ) else
          perms (next_permutation x) )
;;

let%expect_test _ =
  let print lst =
    let open Core in
    print_s [%sexp (lst : int array list)]
  in
  let perms arr =
    let perms_stream : 'a array lazylist = perms arr in
    let result : int array list = ltake perms_stream 3 in
    print result
  in
  ()
  ; perms [| 1; 2; 3 |]
  ; [%expect {| ((1 2 3) (1 3 2) (2 1 3)) |}]
;;

(* EXERCISES *)

(* 1. Write a function to generate all the unordered combinations of items from
      a list.

      For example, for the list [1;2;3], the resulte, whose order is not
      important, might be:

      [[]; [1]; [2]; [3]; [1;2]; [1;3]; [2;3]; [1;2;3]]
*)

(* I had already solved this problem, via working on the 99 problems exercises.

   This one is still a little crazy to me.
   To visualize things, refer to the last test case + the `./extra/chapter10/subsets.dot` file.

   When we are taking the left branch, we always return the value
   of our current "set", because we always append an empty list.

   When we are taking the right branch, we take the whole left branch of the
   current "set", and append the current value.

   left,  left           -->   [] @    [] @    []           ==>   []
   left,  right, left    -->   [] @ ['b'] @    []           ==>   ['b']
   left,  right, right   -->   [] @ ['b'] @ ['c']           ==>   ['b'; 'c']
   right, right, right   -->   [] @ ['a'] @ ['b'] @ ['c']   ==>   ['a'; 'b'; 'c']


   I'm not 100% sure about the following representation:

   right, right, right
     ==> List.map (List.cons 'a') @@ List.map (List.cons 'b') @@ List.map (List.cons 'c') [[]];;
     ==> [['a'; 'b'; 'c']]

   right, right, left
     ==> List.map (List.cons 'a') @@ List.map (List.cons 'b') @@ [[]];;
     ==> [['a'; 'b']]
*)
let rec subsets : 'a list -> 'a list list = function
  | [] -> [ [] ]
  | h :: t ->
      let left = subsets t in
      let right = List.map (fun set -> h :: set) left in
      left @ right
;;

let subsets_sorted lst =
  subsets lst
  |> List.sort compare
  |> List.sort (fun a b -> List.length a - List.length b)
;;

let%expect_test _ =
  let print lst =
    let open Core in
    print_s [%sexp (lst : int list list)]
  in
  let subsets_ lst = print @@ subsets_sorted lst in
  ()
  ; subsets_ []
  ; [%expect {| (()) |}]
  ; ()
  ; subsets_ [ 1 ]
  ; [%expect {| (() (1)) |}]
  ; ()
  ; subsets_ [ 1; 2 ]
  ; [%expect {| (() (1) (2) (1 2)) |}]
  ; ()
  ; subsets_ [ 1; 2; 3 ]
  ; [%expect {| (() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)) |}]
  ; ()
  ; Core.(print_s [%sexp (subsets [ 'a'; 'b'; 'c' ] : char list list)])
  ; [%expect {| (() (c) (b) (b c) (a) (a c) (a b) (a b c)) |}]
;;

(* 2. Generate all the "permicombinations" — that is all the permutations of
      all the combinations of a list.

      For example, for the list [1;2;3] the result might be:

      [[]; [1]; [2]; [3]; [1;2]; [2;1]; [1;3]; [3;1]; [2;3]; [3;2]; [1;2;3]]
*)

let rec perms = function
  | [] -> [ [] ]
  | xs ->
      List.concat
      @@ List.map
           (fun x -> List.map
                       (fun xs' -> x :: xs')
                       (perms (without x xs)))
           xs
[@@ocamlformat "disable"]

let permicombinations (lst : int list) : int list list =
  (lst |> subsets |> List.rev |> List.tl |> List.concat_map perms |> List.rev)
  @ [ lst ]
;;

let%expect_test _ =
  ()
  ; Core.(print_s [%sexp (permicombinations [ 1; 2; 3 ] : int list list)])
  ; [%expect {| (() (3) (2) (3 2) (2 3) (1) (3 1) (1 3) (2 1) (1 2) (1 2 3)) |}]
;;

(* Leaving out questions 3, 4 and 5 for now...
   I don't think I'll get much out of them.
*)
