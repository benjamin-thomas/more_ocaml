(*
  Considering `fold_left`'s signature: `('a -> 'b -> 'a) -> 'a -> 'b list -> 'a`

  Here, `( + )` has both `'a` and `'b` are `int`.
  `0` is the **identity element** for `( + )`
*)

(* Function definitions, for ref. *)

let rec fold_left (fn : 'a -> 'b -> 'a) (acc : 'a) (lst : 'b list) : 'a =
  match lst with
  | [] -> acc
  | x :: xs -> fold_left fn (fn acc x) xs
  [@@warning "-32"]
;;

let rec fold_right (fn : 'a -> 'b -> 'b) (lst : 'a list) (acc : 'b) : 'b =
  match lst with
  | [] -> acc
  | x :: xs -> fn x (fold_right fn xs acc)
  [@@warning "-32"]
;;

(* FOLD_LEFT (tail-recursive)
 * **************************
 *)

(*
   Summing numbers
   ================
*)

let%test _ = 6 = List.fold_left ( + ) 0 [ 1; 2; 3 ]
let%test _ = 6 = List.fold_left ( + ) 1 [ 2; 3 ]
let%test _ = 6 = List.fold_left ( + ) 3 [ 3 ]
let%test _ = 6 = List.fold_left ( + ) 6 []
let%test _ = 6 = 6

let sum = List.fold_left ( + ) 0

let%test _ = 6 = sum [ 1; 2; 3 ]
(*
   Find max number in a list
   =========================
*)
let%test _ = 6 = List.fold_left max min_int [ 2; 4; 6; 0; 1 ]
let%test _ = 6 = List.fold_left max 2 [ 4; 6; 0; 1 ]
let%test _ = 6 = List.fold_left max 4 [ 6; 0; 1 ]
let%test _ = 6 = List.fold_left max 6 [ 0; 1 ]
let%test _ = 6 = List.fold_left max 6 [ 1 ]
let%test _ = 6 = List.fold_left max 6 []

let max_num_int = function
  | [] -> raise @@ Invalid_argument "empty list"
  | lst -> List.fold_left Int.max min_int lst
;;

let max_num_float = function
  | [] -> raise @@ Invalid_argument "empty list"
  | lst -> List.fold_left Float.max min_float lst
;;

let%test _ = 6 = max_num_int [ 2; 4; 6; 0; 1 ]
let%test _ = 6.0 = max_num_float [ 2.0; 4.0; 6.0; 0.0; 1.0 ]

(*
  START off-track: make the function generic
*)

(* v1: delay the type specificity by passing the type specific parameter *)
let max_of_nums_v1 init = function
  | [] -> raise @@ Invalid_argument "empty list"
  | lst -> List.fold_left max init lst
;;

let%test _ = 6 = max_of_nums_v1 min_int [ 2; 4; 6; 0; 1 ]
let%test _ = 6.0 = max_of_nums_v1 min_float [ 2.0; 4.0; 6.0; 0.0; 1.0 ]

type number = Int of int | Float of float
type numbers = Ints of int list | Floats of float list

(* v2: use a custom type *)
let max_of_nums_v2 (lst : numbers) : number =
  match lst with
  | Ints []
  | Floats [] ->
      raise @@ Invalid_argument "empty list"
  | Ints lst -> Int (List.fold_left Int.max min_int lst)
  | Floats lst -> Float (List.fold_left Float.max min_float lst)
;;

let%test _ = Int 6 = max_of_nums_v2 (Ints [ 2; 4; 6; 0; 1 ])
let%test _ = Float 6.0 = max_of_nums_v2 (Floats [ 2.0; 4.0; 6.0; 0.0; 1.0 ])

module type NUMBER = sig
  type t

  val min : t
  val max : t -> t -> t
end

module Int' : NUMBER with type t = int = struct
  type t = int

  let min = min_int
  let max = Int.max
end

module Float' : NUMBER with type t = float = struct
  type t = float

  let min = min_float
  let max = Float.max
end

(* v3 : use a functor *)
let max_of_nums_v3 (type a) (module M : NUMBER with type t = a) = function
  | [] -> raise @@ Invalid_argument "empty list"
  | lst -> List.fold_left M.max M.min lst
;;

let%test _ = 6 = max_of_nums_v3 (module Int') [ 2; 4; 6; 0; 1 ]
let%test _ = 6.0 = max_of_nums_v3 (module Float') [ 2.0; 4.0; 6.0; 0.0; 1.0 ]

(*
  END off-track -> make the function generic
*)

let all = List.fold_left ( && ) true

(* NOTE: calling on an empty list probably doesn't make much sens *)
let%test _ = true = all []
let%test _ = true = all [ true; true; true ]
let%test _ = false = all [ true; false; true ]

let any = List.fold_left ( || ) false

(* NOTE: calling on an empty list probably doesn't make much sens *)
let%test _ = false = any []
let%test _ = true = any [ true; false ]
let%test _ = true = any [ false; true ]
let%test _ = false = any [ false; false ]

(* Transform a list into a set *)
let setify =
  let prepend_once acc elem =
    if List.mem elem acc then
      acc
    else
      elem :: acc
  in
  List.fold_left prepend_once []
;;

let%test _ = [ 3; 2; 1 ] = setify [ 1; 2; 2; 3; 3; 3 ]

(* FOLD_RIGHT (not tail-recursive)
 * *******************************
 *
 * Contrary to `fold_left`, which applies the given function over the list's
 * elements from the left, `fold_right` processes them from the right.
 *)

(*
   Summing numbers
   ================
*)
[@@@ocamlformat "disable"]

let%test _ = 6 = List.fold_right ( + ) [ 1; 2; 3 ] 0
let%test _ = 6 = ( + ) 1 (List.fold_right ( + ) [ 2; 3 ] 0)
let%test _ = 6 = ( + ) 1 (( + ) 2 (List.fold_right ( + ) [ 3 ] 0))
let%test _ = 6 = ( + ) 1 (( + ) 2 (( + ) 3 (List.fold_right ( + ) [ ] 0)))
let%test _ = 6 = ( + ) 1 (( + ) 2 (( + ) 3 0))
let%test _ = 6 = ( + ) 1 (( + ) 2 3)
let%test _ = 6 = ( + ) 1 5
let%test _ = 6 = 6

[@@@ocamlformat "enable"]

(* `map` can be defined with `fold_right` *)

let map fn lst = List.fold_right (fun elem acc -> fn elem :: acc) lst []

let%test _ = [ 2; 4; 6 ] = map (( * ) 2) [ 1; 2; 3 ]

(* `fold_right` can be made tail-recursive by using `fold_left` with the added cost of a list reverse *)

let fold_right_tr fn lst init =
  List.fold_left (fun acc elem -> fn elem acc) init (List.rev lst)
;;

let map_tr fn lst = fold_right_tr (fun elem acc -> fn elem :: acc) lst []

let%test _ = [ 2; 4; 6 ] = map_tr (( * ) 2) [ 1; 2; 3 ]

let map fn lst =
  List.rev lst |> List.fold_left (fun acc elem -> fn elem :: acc) []
;;

let%test _ = [ 2; 4; 6 ] = map (( * ) 2) [ 1; 2; 3 ]

(* init = [] = identity function for list type *)
let copy lst = List.fold_right (fun elem acc -> elem :: acc) lst []

let%test _ = [ 1; 2; 3 ] = copy [ 1; 2; 3 ]

(* init = ys *)
let append xs ys = List.fold_right (fun x acc -> x :: acc) xs ys

let%test _ = [ 1; 2; 3 ] = [ 1 ] @ [ 2; 3 ]
let%test _ = [ 1; 2; 3 ] = append [ 1 ] [ 2; 3 ]

let undict lst =
  List.fold_right (fun (x, y) (xs, ys) -> (x :: xs, y :: ys)) lst ([], [])
;;

let%test _ = ([ 1; 2 ], [ "one"; "two" ]) = undict [ (1, "one"); (2, "two") ]

(* Warning, inefficiency below: we process the list again and again due to the nature of `@` *)
let concat = List.fold_left ( @ ) []

[@@@ocamlformat "disable"]

let%test _ = [ 1; 2; 3; 4; 5 ] = concat [ [ 1; 2 ]; [ 3 ]; [ 4; 5 ] ]

let%test _ = [ 1; 2; 3; 4; 5 ] = List.fold_left ( @ ) [] [ [ 1; 2 ]; [ 3 ]; [ 4; 5 ] ]

let%test _ = [ 1; 2; 3; 4; 5 ] = List.fold_left ( @ ) [1; 2] [ [ 3 ]; [ 4; 5 ] ]
let%test _ = [ 1; 2; 3; 4; 5 ] = List.fold_left ( @ ) [1; 2; 3] [ [ 4; 5 ] ]
let%test _ = [ 1; 2; 3; 4; 5 ] = List.fold_left ( @ ) [1; 2; 3; 4; 5] []
let%test _ = [ 1; 2; 3; 4; 5 ] = [1; 2; 3; 4; 5]

[@@@ocamlformat "enable"]

(* Folding over trees *)

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let my_tree =
  Br ( "1"
     , Br ( "0"
          , Lf
          , Lf
          )
     , Br ( "6"
          , Br ( "4"
               , Lf
               , Lf
               )
          , Lf
          )
     )
  [@@ocamlformat "disable"]

let rec fold_tree fn acc = function
  | Lf -> acc
  | Br (x, l, r) -> fn x (fold_tree fn acc l) (fold_tree fn acc r)
;;

(* [tree_size] returns the number of "nodes" in a tree *)
let tree_size = fold_tree (fun _ size_l size_r -> 1 + size_l + size_r) 0

let%test _ = 4 = tree_size my_tree

let sum_tree = fold_tree (fun x l r -> int_of_string x + l + r) 0

let%test _ = 11 = sum_tree my_tree

let tree_pre_order = fold_tree (fun x l r -> [ x ] @ l @ r) []
let tree_in_order = fold_tree (fun x l r -> l @ [ x ] @ r) []
let tree_post_order = fold_tree (fun x l r -> l @ r @ [ x ]) []

let%test _ = [ "1"; "0"; "6"; "4" ] = tree_pre_order my_tree
let%test _ = [ "0"; "1"; "4"; "6" ] = tree_in_order my_tree
let%test _ = [ "0"; "4"; "6"; "1" ] = tree_post_order my_tree

(* QUESTIONS *)

(*
  1. Write a function which, given a list of integers representing expenses,
     removes them from a budget, again represented by an integer.
*)

let deduct total = List.fold_left ( - ) total

let%test _ = 4 = deduct 10 [ 1; 2; 3 ]

(*
  2. Calculate the length of a list using one of the fold functions
*)

let len =
  let inc acc _ = acc + 1 in
  List.fold_left inc 0
;;

let%test _ = 0 = len []
let%test _ = 1 = len [ 1 ]
let%test _ = 2 = len [ 1; 2 ]
let%test _ = 3 = len [ 1; 2; 3 ]

(*
  3. Use one of the fold functions to find the last element of a list, if any.
     Behave sensibly if the list is empty.
*)

let last_elem = function
  | [] -> raise @@ Invalid_argument "empty list"
  | h :: t -> List.fold_left (fun _ x -> x) h t
;;

let%test _ = 1 = last_elem [ 1 ]
let%test _ = 2 = last_elem [ 1; 2 ]
let%test _ = 3 = last_elem [ 1; 2; 3 ]

(*
  4. Write a function to reverse a list, using one of the fold functions
*)

let rev lst = List.fold_left (fun acc x -> x :: acc) [] lst

let%test _ = [ 3; 2; 1 ] = rev [ 1; 2; 3 ]

(*
  5. Write a version of `List.mem` using one of the fold functions.
     Now `setify` can be defined entirely using folds.
 *)

let mem x = List.fold_left (fun acc x' -> acc || x = x') false

let%test _ = mem 1 [ 1; 2; 3 ]
let%test _ = mem 2 [ 1; 2; 3 ]
let%test _ = mem 3 [ 1; 2; 3 ]
let%test _ = not @@ mem 4 [ 1; 2; 3 ]

let setify lst =
  let prepend_once acc x =
    if mem x acc then
      acc
    else
      x :: acc
  in
  List.fold_left prepend_once [] (List.rev lst)
;;

let%test _ = [ 1; 2; 3 ] = setify [ 1; 2; 2; 3; 3; 3 ]

(*
  6. Use a fold to write a function which, given a list of non-empty strings
     representing words, returns a single string where the words are separated
     by spaces. Comment on its efficiency.
*)

(*
  Appending to a string in a loop is "slow".
  See string builder version below.
*)
let join_space = function
  | [] -> ""
  | h :: t -> List.fold_left (fun acc elem -> acc ^ " " ^ elem) h t
;;

let%test _ = "Hello World" = join_space [ "Hello"; "World" ]

let join_space_sb = function
  | [] -> ""
  | h :: t ->
      let buf =
        let b = Buffer.create 16 in
        Buffer.add_string b h
        ; b
      in
      let res =
        List.fold_left
          (fun acc elem ->
            Buffer.add_string acc " "
            ; Buffer.add_string acc elem
            ; acc)
          buf t
      in
      Buffer.contents res
;;

let%test _ = "Hello World" = join_space_sb [ "Hello"; "World" ]

(*
  7. Use `fold_tree` to write a function which calculates the maximum depth of a tree.

     What is its type? => 'a tree -> int`
*)

let depth tree = fold_tree (fun _ l r -> 1 + max l r) 0 tree

let%test _ = 0 = depth Lf
let%test _ = 3 = depth my_tree

(*
  8. Compare the time efficiency of one or more of your functions with the system implementation
     of the same function.

     Some system functions are optimized in particular ways for performance. Not sure what to add to that.
*)

(*
  9. Comment on whether the use of folds in each of questions 1-7 is good style.

     I suppose this is subjective. Using folds looks nice to me. But OCaml's recursion construct is pretty
     straight forword, so I wouldn't refrain from using it when I'd need to validate the first inputs in a
     particular way, for instance, etc.
*)
