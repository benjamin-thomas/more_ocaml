(*
  Considering `fold_left`'s signature: `('a -> 'b -> 'a) -> 'a -> 'b list -> 'a`

  Here, `( + )` has both `'a` and `'b` are `int`.
  `0` is the **identity element** for `( + )`
*)

(*
   Summing numbers
   ================
*)

let%test _ = 6 = List.fold_left ( + ) 0 [ 1; 2; 3 ]
let%test _ = 6 = List.fold_left ( + ) 1 [ 2; 3 ]
let%test _ = 6 = List.fold_left ( + ) 3 [ 3 ]
let%test _ = 6 = List.fold_left ( + ) 6 []

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
