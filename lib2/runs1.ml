(* This is my first attempt at creating a functional solution *)

(* We temporarily accumulate "same" elements into [acc1].
 * We accumulate the final result into [acc2]
 *)
let group_sames lst =
  let rec aux prev acc1 acc2 = function
    | [] -> List.rev (acc1 :: acc2)
    | curr :: rest ->
        if curr = prev then
          aux curr (curr :: acc1) acc2 rest
        else
          aux curr [ curr ] (List.rev acc1 :: acc2) rest
  in
  match lst with
  | [] -> []
  | h :: t -> aux h [ h ] [] t
;;

let%test_unit _ =
  let ( => ) = [%test_eq: Base.int Base.list Base.list] in
  ()
  ; group_sames []                => []
  ; group_sames [ 1; 2; 3; 4 ]    => [ [ 1 ]; [ 2 ]; [ 3 ]; [ 4 ] ]
  ; group_sames [ 1; 1; 2; 3; 4 ] => [ [ 1; 1 ]; [ 2 ]; [ 3 ]; [ 4 ] ]
  ; group_sames [ 1; 2; 2; 3; 4 ] => [ [ 1 ]; [ 2; 2 ]; [ 3 ]; [ 4 ] ]
  ; group_sames [ 1; 2; 3; 3; 4 ] => [ [ 1 ]; [ 2 ]; [ 3; 3 ]; [ 4 ] ]
  ; group_sames [ 1; 2; 2; 3; 4 ] => [ [ 1 ]; [ 2; 2 ]; [ 3 ]; [ 4 ] ]
  [@@ocamlformat "disable"]

(* We temporarily accumulate "single" element lists into [acc1].
 * We accumulate the final result into [acc2]
 *)
let accum_singles (lst : int list list) =
  let rec aux acc1 acc2 = function
    | [] ->
        if acc1 = [] then
          acc2
        else
          List.rev acc1 :: acc2
    | [ single ] :: t -> aux (single :: acc1) acc2 t
    | h :: t ->
        if acc1 = [] then
          aux [] (h :: acc2) t
        else
          aux [] (h :: List.rev acc1 :: acc2) t
  in
  aux [] [] lst |> List.rev
;;

let%test_unit _ =
  let ( => ) = [%test_eq: Base.int Base.list Base.list] in
  ()
  ; accum_singles [ [ 1 ]; [ 2 ]; [ 3 ]; [ 4 ] ]                 => [ [ 1; 2; 3; 4 ] ]
  ; accum_singles [ [ 1; 1 ]; [ 2 ]; [ 3 ]; [ 4 ] ]              => [ [ 1; 1 ]; [ 2; 3; 4 ] ]
  ; accum_singles [ [ 1 ]; [ 2 ]; [ 3; 3 ]; [ 4 ] ]              => [ [ 1; 2 ]; [ 3; 3 ]; [ 4 ] ]
  ; accum_singles [ [ 1 ]; [ 2; 2 ]; [ 3 ]; [ 4 ] ]              => [ [ 1 ]; [ 2; 2 ]; [ 3; 4 ] ]
  ; accum_singles [ [ 1; 1; 1 ]; [ 2 ]; [ 3 ]; [ 4; 4 ]; [ 5 ] ] => [ [ 1; 1; 1 ]; [ 2; 3 ]; [ 4; 4 ]; [ 5 ] ]
[@@ocamlformat "disable"]

let group_alt_runs lst = lst |> group_sames |> accum_singles

let%test_unit _ =
  let ( => ) = [%test_eq: Base.int Base.list Base.list] in
  ()
  ; group_alt_runs []                   => []
  ; group_alt_runs [ 1; 2; 3; 4 ]       => [ [ 1; 2; 3; 4 ] ]
  ; group_alt_runs [ 1; 1; 2; 3; 4 ]    => [ [ 1; 1 ]; [ 2; 3; 4 ] ]
  ; group_alt_runs [ 1; 2; 3; 4; 4 ]    => [ [ 1; 2; 3 ]; [ 4; 4 ] ]
  ; group_alt_runs [ 1; 2; 3; 3; 4; 5 ] => [ [ 1; 2 ]; [ 3; 3 ]; [ 4; 5 ] ]
  ; group_alt_runs [ 1; 2; 3; 3; 4 ]    => [ [ 1; 2 ]; [ 3; 3 ]; [ 4 ] ]
  ; group_alt_runs [ 1; 2; 2; 3; 4 ]    => [ [ 1 ]; [ 2; 2 ]; [ 3; 4 ] ]
[@@ocamlformat "disable"]
