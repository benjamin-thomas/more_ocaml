(* This is the method used in the book. Imperative style. *)

type input =
  { len    : int
  ; cursor : unit -> int
  ; seek   : int  -> unit
  ; next   : unit -> int
  }
[@@ocamlformat "disable"]

let step_back (inp : input) = inp.seek (inp.cursor () - 1)

let make_input (arr : 'a array) =
  let cursor = ref 0 in
  let len = Array.length arr in
  { len
  ; cursor = (fun () -> !cursor)
  ; seek =
      (fun i ->
        ()
        ; if i < 0 then raise (Invalid_argument "seek before start")
        ; if i > len then raise (Invalid_argument "seek after end")
        ; cursor := i)
  ; next =
      (fun () ->
        if !cursor < len then (
          let curr = arr.(!cursor) in
          ()
          ; incr cursor
          ; curr
        ) else
          raise End_of_file)
  }
;;

let get_different (inp : input) =
  let rec aux acc len =
    try
      let n = inp.next () in
      if n <> List.hd acc then
        aux (n :: acc) (len + 1)
      else (
        ()
        ; step_back inp
        ; step_back inp
        ; List.rev (List.tl acc)
      )
    with
    | End_of_file -> List.rev acc
  in
  aux [ inp.next () ] 1
;;

let%expect_test _ =
  let print f =
    let open Core in
    print_s [%sexp (f : int list)]
  in
  let inp = make_input [| 1; 2; 3; 4 |] in
  ()
  ; print @@ get_different inp
  ; ()
  ; [%expect {| (1 2 3 4) |}]
  ; ()
  ; let inp = make_input [| 1; 2; 3; 3 |] in
    ()
    ; print @@ get_different inp
    ; print @@ get_different inp
    ; print @@ get_different inp
    ; ()
    ; [%expect {|
      (1 2)
      ()
      () |}]
;;

let get_same (inp : input) =
  let rec get n len =
    try
      if inp.next () = n then
        get n (len + 1)
      else (
        ()
        ; step_back inp
        ; len
      )
    with
    | End_of_file -> len
  in
  let n = inp.next () in
  let cnt = get n 1 in
  List.init cnt (fun _ -> n)
;;

let%expect_test _ =
  let print f =
    let open Core in
    print_s [%sexp (f : int list)]
  in
  let inp = make_input [| 1; 1; 1; 2 |] in
  ()
  ; print @@ get_same inp
  ; ()
  ; [%expect {| (1 1 1) |}]
  ; ()
  ; let inp = make_input [| 1; 2; 3; 3; 3 |] in
    ()
    ; print @@ get_same inp
    ; print @@ get_same inp
    ; print @@ get_same inp
    ; ()
    ; [%expect {|
      (1)
      (2)
      (3 3 3) |}]
;;

let consume (inp : input) =
  let stack = Stack.create () in
  let rec aux f1 f2 =
    let res = f1 inp in
    if res = [] then (
      ()
      ; aux f2 f1
    ) else (
      ()
      ; Stack.push res stack
      ; aux get_different get_same
    )
  in
  try aux get_different get_same with
  | End_of_file -> Stack.fold (fun acc i -> i :: acc) [] stack
;;

let%expect_test _ =
  let print f =
    let open Core in
    print_s [%sexp (f : int list list)]
  in
  ()
  ; print @@ consume @@ make_input [| 1; 2; 3; 4 |]
  ; ()
  ; [%expect {| ((1 2 3 4)) |}]
  ; print @@ consume @@ make_input [| 1; 2; 3; 4; 4 |]
  ; ()
  ; [%expect {| ((1 2 3) (4 4)) |}]
  ; print @@ consume @@ make_input [| 1; 2; 3; 3; 4 |]
  ; ()
  ; [%expect {| ((1 2) (3 3) (4)) |}]
  ; print @@ consume @@ make_input [| 1; 2; 2; 3; 4; 4 |]
  ; ()
  ; [%expect {| ((1) (2 2) (3) (4 4)) |}]
  ; print @@ consume @@ make_input [| 1; 1; 2; 3; 4; 4 |]
  ; ()
  ; [%expect {| ((1 1) (2 3) (4 4)) |}]
  ; print @@ consume @@ make_input [| 1; 2; 2; 3; 3; 3; 4 |]
  ; [%expect {| ((1) (2 2) (3 3 3) (4)) |}]
;;
