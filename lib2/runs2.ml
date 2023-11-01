(* This is my second attempt at creating a functional solution *)

type 'a result = { got : 'a; remaining : 'a } [@@deriving sexp]

let get_different lst =
  let rec aux acc rest =
    match rest with
    | [] -> { got = List.rev acc; remaining = rest }
    | [ x ] -> { got = List.rev (x :: acc); remaining = [] }
    | [ a; b ] ->
        if a = b then
          { got = List.rev acc; remaining = rest }
        else
          { got = List.rev (b :: a :: acc); remaining = [] }
    | a :: b :: c :: t ->
        if a = b then
          { got = List.rev acc; remaining = rest }
        else if b == c then
          { got = List.rev (a :: acc); remaining = c :: b :: t }
        else
          aux (b :: a :: acc) (c :: t)
  in
  aux [] lst
;;

let%expect_test _ =
  let print f =
    let open Core in
    print_s [%sexp (f : int list result)]
  in
  ()
  ; print @@ get_different []
  ; [%expect {| ((got ()) (remaining ())) |}]
  ; ()
  ; print @@ get_different [ 1 ]
  ; [%expect {| ((got (1)) (remaining ())) |}]
  ; ()
  ; print @@ get_different [ 1; 1 ]
  ; [%expect {| ((got ()) (remaining (1 1))) |}]
  ; ()
  ; print @@ get_different [ 1; 2 ]
  ; [%expect {| ((got (1 2)) (remaining ())) |}]
  ; ()
  ; print @@ get_different [ 1; 2; 3; 3 ]
  ; [%expect {| ((got (1 2)) (remaining (3 3))) |}]
  ; ()
  ; print @@ get_different [ 1; 1; 2; 3 ]
  ; [%expect {| ((got ()) (remaining (1 1 2 3))) |}]
  ; ()
  ; print @@ get_different [ 1; 1; 2 ]
  ; [%expect {| ((got ()) (remaining (1 1 2))) |}]
  ; ()
  ; print @@ get_different [ 1; 2; 2 ]
  ; [%expect {| ((got (1)) (remaining (2 2))) |}]
  ; ()
  ; print @@ get_different [ 1; 2; 3; 4; 5; 6 ]
  ; [%expect {| ((got (1 2 3 4 5 6)) (remaining ())) |}]
  ; ()
  ; print @@ get_different [ 1; 2; 3 ]
  ; [%expect {| ((got (1 2 3)) (remaining ())) |}]
  ; ()
  ; print @@ get_different [ 1; 2; 3; 4 ]
  ; [%expect {| ((got (1 2 3 4)) (remaining ())) |}]
;;

let get_same lst =
  let rec aux ref' acc rest =
    match rest with
    | [] -> { got = acc; remaining = [] }
    | [ x ] ->
        if x = ref' then
          { got = x :: acc; remaining = [] }
        else
          { got = acc; remaining = [ x ] }
    | a :: b :: t ->
        if a = ref' && b = ref' then
          aux ref' (a :: b :: acc) t
        else if a = ref' && acc <> [] then
          { got = a :: acc; remaining = b :: t }
        else
          { got = acc; remaining = rest }
  in
  match lst with
  | [] -> { got = []; remaining = [] }
  | h :: t -> aux h [] (h :: t)
;;

let%expect_test _ =
  let print f =
    let open Core in
    print_s [%sexp (f : int list result)]
  in
  ()
  ; print @@ get_same []
  ; [%expect {| ((got ()) (remaining ())) |}]
  ; ()
  ; print @@ get_same [ 1 ]
  ; [%expect {| ((got (1)) (remaining ())) |}]
  ; ()
  ; print @@ get_same [ 1; 1 ]
  ; [%expect {| ((got (1 1)) (remaining ())) |}]
  ; ()
  ; print @@ get_same [ 1; 2 ]
  ; [%expect {| ((got ()) (remaining (1 2))) |}]
  ; ()
  ; print @@ get_same [ 1; 2; 3; 3 ]
  ; [%expect {| ((got ()) (remaining (1 2 3 3))) |}]
  ; ()
  ; print @@ get_same [ 1; 1; 2; 3 ]
  ; [%expect {| ((got (1 1)) (remaining (2 3))) |}]
  ; ()
  ; print @@ get_same [ 1; 1; 2 ]
  ; [%expect {| ((got (1 1)) (remaining (2))) |}]
  ; ()
  ; print @@ get_same [ 1; 2; 2 ]
  ; [%expect {| ((got ()) (remaining (1 2 2))) |}]
  ; ()
  ; print @@ get_same [ 1; 1; 1; 1; 1; 2; 3 ]
  ; [%expect {| ((got (1 1 1 1 1)) (remaining (2 3))) |}]
  ; ()
  ; print @@ get_same [ 1; 1; 1; 1; 1 ]
  ; [%expect {| ((got (1 1 1 1 1)) (remaining ())) |}]
  ; ()
  ; print @@ get_same [ 1; 1; 1 ]
  ; [%expect {| ((got (1 1 1)) (remaining ())) |}]
  ; ()
  ; print @@ get_same [ 1; 1; 1; 1 ]
  ; [%expect {| ((got (1 1 1 1)) (remaining ())) |}]
  ; ()
  ; print @@ get_same [ 1; 1; 2; 2 ]
  ; [%expect {| ((got (1 1)) (remaining (2 2))) |}]
  ; ()
  ; print @@ get_same [ 1; 1; 1; 2; 2; 2 ]
  ; [%expect {| ((got (1 1 1)) (remaining (2 2 2))) |}]
  ; ()
  ; print @@ get_same [ 1; 1; 1; 2 ]
  ; [%expect {| ((got (1 1 1)) (remaining (2))) |}]
;;

let consume (lst : int list) =
  let rec aux acc lst =
    let same = get_same lst in
    if same.got <> [] then
      aux (same.got :: acc) same.remaining
    else
      let diff = get_different same.remaining in
      if diff.got = [] then
        acc
      else
        aux (diff.got :: acc) diff.remaining
  in
  aux [] lst |> List.rev
;;

let%expect_test _ =
  let print f =
    let open Core in
    print_s [%sexp (f : int list list)]
  in
  ()
  ; print @@ consume [ 1 ]
  ; [%expect {| ((1)) |}]
  ; ()
  ; print @@ consume [ 1; 1 ]
  ; [%expect {| ((1 1)) |}]
  ; ()
  ; print @@ consume [ 1; 2 ]
  ; [%expect {| ((1 2)) |}]
  ; ()
  ; print @@ consume [ 1; 1; 1 ]
  ; [%expect {| ((1 1 1)) |}]
  ; ()
  ; print @@ consume [ 1; 2; 3 ]
  ; [%expect {| ((1 2 3)) |}]
  ; ()
  ; print @@ consume [ 1; 1; 1; 1 ]
  ; [%expect {| ((1 1 1 1)) |}]
  ; ()
  ; print @@ consume [ 1; 1; 1; 2; 2; 2 ]
  ; [%expect {| ((1 1 1) (2 2 2)) |}]
  ; ()
  ; print @@ consume [ 1; 2; 3; 4 ]
  ; [%expect {| ((1 2 3 4)) |}]
  ; ()
  ; print @@ consume [ 1; 2; 3; 4; 4 ]
  ; [%expect {| ((1 2 3) (4 4)) |}]
  ; ()
  ; print @@ consume [ 1; 2; 3; 3; 4 ]
  ; [%expect {| ((1 2) (3 3) (4)) |}]
  ; ()
  ; print @@ consume [ 1; 2; 2; 3; 4; 4 ]
  ; [%expect {| ((1) (2 2) (3) (4 4)) |}]
  ; ()
  ; print @@ consume [ 1; 1; 2; 3; 4; 4 ]
  ; [%expect {| ((1 1) (2 3) (4 4)) |}]
  ; ()
  ; print @@ consume [ 1; 2; 2; 3; 3; 3; 4 ]
  ; [%expect {| ((1) (2 2) (3 3 3) (4)) |}]
;;
