open More_ocaml.Chapter04

(* dune exec ./bin/chapter04/cat_first_line.exe -- < /tmp/tmp *)

let () =
  let inp : input = single_line_input_of_channel stdin in
  let str = input_string inp max_int in
  print_endline str
;;
