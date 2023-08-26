open More_ocaml.Chapter05

(*
   dune exec ./bin/chapter05/print_tcp_header.exe -- ./bin/chapter05/tcp_header
 *)

let get_path () =
  try Filename.concat (Sys.getcwd ()) Sys.argv.(1) with
  | Invalid_argument _ ->
      ()
      ; print_endline "Must provide a PATH"
      ; exit 1
;;

let () =
  In_channel.with_open_bin (get_path ()) @@ fun ic ->
  let input = input_of_channel ic in
  let ib = make_input_bits input in
  print_tcp_header ib
;;
