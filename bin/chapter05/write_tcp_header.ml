open More_ocaml.Chapter05

(*
   dune exec -w ./bin/chapter05/write_tcp_header.exe -- ./bin/chapter05/tcp_header.out

   watch "echo --- && xxd ./tcp_header && echo --- && xxd ./tcp_header.out"
 *)

let get_path () = Filename.concat (Sys.getcwd ()) Sys.argv.(1)

let () =
  Out_channel.with_open_bin (get_path ()) @@ fun oc ->
  let output = output_of_channel oc in
  write_tcp_header output
;;
