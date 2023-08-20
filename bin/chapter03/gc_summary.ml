(*
  echo ./bin/chapter03/gc_summary.ml | entr -c bash -c 'dune exec ./bin/chapter03/gc_summary.exe && cat /tmp/ocaml-gc'
*)

let output_stats (stat : Gc.stat) oc =
  let output_stat fn k v =
    Out_channel.output_string oc (k ^ "=" ^ fn v ^ "\n")
  in
  let output_float = output_stat string_of_float in
  let output_int = output_stat string_of_int in
  ()
  ; output_float "minor_words" stat.minor_words
  ; output_float "major_words" stat.major_words
  ; output_int "minor_collection" stat.minor_collections
  ; output_int "major_collection" stat.major_collections
;;

let output_sep oc =
  Out_channel.output_string oc "------------------------------------\n"
;;

let output_all stat oc =
  ()
  ; Gc.print_stat oc
  ; output_sep oc
  ; output_stats stat oc
  ; output_sep oc
;;

let () =
  let ctrl = Gc.get () in
  let stat = Gc.stat () in
  ()
  ; Gc.set { ctrl with verbose = 0x01 }
  ; Out_channel.with_open_text "/tmp/ocaml-gc" (output_all stat)
;;
