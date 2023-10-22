(** Utility function for the REPL *)
let string_of_int_list lst =
  let bytes = Bytes.create (List.length lst) in
  ()
  ; List.iteri (fun n c -> Bytes.set bytes n (char_of_int c)) lst
  ; Bytes.to_string bytes
;;

(** Utility function for the REPL *)
let int_list_of_string str =
  let lst = ref [] in
  (* NOTE: we use downto to avoid a list reversal *)
  ()
  ; for i = String.length str - 1 downto 0 do
      lst := int_of_char str.[i] :: !lst
    done
  ; !lst
;;

type output =
  { byte   : int -> unit
  ; length : unit -> int
  }
[@@ocamlformat "disable"]

type input =
  { pos : unit -> int
  ; seek : int -> unit
  ; byte : unit -> int
  ; length : int
  }
[@@ocamlformat "disable"]

(*
 * We need to be able to read from any input, and write to any output.
 * Since we don't know the size of the input or the output, we use [Buffer.t].
 *)
let output_of_buffer (b : Buffer.t) : output =
  { byte = Buffer.add_uint8 b
  ; length = fun () -> Buffer.length b
  }
[@@ocamlformat "disable"]

let input_of_string (s : string) : input =
  let pos = ref 0 in
  let len = String.length s in
  { pos = (fun () -> !pos)
  ; seek =
      (fun i ->
        if i < 0 then
          raise @@ Invalid_argument "seek before start"
        else
          pos := i)
  ; byte =
      (fun () ->
        if !pos >= len then
          raise End_of_file
        else
          let n = String.get_uint8 s !pos in
          ()
          ; pos := !pos + 1
          ; n)
  ; length = len
  }
;;

let process f str : string =
  let buf = Buffer.create (String.length str) in
  ()
  ; f (input_of_string str) (output_of_buffer buf)
  ; Buffer.contents buf
;;

(*

FIXME: try to find something more elegant than manipulating ints for bytes

We have 3 states for the bit pattern:
  10000000 (128) -> end of decoding
  0xxxxxxx       -> different run
  1xxxxxxx       -> same run

See:
{|
    repr = lambda { |n| {n: n, bin: n.to_s(2).rjust(8, "x") } }
    pp 0.upto(255).map {|n| repr.call n};nil
|}
*)
exception EOD

let is_different_run x = x >= 0 && x <= 127
let is_same_run x = x > 128 && x <= 255

let decompress (i : input) (o : output) =
  try
    while true do
      match i.byte () with
      | x when is_different_run x ->
          for _ = 1 to x + 1 do
            o.byte (i.byte ())
          done
      | x when is_same_run x ->
          let c = i.byte () in
          for _ = 1 to 257 - x do
            o.byte c
          done
      | 128 -> raise EOD
      | _ -> failwith "wat"
    done
  with
  | EOD -> ()
;;

let decompress_string = process decompress
let rewind inp = inp.seek (inp.pos () - 1)

let get_same i =
  let rec get ch c =
    if c = 128 then
      128
    else
      try
        if i.byte () = ch then
          get ch (c + 1)
        else (
          ()
          ; rewind i
          ; c
        )
      with
      | End_of_file -> c
  in
  let ch = i.byte () in
  (ch, get ch 1)
;;

let get_different i =
  let rec aux a c =
    if c = 128 then
      List.rev a
    else
      try
        let ch' = i.byte () in
        if ch' <> List.hd a then
          aux (ch' :: a) (c + 1)
        else (
          ()
          ; rewind i
          ; rewind i
          ; List.rev (List.tl a)
        )
      with
      | End_of_file -> List.rev a
  in
  aux [ i.byte () ] 1
;;

let compress (i : input) (o : output) =
  try
    while true do
      match get_same i with
      | _, 1 ->
          ()
          ; rewind i
          ; let cs = get_different i in
            o.byte (List.length cs - 1)
            ; List.iter o.byte cs
      | b, c ->
          o.byte (257 - c)
          ; o.byte b
    done
  with
  | End_of_file -> o.byte 128
;;

let compress_string = process compress

let%expect_test _ =
  let example = "((5.000000, 4.583333), (4.500000,5.000000))" in
  let print (lst : int list) =
    print_string @@ "[ " ^ String.concat "; " (List.map string_of_int lst) ^ " ]"
  in
  ()
  ; print (compress_string example |> int_list_of_string)
  ; [%expect
      "[ 255; 40; 1; 53; 46; 251; 48; 5; 44; 32; 52; 46; 53\
       ; 56; 253; 51; 6; 41; 44; 32; 40; 52; 46; 53; 252; 48\
       ; 2; 44; 53; 46; 251; 48; 255; 41; 128 \
       ]"]
[@@ocamlformat "disable"]

let%test _ =
  let example = "((5.000000, 4.583333), (4.500000,5.000000))" in
  example = (example |> compress_string |> decompress_string)
;;
