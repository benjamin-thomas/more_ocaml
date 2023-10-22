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
 * BYTE BY BYTE COMPRESSION SCHEME
 *)

(*

We have 3 states for the bit pattern:
  10000000 (=128) -> end of decoding
  0xxxxxxx (<128) -> different run
  1xxxxxxx (>128) -> same run

Checking against the int's value is not very elegant. I could use a bit mask, like such:

utop # 0b10000000 land 129;;
- : int = 128

But then, what would masking against a big number mean? Or a negative number?
Since we don't have a built-in uint8 type, I'll keep it simple and do as the author said.

TODO: I should also look into the `ocaml-stdint` lib, it provides many integer types.

See:
{|
    repr = lambda { |n| {n: n, bin: n.to_s(2).rjust(8, "x") } }
    pp 0.upto(255).map {|n| repr.call n};nil
|}
*)
exception EOD

let is_bit_set pattern n = pattern land n = pattern

let decompress (inp : input) (out : output) =
  try
    while true do
      match inp.byte () with
      | n when n >= 0 && n <= 127 ->
          for _ = 1 to n + 1 do
            out.byte (inp.byte ())
          done
      | n when n > 128 && n <= 255 ->
          let c = inp.byte () in
          for _ = 1 to 257 - n do
            out.byte c
          done
      | 128 -> raise EOD
      | _ -> failwith "wat"
    done
  with
  | EOD -> ()
;;

let decompress_string = process decompress
let rewind inp = inp.seek (inp.pos () - 1)

let get_same inp =
  let rec get n len =
    if len = 128 then
      128
    else
      try
        if inp.byte () = n then
          get n (len + 1)
        else (
          ()
          ; rewind inp
          ; len
        )
      with
      | End_of_file -> len
  in
  let n = inp.byte () in
  (n, get n 1)
;;

let get_different inp =
  let rec aux acc len =
    if len = 128 then
      List.rev acc
    else
      try
        let n = inp.byte () in
        if n <> List.hd acc then
          aux (n :: acc) (len + 1)
        else (
          ()
          ; rewind inp
          ; rewind inp
          ; List.rev (List.tl acc)
        )
      with
      | End_of_file -> List.rev acc
  in
  aux [ inp.byte () ] 1
;;

let compress (inp : input) (out : output) =
  try
    while true do
      match get_same inp with
      | _, 1 ->
          ()
          ; rewind inp
          ; let ns = get_different inp in
            ()
            ; out.byte (List.length ns - 1)
            ; List.iter out.byte ns
      | n, len ->
          ()
          ; out.byte (257 - len)
          ; out.byte n
    done
  with
  | End_of_file -> out.byte 128
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

(*
 * TODO: BIT BY BIT COMPRESSION SCHEME
 *)
