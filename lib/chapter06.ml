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
 * Implementing the ISO-32000 compression scheme (the PDF standard)
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

module Bit_by_bit_compression = struct
  (*
   * BIT BY BIT COMPRESSION SCHEME
   * Implementing CCITT Group 3 compression scheme
   *)

  (* This string represents binary data, which we will convert in a sec. *)
  let input_data =
  "00000000000000000000000000000000000000000000000000000000000000000000000000000000\
   00000000000000000000000000000000000000000000000000000000000000000000000001000000\
   00000000111111110000000000011111111100000000000000000000000000000000000111100000\
   00000011000000011100000001110000001110000000000000000000000000000000000011000000\
   00000110000000001110000011000000000110000000000000000000000000000000000011000000\
   00001110000000000111000111000000000000000000000000000000000000000000000011000000\
   00001100000000000111000110000000000000000000000000000000000000000000000011000000\
   00001100000000000011001110000000000000000011100000000100111000011100000011000000\
   00011100000000000011001110000000000000001111111000111111111101111110000001000000\
   00011100000000000011101100000000000000001000011000001110001111000111000001000000\
   00011100000000000011101100000000000000000000011000001100000110000011000001000000\
   00011100000000000011001110000000000000000000011000001100000110000011000001000000\
   00001100000000000011001110000000000000000111011000001100000110000011000001000000\
   00001110000000000011000110000000000000011100011000001100000110000011000001000000\
   00001110000000000110000111000000000000011000011000001100000110000011000011000000\
   00000111000000000110000011100000000000011000011000001100000110000011000011100000\
   00000011100000001100000001110000000010010001110000011000001100000110000111000000\
   00000011111111100000000001111111111000111110111000111100011110000111001111100000\
   00000000011100000000000000001111000000001000000000000000000000000000000000000000\
   00000000000000000000000000000000000000000000000000000000000000000000000000000000\
   00000000000000000000000000000000000000000000000000000000000000000000000000000000"
[@@ocamlformat "disable"]

  (* type output =
       { char   : char -> unit
       ; length : unit -> int
       ; rewind : unit -> unit
       }
     [@@ocamlformat "disable"] *)

  type output_bits =
  { output : output
  ; mutable byte : int  (* the current byte being constructed bit by bit *)
  ; mutable bit : int   (* value=7-0, to represent where to add the bit to the current byte.
                           value=-1 if it's time to move to construct next byte. *)
  }
[@@ocamlformat "disable"]

  type input_bits =
  { input        : input  (** input this bitstream is based on *)
  ; mutable byte : int    (** holds the byte just read from [input], otherwise an undefined value *)
  ; mutable bit  : int    (** records the current bit position, expressed in powers of 2 *)
  }
[@@ocamlformat "disable"]

  let flush (ob : output_bits) =
    if ob.bit < 7 then (
      ()
      ; ob.output.byte ob.byte
      ; ob.byte <- 0
      ; ob.bit <- 7
    )
  ;;

  let rec put_bit (ob : output_bits) is_set =
    if ob.bit = -1 then (
      ()
      ; flush ob
      ; put_bit ob is_set
    ) else (
      if is_set then ob.byte <- ob.byte lor (1 lsl ob.bit)
      ; ()
      ; ob.bit <- ob.bit - 1
    )
  ;;

  let make_output_bits output = { output; byte = 0; bit = 7 }
  let make_input_bits input = { input; byte = 0; bit = 0 }

  (*
  * We need to be able to read from any input, and write to any output.
  * Since we don't know the size of the input or the output, we use [Buffer.t].
  *)
  let output_of_buffer (b : Buffer.t) : output =
{ byte = Buffer.add_uint8 b
; length = fun () -> Buffer.length b
}
[@@ocamlformat "disable"]

  (** Builds a string containing 0s and 1s
   * Bit_by_bit_compression.(packed_string_of_string input_data);;
   *)
  let packed_string_of_string str : string =
    let buf = Buffer.create ((String.length str / 8) + 1) in
    let out = make_output_bits (output_of_buffer buf) in
    ()
    ; for x = 0 to String.length str - 1 do
        put_bit out (str.[x] = '1')
      done
    ; flush out
    ; Buffer.contents buf
  ;;

  let rec is_bit_set (x : input_bits) =
    if x.bit = 0 then (
      ()
      ; x.byte <- x.input.byte ()
      ; x.bit <- 128
      ; is_bit_set x
    ) else
      let bool = x.byte land x.bit > 0 in
      ()
      ; x.bit <- x.bit / 2
      ; bool
  ;;

  let get_val (ib : input_bits) (n : int) : int =
    if n <= 0 || n > 32 then
      raise (Invalid_argument "get_val")
    else
      let r = ref 0 in
      for i = n - 1 downto 0 do
        let bv =
          if is_bit_set ib then
            1
          else
            0
        in
        r := !r lor (bv lsl i)
      done
      ; !r
  ;;

  let get_bit_val (ib : input_bits) : int = get_val ib 1

  (** Outputs to the width of an image
    * Bit_by_bit_compression.(packed_string_of_string input_data |> print_packed_string 80);;
    *)
  let print_packed_string width str =
    let inp_bits = make_input_bits (input_of_string str) in
    try
      while true do
        for col = 1 to width do
          ()
          ; print_int (get_bit_val inp_bits)
          ; if col = width then print_newline ()
        done
      done
    with
    | End_of_file -> ()
  ;;

  (**
    * We will use arrays for direct indexing.
    * But lists for each element since we do not need random access to each bit.
    *)
  let white_terminating_codes =
    [| [ 0; 0; 1; 1; 0; 1; 0; 1 ]
     ; [ 0; 0; 0; 1; 1; 1 ]
     ; [ 0; 1; 1; 1 ]
     ; [ 1; 0; 0; 0 ]
     ; [ 1; 0; 1; 1 ]
     ; [ 1; 1; 0; 0 ]
     ; [ 1; 1; 1; 0 ]
     ; [ 1; 1; 1; 1 ]
     ; [ 1; 0; 0; 1; 1 ]
     ; [ 1; 0; 1; 0; 0 ]
     ; [ 0; 0; 1; 1; 1 ]
     ; [ 0; 1; 0; 0; 0 ]
     ; [ 0; 0; 1; 0; 0; 0 ]
     ; [ 0; 0; 0; 0; 1; 1 ]
     ; [ 1; 1; 0; 1; 0; 0 ]
     ; [ 1; 1; 0; 1; 0; 1 ]
     ; [ 1; 0; 1; 0; 1; 0 ]
     ; [ 1; 0; 1; 0; 1; 1 ]
     ; [ 0; 1; 0; 0; 1; 1; 1 ]
     ; [ 0; 0; 0; 1; 1; 0; 0 ]
     ; [ 0; 0; 0; 1; 0; 0; 0 ]
     ; [ 0; 0; 1; 0; 1; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 1; 1 ]
     ; [ 0; 0; 0; 0; 1; 0; 0 ]
     ; [ 0; 1; 0; 1; 0; 0; 0 ]
     ; [ 0; 1; 0; 1; 0; 1; 1 ]
     ; [ 0; 0; 1; 0; 0; 1; 1 ]
     ; [ 0; 1; 0; 0; 1; 0; 0 ]
     ; [ 0; 0; 1; 1; 0; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1 ]
     ; [ 0; 0; 0; 1; 1; 0; 1; 0 ]
     ; [ 0; 0; 0; 1; 1; 0; 1; 1 ]
     ; [ 0; 0; 0; 1; 0; 0; 1; 0 ]
     ; [ 0; 0; 0; 1; 0; 0; 1; 1 ]
     ; [ 0; 0; 0; 1; 0; 1; 0; 0 ]
     ; [ 0; 0; 0; 1; 0; 1; 0; 1 ]
     ; [ 0; 0; 0; 1; 0; 1; 1; 0 ]
     ; [ 0; 0; 0; 1; 0; 1; 1; 1 ]
     ; [ 0; 0; 1; 0; 1; 0; 0; 0 ]
     ; [ 0; 0; 1; 0; 1; 0; 0; 1 ]
     ; [ 0; 0; 1; 0; 1; 0; 1; 0 ]
     ; [ 0; 0; 1; 0; 1; 0; 1; 1 ]
     ; [ 0; 0; 1; 0; 1; 1; 0; 0 ]
     ; [ 0; 0; 1; 0; 1; 1; 0; 1 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 1 ]
     ; [ 0; 0; 0; 0; 1; 0; 1; 0 ]
     ; [ 0; 0; 0; 0; 1; 0; 1; 1 ]
     ; [ 0; 1; 0; 1; 0; 0; 1; 0 ]
     ; [ 0; 1; 0; 1; 0; 0; 1; 1 ]
     ; [ 0; 1; 0; 1; 0; 1; 0; 0 ]
     ; [ 0; 1; 0; 1; 0; 1; 0; 1 ]
     ; [ 0; 0; 1; 0; 0; 1; 0; 0 ]
     ; [ 0; 0; 1; 0; 0; 1; 0; 1 ]
     ; [ 0; 1; 0; 1; 1; 0; 0; 0 ]
     ; [ 0; 1; 0; 1; 1; 0; 0; 1 ]
     ; [ 0; 1; 0; 1; 1; 0; 1; 0 ]
     ; [ 0; 1; 0; 1; 1; 0; 1; 1 ]
     ; [ 0; 1; 0; 0; 1; 0; 1; 0 ]
     ; [ 0; 1; 0; 0; 1; 0; 1; 1 ]
     ; [ 0; 0; 1; 1; 0; 0; 1; 0 ]
     ; [ 0; 0; 1; 1; 0; 0; 1; 1 ]
     ; [ 0; 0; 1; 1; 0; 1; 0; 0 ]
    |]
  ;;

  let black_terminating_codes =
    [| [ 0; 0; 0; 0; 1; 1; 0; 1; 1; 1 ]
     ; [ 0; 1; 0 ]
     ; [ 1; 1 ]
     ; [ 1; 0 ]
     ; [ 0; 1; 1 ]
     ; [ 0; 0; 1; 1 ]
     ; [ 0; 0; 1; 0 ]
     ; [ 0; 0; 0; 1; 1 ]
     ; [ 0; 0; 0; 1; 0; 1 ]
     ; [ 0; 0; 0; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 1; 0; 1 ]
     ; [ 0; 0; 0; 0; 1; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 1; 1 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 1; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 1; 1; 0; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 0; 0 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 0; 1; 1; 1 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 0 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 1; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 1; 0 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 1; 1 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 0; 1; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 0; 1; 1; 0; 1 ]
     ; [ 0; 0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 1 ]
     ; [ 0; 0; 0; 0; 0; 1; 1; 0; 1; 0; 1; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 1; 0; 1; 0; 1; 1 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 1; 0 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 1; 1 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 1; 0; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 1; 0; 1; 0; 1 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 1; 0; 1; 1; 0 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 1; 0; 1; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 1 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 1; 0 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 0; 1 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 1; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 1 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 1; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 0; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 1; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 1; 0 ]
     ; [ 0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 1; 1 ]
    |]
  ;;

  let white_make_up_codes =
    [| [ 1; 1; 0; 1; 1 ]
     ; [ 1; 0; 0; 1; 0 ]
     ; [ 0; 1; 0; 1; 1; 1 ]
     ; [ 0; 1; 1; 0; 1; 1; 1 ]
     ; [ 0; 0; 1; 1; 0; 1; 1; 0 ]
     ; [ 0; 0; 1; 1; 0; 1; 1; 1 ]
     ; [ 0; 1; 1; 0; 0; 1; 0; 0 ]
     ; [ 0; 1; 1; 0; 0; 1; 0; 1 ]
     ; [ 0; 1; 1; 0; 1; 0; 0; 0 ]
     ; [ 0; 1; 1; 0; 0; 1; 1; 1 ]
     ; [ 0; 1; 1; 0; 0; 1; 1; 0; 0 ]
     ; [ 0; 1; 1; 0; 0; 1; 1; 0; 1 ]
     ; [ 0; 1; 1; 0; 1; 0; 0; 1; 0 ]
     ; [ 0; 1; 1; 0; 1; 0; 0; 1; 1 ]
     ; [ 0; 1; 1; 0; 1; 0; 1; 0; 0 ]
     ; [ 0; 1; 1; 0; 1; 0; 1; 0; 1 ]
     ; [ 0; 1; 1; 0; 1; 0; 1; 1; 0 ]
     ; [ 0; 1; 1; 0; 1; 0; 1; 1; 1 ]
     ; [ 0; 1; 1; 0; 1; 1; 0; 0; 0 ]
     ; [ 0; 1; 1; 0; 1; 1; 0; 0; 1 ]
     ; [ 0; 1; 1; 0; 1; 1; 0; 1; 0 ]
     ; [ 0; 1; 1; 0; 1; 1; 0; 1; 1 ]
     ; [ 0; 1; 0; 0; 1; 1; 0; 0; 0 ]
     ; [ 0; 1; 0; 0; 1; 1; 0; 0; 1 ]
     ; [ 0; 1; 0; 0; 1; 1; 0; 1; 0 ]
     ; [ 0; 1; 1; 0; 0; 0 ]
     ; [ 0; 1; 0; 0; 1; 1; 0; 1; 1 ]
    |]
  ;;

  let black_make_up_codes =
    [| [ 0; 0; 0; 0; 0; 0; 1; 1; 1; 1 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 0; 0 ]
     ; [ 0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 0; 1 ]
     ; [ 0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 0; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 1; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 0; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 1; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 1; 0; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 1; 1; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 1; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 1; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 0; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 1; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 1; 1 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 0 ]
     ; [ 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 1 ]
    |]
  ;;

  let rec code is_black length =
    if length > 1791 || length < 0 then
      raise (Invalid_argument "code: bad length")
    else if length > 64 then
      let m =
        if is_black then
          black_make_up_codes.((length / 64) - 1)
        else
          white_make_up_codes.((length / 64) - 1)
      in
      m @ code is_black (length mod 64)
    else if is_black then
      black_terminating_codes.(length)
    else
      white_terminating_codes.(length)
  ;;

  let peek_bit (b : input_bits) =
    if b.bit = 0 then (
      let byte = b.input.byte () in
      ()
      ; rewind b.input
      ; byte land 128 > 0
    ) else
      b.byte land b.bit > 0
  ;;

  let rec read_up_to v i n w =
    if n >= w then
      (n, v)
    else if peek_bit i = v then (
      ()
      ; get_bit_val i |> ignore
      ; read_up_to v i (n + 1) w
    ) else
      (n, v)
  ;;

  let encode_fax inp out w h =
    let rec encode_fax_line inp out w =
      if w > 0 then (
        let n, is_black = read_up_to (peek_bit inp) inp 0 w in
        ()
        ; List.iter (fun n -> put_bit out (n <> 0)) (code is_black n)
        ; encode_fax_line inp out (w - n)
      )
    in
    for _ = 1 to h do
      if peek_bit inp then ()
      ; List.iter (fun n -> put_bit out (n <> 0)) (code false 0)
      ; encode_fax_line inp out w
    done
  ;;

  let process f str w h =
    let buf = Buffer.create (String.length str) in
    let inp = make_input_bits (input_of_string str) in
    let out = make_output_bits (output_of_buffer buf) in
    ()
    ; f inp out w h
    ; flush out
    ; Buffer.contents buf
  ;;

  (* Bit_by_bit_compression.(compress_string_ccitt input_data 80 21);; *)
  let compress_string_ccitt = process encode_fax

  (* This is one damn ugly function!
   * Got it from here:
   * https://github.com/johnwhitington/more-ocaml-exercises/blob/master/examples/Chapter6/examples.ml
   *)
  let rec read_white_code inp =
    let a = get_bit_val inp in
    let b = get_bit_val inp in
    let c = get_bit_val inp in
    let d = get_bit_val inp in
    match (a, b, c, d) with
    | 0, 1, 1, 1 -> 2
    | 1, 0, 0, 0 -> 3
    | 1, 0, 1, 1 -> 4
    | 1, 1, 0, 0 -> 5
    | 1, 1, 1, 0 -> 6
    | 1, 1, 1, 1 -> 7
    | _ -> (
        let e = get_bit_val inp in
        match (a, b, c, d, e) with
        | 1, 0, 0, 1, 1 -> 8
        | 1, 0, 1, 0, 0 -> 9
        | 0, 0, 1, 1, 1 -> 10
        | 0, 1, 0, 0, 0 -> 11
        | 1, 1, 0, 1, 1 -> 64 + read_white_code inp
        | 1, 0, 0, 1, 0 -> 128 + read_white_code inp
        | _ -> (
            let f = get_bit_val inp in
            match (a, b, c, d, e, f) with
            | 0, 0, 0, 1, 1, 1 -> 1
            | 0, 0, 1, 0, 0, 0 -> 12
            | 0, 0, 0, 0, 1, 1 -> 13
            | 1, 1, 0, 1, 0, 0 -> 14
            | 1, 1, 0, 1, 0, 1 -> 15
            | 1, 0, 1, 0, 1, 0 -> 16
            | 1, 0, 1, 0, 1, 1 -> 17
            | 0, 1, 0, 1, 1, 1 -> 192 + read_white_code inp
            | 0, 1, 1, 0, 0, 0 -> 1664 + read_white_code inp
            | _ -> (
                let g = get_bit_val inp in
                match (a, b, c, d, e, f, g) with
                | 0, 1, 0, 0, 1, 1, 1 -> 18
                | 0, 0, 0, 1, 1, 0, 0 -> 19
                | 0, 0, 0, 1, 0, 0, 0 -> 20
                | 0, 0, 1, 0, 1, 1, 1 -> 21
                | 0, 0, 0, 0, 0, 1, 1 -> 22
                | 0, 0, 0, 0, 1, 0, 0 -> 23
                | 0, 1, 0, 1, 0, 0, 0 -> 24
                | 0, 1, 0, 1, 0, 1, 1 -> 25
                | 0, 0, 1, 0, 0, 1, 1 -> 26
                | 0, 1, 0, 0, 1, 0, 0 -> 27
                | 0, 0, 1, 1, 0, 0, 0 -> 28
                | 0, 1, 1, 0, 1, 1, 1 -> 256 + read_white_code inp
                | _ -> (
                    let h = get_bit_val inp in
                    match (a, b, c, d, e, f, g, h) with
                    | 0, 0, 1, 1, 0, 1, 0, 1 -> 0
                    | 0, 0, 0, 0, 0, 0, 1, 0 -> 29
                    | 0, 0, 0, 0, 0, 0, 1, 1 -> 30
                    | 0, 0, 0, 1, 1, 0, 1, 0 -> 31
                    | 0, 0, 0, 1, 1, 0, 1, 1 -> 32
                    | 0, 0, 0, 1, 0, 0, 1, 0 -> 33
                    | 0, 0, 0, 1, 0, 0, 1, 1 -> 34
                    | 0, 0, 0, 1, 0, 1, 0, 0 -> 35
                    | 0, 0, 0, 1, 0, 1, 0, 1 -> 36
                    | 0, 0, 0, 1, 0, 1, 1, 0 -> 37
                    | 0, 0, 0, 1, 0, 1, 1, 1 -> 38
                    | 0, 0, 1, 0, 1, 0, 0, 0 -> 39
                    | 0, 0, 1, 0, 1, 0, 0, 1 -> 40
                    | 0, 0, 1, 0, 1, 0, 1, 0 -> 41
                    | 0, 0, 1, 0, 1, 0, 1, 1 -> 42
                    | 0, 0, 1, 0, 1, 1, 0, 0 -> 43
                    | 0, 0, 1, 0, 1, 1, 0, 1 -> 44
                    | 0, 0, 0, 0, 0, 1, 0, 0 -> 45
                    | 0, 0, 0, 0, 0, 1, 0, 1 -> 46
                    | 0, 0, 0, 0, 1, 0, 1, 0 -> 47
                    | 0, 0, 0, 0, 1, 0, 1, 1 -> 48
                    | 0, 1, 0, 1, 0, 0, 1, 0 -> 49
                    | 0, 1, 0, 1, 0, 0, 1, 1 -> 50
                    | 0, 1, 0, 1, 0, 1, 0, 0 -> 51
                    | 0, 1, 0, 1, 0, 1, 0, 1 -> 52
                    | 0, 0, 1, 0, 0, 1, 0, 0 -> 53
                    | 0, 0, 1, 0, 0, 1, 0, 1 -> 54
                    | 0, 1, 0, 1, 1, 0, 0, 0 -> 55
                    | 0, 1, 0, 1, 1, 0, 0, 1 -> 56
                    | 0, 1, 0, 1, 1, 0, 1, 0 -> 57
                    | 0, 1, 0, 1, 1, 0, 1, 1 -> 58
                    | 0, 1, 0, 0, 1, 0, 1, 0 -> 59
                    | 0, 1, 0, 0, 1, 0, 1, 1 -> 60
                    | 0, 0, 1, 1, 0, 0, 1, 0 -> 61
                    | 0, 0, 1, 1, 0, 0, 1, 1 -> 62
                    | 0, 0, 1, 1, 0, 1, 0, 0 -> 63
                    | 0, 0, 1, 1, 0, 1, 1, 0 -> 320 + read_white_code inp
                    | 0, 0, 1, 1, 0, 1, 1, 1 -> 384 + read_white_code inp
                    | 0, 1, 1, 0, 0, 1, 0, 0 -> 448 + read_white_code inp
                    | 0, 1, 1, 0, 0, 1, 0, 1 -> 512 + read_white_code inp
                    | 0, 1, 1, 0, 1, 0, 0, 0 -> 576 + read_white_code inp
                    | 0, 1, 1, 0, 0, 1, 1, 1 -> 640 + read_white_code inp
                    | _ -> (
                        let j = get_bit_val inp in
                        match (a, b, c, d, e, f, g, h, j) with
                        | 0, 1, 1, 0, 0, 1, 1, 0, 0 -> 704 + read_white_code inp
                        | 0, 1, 1, 0, 0, 1, 1, 0, 1 -> 768 + read_white_code inp
                        | 0, 1, 1, 0, 1, 0, 0, 1, 0 -> 832 + read_white_code inp
                        | 0, 1, 1, 0, 1, 0, 0, 1, 1 -> 896 + read_white_code inp
                        | 0, 1, 1, 0, 1, 0, 1, 0, 0 -> 960 + read_white_code inp
                        | 0, 1, 1, 0, 1, 0, 1, 0, 1 -> 1024 + read_white_code inp
                        | 0, 1, 1, 0, 1, 0, 1, 1, 0 -> 1088 + read_white_code inp
                        | 0, 1, 1, 0, 1, 0, 1, 1, 1 -> 1152 + read_white_code inp
                        | 0, 1, 1, 0, 1, 1, 0, 0, 0 -> 1216 + read_white_code inp
                        | 0, 1, 1, 0, 1, 1, 0, 0, 1 -> 1280 + read_white_code inp
                        | 0, 1, 1, 0, 1, 1, 0, 1, 0 -> 1344 + read_white_code inp
                        | 0, 1, 1, 0, 1, 1, 0, 1, 1 -> 1408 + read_white_code inp
                        | 0, 1, 0, 0, 1, 1, 0, 0, 0 -> 1472 + read_white_code inp
                        | 0, 1, 0, 0, 1, 1, 0, 0, 1 -> 1536 + read_white_code inp
                        | 0, 1, 0, 0, 1, 1, 0, 1, 0 -> 1600 + read_white_code inp
                        | 0, 1, 0, 0, 1, 1, 0, 1, 1 -> 1728 + read_white_code inp
                        | _ -> raise (Failure "bad white code"))))))
  [@@ocamlformat "disable"]

  let rec read_black_code inp =
    let a = get_bit_val inp in
    let b = get_bit_val inp in
    match (a, b) with
    | 1, 1 -> 2
    | 1, 0 -> 3
    | _ -> (
        let c = get_bit_val inp in
        match (a, b, c) with
        | 0, 1, 0 -> 1
        | 0, 1, 1 -> 4
        | _ -> (
            let d = get_bit_val inp in
            match (a, b, c, d) with
            | 0, 0, 1, 1 -> 5
            | 0, 0, 1, 0 -> 6
            | _ -> (
                let e = get_bit_val inp in
                match (a, b, c, d, e) with
                | 0, 0, 0, 1, 1 -> 7
                | _ -> (
                    let f = get_bit_val inp in
                    match (a, b, c, d, e, f) with
                    | 0, 0, 0, 1, 0, 1 -> 8
                    | 0, 0, 0, 1, 0, 0 -> 9
                    | _ -> (
                        let g = get_bit_val inp in
                        match (a, b, c, d, e, f, g) with
                        | 0, 0, 0, 0, 1, 0, 0 -> 10
                        | 0, 0, 0, 0, 1, 0, 1 -> 11
                        | 0, 0, 0, 0, 1, 1, 1 -> 12
                        | _ -> (
                            let h = get_bit_val inp in
                            match (a, b, c, d, e, f, g, h) with
                            | 0, 0, 0, 0, 0, 1, 0, 0 -> 13
                            | 0, 0, 0, 0, 0, 1, 1, 1 -> 14
                            | _ -> (
                                let j = get_bit_val inp in
                                match (a, b, c, d, e, f, g, h, j) with
                                | 0, 0, 0, 0, 1, 1, 0, 0, 0 -> 15
                                | _ -> (
                                    let k = get_bit_val inp in
                                    match (a, b, c, d, e, f, g, h, j, k) with
                                    | 0, 0, 0, 0, 1, 1, 0, 1, 1, 1 -> 0
                                    | 0, 0, 0, 0, 0, 1, 0, 1, 1, 1 -> 16
                                    | 0, 0, 0, 0, 0, 1, 1, 0, 0, 0 -> 17
                                    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 -> 18
                                    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 1 ->
                                        64 + read_black_code inp
                                    | _ -> (
                                        let l = get_bit_val inp in
                                        match
                                          (a, b, c, d, e, f, g, h, j, k, l)
                                        with
                                        | 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1 -> 19
                                        | 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0 -> 20
                                        | 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0 -> 21
                                        | 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1 -> 22
                                        | 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0 -> 23
                                        | 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1 -> 24
                                        | 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0 -> 25
                                        | _ -> (
                                            let m = get_bit_val inp in
                                            match ( a, b, c, d, e, f, g, h, j, k, l, m)
                                            with
                                            | 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0 -> 26
                                            | 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1 -> 27
                                            | 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0 -> 28
                                            | 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1 -> 29
                                            | 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0 -> 30
                                            | 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1 -> 31
                                            | 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0 -> 32
                                            | 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1 -> 33
                                            | 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0 -> 34
                                            | 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1 -> 35
                                            | 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0 -> 36
                                            | 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1 -> 37
                                            | 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0 -> 38
                                            | 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1 -> 39
                                            | 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0 -> 40
                                            | 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1 -> 41
                                            | 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0 -> 42
                                            | 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1 -> 43
                                            | 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0 -> 44
                                            | 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1 -> 45
                                            | 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0 -> 46
                                            | 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1 -> 47
                                            | 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0 -> 48
                                            | 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1 -> 49
                                            | 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0 -> 50
                                            | 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1 -> 51
                                            | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0 -> 52
                                            | 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1 -> 53
                                            | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0 -> 54
                                            | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1 -> 55
                                            | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0 -> 56
                                            | 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0 -> 57
                                            | 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1 -> 58
                                            | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1 -> 59
                                            | 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0 -> 60
                                            | 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0 -> 61
                                            | 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0 -> 62
                                            | 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1 -> 63
                                            | 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0 -> 128 + read_black_code inp
                                            | 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1 -> 192 + read_black_code inp
                                            | 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1 -> 256 + read_black_code inp
                                            | 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1 -> 320 + read_black_code inp
                                            | 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0 -> 384 + read_black_code inp
                                            | 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1 -> 448 + read_black_code inp
                                            | _ -> ( let n = get_bit_val inp in match
                                                  ( a, b, c, d, e, f, g, h, j, k, l, m, n)
                                                with
                                                | (0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0) -> 512 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1) -> 576 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0) -> 640 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1) -> 704 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0) -> 768 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1) -> 832 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0) -> 896 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1) -> 960 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0) -> 1024 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1) -> 1088 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0) -> 1152 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1) -> 1216 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0) -> 1280 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1) -> 1344 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0) -> 1408 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1) -> 1472 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0) -> 1536 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1) -> 1600 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0) -> 1664 + read_black_code inp
                                                | (0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1) -> 1728 + read_black_code inp
                                                | _ -> raise (Failure "bad black code")
                                                )))))))))))
  [@@ocamlformat "disable"]

  let decode_fax inp out w h =
    let lines = ref h in
    let pixels = ref w in
    let is_white = ref true in
    while !lines > 0 do
      while !pixels > 0 do
        let n =
          (if !is_white then
             read_white_code
           else
             read_black_code)
            inp
        in
        for _ = 1 to n do
          put_bit out (not !is_white)
        done
        ; pixels := !pixels - n
        ; is_white := not !is_white
      done
      ; is_white := true
      ; pixels := w
      ; lines := !lines - 1
    done
  ;;

  (*
   * This should work:
   * Bit_by_bit_compression.(decompress_string_ccitt (compress_string_ccitt input_data 80 21) 80 21);;
   * But it doesn't, and to be honest it doesn't seem that interesting to me to keep spending time on this.
   * I disliked this chapter.
   *)
  let decompress_string_ccitt = process decode_fax
  (* let%test _ = input_data = (decompress_string_ccitt (compress_string_ccitt input_data 80 21) 80 21) *)
end

(*
  Question 1
  ==========

  How much complexity did using the input and output types add to compress and decompress
  in our byte-by-byte example? Rewrite the functions so they just operate over lists of integers, in
  functional style, and compare the two.

  => See lib2/runs*.ml

  Conclusion
  ==========
  There is a lot of complexity in this seemingly simple problem.
  Both styles necessitated quite a lot of testing to make sure the output is correct.
  Without considering performance implications, I'm not sure one style is better than another in this case.
*)

(*
 * TODO: I think I will return to these questions later, I want to move forward.
 *)

(*
  2. Replace our manual tree of codes with a tree automatically generated from the lists of codes used
  for compression. The tree will have no data at its branches (since no code is a prefix of another), and
  will have data at only some of its leaves. Define a suitable data type first.
*)

(*
  3. What happens if we compress our data as a single line of 1680 bits instead of 21 lines of 80? What
    happens if we try to compress already-compressed data?
*)

(*
  4. Write a function which, given input data, will calculate a histogram of the frequencies of different
  runs of white and black. This could be used to build custom codes for each image, improving
  compression.
*)
