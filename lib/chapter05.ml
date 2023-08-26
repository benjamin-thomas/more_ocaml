type input =
  { pos : unit -> int
  ; seek : int -> unit
  ; byte : unit -> int
  ; length : int
  }
[@@ocamlformat "disable"]

let input_of_channel (ch : in_channel) : input =
  { pos = (fun () -> pos_in ch)
  ; seek = seek_in ch
  ; byte = (fun () -> input_byte ch)
  ; length = in_channel_length ch
  }
;;

let input_of_int_array arr : input =
  let pos = ref 0 in
  let length = Array.length arr in
  { pos = (fun () -> !pos)
  ; seek =
      (fun i ->
        if i < 0 then
          raise (Invalid_argument "seek before start")
        else
          pos := i)
  ; byte =
      (fun () ->
        if !pos >= length then
          raise End_of_file
        else
          let n = Array.get arr !pos in
          ()
          ; pos := !pos + 1
          ; n)
  ; length
  }
;;

type input_bits =
  { input        : input  (** input this bitstream is based on *)
  ; mutable byte : int    (** holds the byte just read from [input], otherwise an undefined value *)
  ; mutable bit  : int    (** records the current bit position, expressed in powers of 2 *)
  }
[@@ocamlformat "disable"]

(*

   0     1     2     3     4     5     6     7
+ --- + --- + --- + --- + --- + --- + --- + --- + ------------------ +
|  0  |  1  |  1  |  1  |  0  |  1  |  1  |  0  | input data         | (for byte=0b1110110)
| --- | --- | --- | --- | --- | --- | --- | --- | ------------------ |
| 128 |  64 |  32 |  16 |   8 |   4 |   2 |   1 | value of bits      |
| --- | --- | --- | --- | --- | --- | --- | --- | ------------------ |
|  F  |  T  |  T  |  T  |  F  |  T  |  T  |  F  | byte land bits > 0 |
+ --- + --- + --- + --- + --- + --- + --- + --- + ------------------ +


utop # 0b01110110;;
- : int = 118

---

utop # 0b1110110 land 128;;
- : int = 0

(
     0b01110110
land 0b10000000
   = 0b00000000
)

---

utop # 0b1110110 land 64;;
- : int = 64

(
     0b01110110
land 0b01000000
   = 0b01000000
)

---

utop # 0b1110110 land 32;;
- : int = 32
utop # 0b1110110 land 16;;
- : int = 16
utop # 0b1110110 land 8;;
- : int = 0
utop # 0b1110110 land 4;;
- : int = 4
utop # 0b1110110 land 2;;
- : int = 2
utop # 0b1110110 land 1;;
- : int = 0

*)

let make_input_bits input = { input; byte = 0; bit = 0 }

(** If [bit] is zero, we must load a new [byte] from the [input] and return the next bit.
  * If [bit] is non-zero, we extract the given bit, and halve [bit], ready for next time.
  *)
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

let align (x : input_bits) = x.bit <- 0

(** Read up to 31 bits.
  * Which is the number of bits used to represent [int] on a 32 bit computer (for the [bit] field).
  * We could double that amount these days (see: [Sys.word_size], which returns `64` on my computer)

  I'm not sure why we are not simply shifting by n bits instead, and grabbing the resulting value.

  For instance, let's say I want to read the first 2 bytes (so 16 bits) from the bytes 0xBB_14_62_B7...

  I could just do a shift right and I've got the same value.

  utop # 0xBB_14_62_B7 lsr 16;;
  - : int = 47892

  utop # 0xBB_14;;
  - : int = 47892

  --> So the answer is that systems deal with bytes only. I cannot "advance" my input cursor by x bits, because the
  --> underlying storage will always be expressed in bytes (that's an OS thing).
  --> So that's why we act one every bits via a loop, to then mutate the state of a global byte field.
  --> I'll have to give this more thought though.


  What this function does is this (for reading 16 bits)

  - sets an mutuable accumalator at 0
  - for each `n` steps (where we analyze a bit):
    - does a NOOP if the bit is zero
    - ORs the mutable accumalator with 2^15, 2^14, 2^13... 2^0, 0 to
  - thereby "saving"/setting any bit set in the mutable accumulator
  - this returns the same set bits for `n` bits.
  - i.e., for the input 0xBB_14_62_B7... + `n`=16, it will return 0xBB_14 (=47892) while advancing the cursor

    000000 lor (1 lsl 15 = 32768) => 32768
    032768 lor (0 lsl 14 = 00000) => 32768
    032768 lor (1 lsl 13 = 08192) => 40960
    040960 lor (1 lsl 12 = 04096) => 45056
    045056 lor (1 lsl 11 = 02048) => 47104
    047104 lor (0 lsl 10 = 00000) => 47104
    047104 lor (1 lsl 09 = 00512) => 47616
    047616 lor (1 lsl 08 = 00256) => 47872
    047872 lor (0 lsl 07 = 00000) => 47872
    047872 lor (0 lsl 06 = 00000) => 47872
    047872 lor (0 lsl 05 = 00000) => 47872
    047872 lor (1 lsl 04 = 00016) => 47888
    047888 lor (0 lsl 03 = 00000) => 47888
    047888 lor (1 lsl 02 = 00004) => 47892
    047892 lor (0 lsl 01 = 00000) => 47892
    047892 lor (0 lsl 00 = 00000) => 47892
  *)

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

let get_val_32 (ib : input_bits) (n : int) : Int32.t =
  if n <= 0 || n > 32 then
    raise (Invalid_argument "get_val_32")
  else
    let r = ref Int32.zero in
    for i = n - 1 downto 0 do
      let bv =
        if is_bit_set ib then
          Int32.one
        else
          Int32.zero
      in

      r := Int32.logor !r (Int32.shift_left bv i)
    done
    ; !r
;;

let pp_int key value =
  ()
  ; print_string key
  ; print_string " = "
  ; print_int value
  ; print_newline ()
;;

let pp_int_32 key (value : Int32.t) =
  ()
  ; print_string key
  ; print_string " = "
  ; print_string @@ Int32.to_string value
  ; print_newline ()
;;

let pp_bool key bool =
  ()
  ; print_string "  "
  ; print_string key
  ; print_string " = "
  ; print_string
      (if bool then
         "true"
       else
         "false")
  ; print_newline ()
;;

let print_tcp_header (b : input_bits) =
  let src_port = get_val b 16 in
  let dst_port = get_val b 16 in
  let seq_number = get_val_32 b 32 in
  let ack_number = get_val_32 b 32 in
  let _offset = get_val b 4 in
  let _reserved = get_val b 6 in
  let urgent = is_bit_set b in
  let ack = is_bit_set b in
  let push = is_bit_set b in
  let reset = is_bit_set b in
  let syn = is_bit_set b in
  let fin = is_bit_set b in
  let window_size = get_val b 16 in
  let checksum = get_val b 16 in
  let urgent_pointer = get_val b 16 in
  ()
  ; pp_int "Source port" src_port
  ; pp_int "Destination port" dst_port
  ; pp_int_32 "Sequence" seq_number
  ; pp_int_32 "Acknowledgment Number" ack_number
  ; print_string "Flags:"
  ; print_newline ()
  ; pp_bool "Urgent" urgent
  ; pp_bool "Ack" ack
  ; pp_bool "Push" push
  ; pp_bool "Reset" reset
  ; pp_bool "Syn" syn
  ; pp_bool "Fin" fin
  ; pp_int "Receive window size" window_size
  ; pp_int "Checksum" checksum
  ; pp_int "Urgent pointer" urgent_pointer
;;

let%expect_test _ =
  (* Also see: bin/chapter05/print_tcp_header.ml *)
  let tcp_header =
    [| 0x00 (* src port: 2 bytes *)
     ; 0x26
     ; 0xBB (* dst port: 2 bytes *)
     ; 0x14
     ; 0x62 (* sequence: 4 bytes *)
     ; 0xB7
     ; 0xCC
     ; 0x33
     ; 0x58 (* ack number *)
     ; 0x55
     ; 0x1E
     ; 0xED
     ; 0x50 (* offset, reserved, flags: 1 bytes *)
     ; 0x00
     ; 0x45 (* window size: 2 bytes *)
     ; 0x00
     ; 0x03 (* checksum: 2 bytes *)
     ; 0x78
     ; 0xF7 (* urgent pointer: 2 bytes *)
     ; 0xAC
    |]
  in
  let input = input_of_int_array tcp_header in
  let input_bits = make_input_bits input in
  ()
  ; print_tcp_header input_bits
  ; [%expect
      {|
    Source port = 38
    Destination port = 47892
    Sequence = 1656212531
    Acknowledgment Number = 1481973485
    Flags:
      Urgent = false
      Ack = false
      Push = false
      Reset = false
      Syn = false
      Fin = false
    Receive window size = 17664
    Checksum = 888
    Urgent pointer = 63404
    |}]
;;

type output =
  { char   : char -> unit
  ; length : unit -> int
  ; rewind : unit -> unit
  }
[@@ocamlformat "disable"]

type output_bits =
  { output : output
  ; mutable byte : int  (* the current byte being constructed bit by bit *)
  ; mutable bit : int   (* value=7-0, to represent where to add the bit to the current byte.
                            value=-1 if it's time to move to construct next byte. *)
  }
[@@ocamlformat "disable"]

let print_output (o : output) =
  ()
  ; print_int (o.length ())
;;

let make_output_bits output = { output; byte = 0; bit = 7 }

let flush (ob : output_bits) =
  if ob.bit < 7 then (
    ()
    ; ob.output.char (char_of_int ob.byte)
    ; ob.byte <- 0
    ; ob.bit <- 7
  )
;;

(* Any non-zero input is condisered to be 1 bit *)
let rec put_bit (ob : output_bits) (byte : int) =
  if ob.bit = -1 then (
    ()
    ; flush ob
    ; put_bit ob byte
  ) else (
    if byte <> 0 then ob.byte <- ob.byte lor (1 lsl ob.bit)
    ; ()
    ; ob.bit <- ob.bit - 1
  )
;;

let put_val (ob : output_bits) (v : int) (l : int) =
  for x = l - 1 downto 0 do
    put_bit ob (v land (1 lsl x))
  done
;;

let put_val_32 (ob : output_bits) (v : Int32.t) (l : int) =
  for x = l - 1 downto 0 do
    put_bit ob @@ Int32.to_int (Int32.logand v (Int32.shift_left Int32.one x))
  done
;;

let output_of_channel chan =
  { char = (fun char -> output_char chan char)
  ; length = (fun () -> out_channel_length chan)
  ; rewind = (fun () -> seek_out chan (out_channel_length chan - 1))
  }
;;

let write_tcp_header (o : output) =
  let ob = make_output_bits o in
  ()
  ; put_val ob 38 16 (* src port *)
  ; put_val ob 47892 16 (* dst port *)
  ; put_val_32 ob 1656212531l 32 (* sequence *)
  ; put_val_32 ob 1481973485l 32 (* ack number *)
  ; put_val ob 5 4 (* offset *)
  ; put_val ob 0 6 (* reserved *)
  ; put_bit ob 0 (* urg *)
  ; put_bit ob 0 (* ack *)
  ; put_bit ob 0 (* psh *)
  ; put_bit ob 0 (* rst *)
  ; put_bit ob 0 (* syn *)
  ; put_bit ob 0 (* fin *)
  ; put_val ob 17664 16 (* window size *)
  ; put_val ob 888 16 (* checksum *)
  ; put_val ob 63404 16 (* urgent pointer *)
  ; flush ob
;;

let reader () =
  { char = (fun c -> Printf.printf "%#x " (int_of_char c))
  ; length = (fun () -> raise (Invalid_argument "Not applicable"))
  ; rewind = (fun () -> raise (Invalid_argument "Not applicable"))
  }
;;

let%expect_test _ =
  let out = reader () in
  ()
  ; write_tcp_header out
  ; [%expect
      {| 0 0x26 0xbb 0x14 0x62 0xb7 0xcc 0x33 0x58 0x55 0x1e 0xed 0x50 0 0x45 0 0x3 0x78 0xf7 0xac |}]
;;

(*
 * QUESTIONS
 *)

(*
 * 1. Specialize the function [get_val] so that writing (the author meant reading??) 8 bits at a time when the input is aligned is optimized.
 *    Benchmark this function against the naive one.
 *
 *    I did not notice any significant performance improvements timing the execution of `print_tcp_header.exe`
 *)

let get_val_fast (ib : input_bits) (n : int) : int =
  if n = 8 && ib.bit = 0 then
    ib.input.byte ()
  else
    get_val ib n
;;

(*
 * 2. Write the function [get_val_32] which can get a value of type [Int32.t] in the same fashion as [get_val]
 *
 * Done: see above.
 *)

(*
 * 3. Specialize the function [put_val] so that writing 8 bits at a time when the output is aligned is optimized.
 *    Benchmark this function against the naive one.
 *
 *    I did not notice any significant performance improvements timing the execution of `write_tcp_header.exe`
 *)

let put_val_fast (ob : output_bits) (v : int) (l : int) =
  if l = 8 && ob.bit = 7 then
    ob.output.char (char_of_int v)
  else
    put_val ob v l
;;

(*
 * 4. Write a function [put_val_32] which can put a value of type [Int32.t] in the same fashion as [put_val]
 *    Done, see above.
 *)

(*
 * 5. We said that the [output_bits] type needed a [flush] operation.
 *    In fact, this is not always true. For outputs built with, for example, [output_of_bytes], we could write
 *    the current byte every time a bit is written, seeking back one byte each time, only moving on when the byte
 *    is actually finished.
 *
 *    Implement this <-- my solution was wrong(ish). Below is the book's solution.
 *)

let output_of_bytes b =
  let pos = ref 0 in
  { char =
      (fun c ->
        if !pos >= Bytes.length b then
          raise End_of_file
        else (
          ()
          ; Bytes.set b !pos c
          ; pos := !pos + 1
        ))
  ; length = (fun () -> Bytes.length b)
  ; rewind =
      (fun () ->
        pos :=
          if !pos > 0 then
            !pos - 1
          else
            raise (Failure "rewind"))
  }
;;

(* Any non-zero input is condisered to be 1 bit *)
let rec put_bit (ob : output_bits) (byte : int) =
  if ob.bit = -1 then (
    ()
    ; ob.byte <- 0
    ; ob.bit <- 7
    ; put_bit ob byte
  ) else (
    if byte <> 0 then ob.byte <- ob.byte lor (1 lsl ob.bit)
    ; ()
    ; ob.output.char (char_of_int ob.byte)
    ; ob.output.rewind ()
    ; ob.bit <- ob.bit - 1
  )
;;
