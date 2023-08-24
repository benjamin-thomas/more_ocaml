type input =
  { pos : unit -> int
  ; seek : int -> unit
  ; char : unit -> char
  ; char_opt : unit -> char option
  ; byte : unit -> int (* integer representing the next byte *)
  ; length : int
  }
(** A general representation of input *)

(* NOTE: the original channel is not represented in the type *)
let input_of_channel ch =
  { pos = (fun () -> pos_in ch)
  ; seek = seek_in ch
  ; char = (fun () -> input_char ch)
  ; char_opt =
      (fun () ->
        try Some (input_char ch) with
        | End_of_file -> None)
  ; byte =
      (fun () ->
        try input_byte ch with
        | End_of_file -> -1)
  ; length = in_channel_length ch
  }
;;

(* Let's ensure we can represent a [String] input.
 * We mimic the channel API.
 *)
let input_of_string str =
  let pos = ref 0 in
  { pos = (fun () -> !pos)
  ; seek =
      (fun p ->
        if p < 0 then
          raise (Invalid_argument "seek before beginning")
        else
          pos := p)
  ; char =
      (fun () ->
        if !pos > String.length str - 1 then
          raise End_of_file
        else
          let c = String.get str !pos in
          ()
          ; pos := !pos + 1
          ; c)
  ; char_opt =
      (fun () ->
        if !pos > String.length str - 1 then
          None
        else
          let c = String.get str !pos in
          ()
          ; pos := !pos + 1
          ; Some c)
  ; byte =
      (fun () ->
        if !pos > String.length str - 1 then
          -1
        else
          let n = String.get_uint8 str !pos in
          ()
          ; pos := !pos + 1
          ; n)
  ; length = String.length str
  }
;;

(*
 * READING WORDS
 * We would like to transform a setence to a list of words.
 * To achive this, we will add 2 functions:
 *   - one that will allow us to go back a step
 *   - one that will tell us if the next char is to be consumed
 *)

let rewind inp = inp.seek (inp.pos () - 1)

let is_non_letter = function
  | ' '
  | '!'
  | '('
  | ')'
  | '.'
  | ','
  | ';'
  | ':' ->
      true
  | _ -> false
;;

let rec skip_non_letters inp =
  if is_non_letter (inp.char ()) then
    skip_non_letters inp
  else
    rewind inp
;;

let rec collect_letter buf inp =
  match
    try Some (inp.char ()) with
    | End_of_file -> None
  with
  | None -> Buffer.contents buf
  | Some c ->
      if is_non_letter c then
        Buffer.contents buf
      else (
        ()
        ; Buffer.add_char buf c
        ; collect_letter buf inp
      )
;;

let read_word inp =
  try
    ()
    ; skip_non_letters inp
    ; Some (collect_letter (Buffer.create 20) inp)
  with
  | End_of_file -> None
;;

let read_words inp =
  let rec loop inp lst =
    match read_word inp with
    | None -> List.rev (List.map String.lowercase_ascii lst)
    | Some word -> loop inp (word :: lst)
  in
  loop inp []
;;

let%expect_test _ =
  ()
  ; read_words (input_of_string "Hello, my name is Benjamin!!")
    |> String.concat "|"
    |> print_string
  ; [%expect "hello|my|name|is|benjamin"]
;;

let%expect_test _ =
  let sut = input_of_string "ABC" in
  ()
  ; sut.length |> print_int
  ; [%expect "3"]
  ; sut.char () |> ignore
  ; sut.char () |> print_char
  ; [%expect "B"]
;;

let%expect_test _ =
  let sut = input_of_string "ABC" in
  ()
  ; sut.pos () |> print_int ; [%expect "0"]

  ; sut.char_opt () |> ignore
  ; sut.pos () |> print_int ; [%expect "1"]

  ; sut.char_opt () |> ignore
  ; sut.pos () |> print_int ; [%expect "2"]

  ; (match sut.char_opt () with
    | None -> ()
    | Some c -> print_char c)
  ; [%expect "C"]
  ; sut.pos () |> print_int ; [%expect "3"]
[@@ocamlformat "disable"]

let%expect_test _ =
  let sut = input_of_string "ABC" in
  ()
  ; sut.char_opt () |> ignore
  ; sut.char_opt () |> ignore
  ; sut.char_opt () |> ignore
  ; (match sut.char_opt () with
    | None -> ()
    | Some c -> print_char c)
  ; [%expect ""]
  ; sut.pos () |> print_int ; [%expect "3"]
  ; sut.char_opt () |> ignore
  ; sut.char_opt () |> ignore
  ; sut.char_opt () |> ignore
  ; sut.char_opt () |> ignore
  ; sut.pos () |> print_int ; [%expect "3"]
    [@@ocamlformat "disable"]

let%expect_test _ =
  let sut = input_of_string "ABC" in
  ()
  ; sut.byte () |> print_int (* integer representing the next byte *)
  ; [%expect "65"]
  ; sut.byte () |> print_int
  ; [%expect "66"]
  ; sut.byte () |> print_int
  ; [%expect "67"]
  ; sut.byte () |> print_int
  ; [%expect "-1"]
;;

type output = { char : char -> unit; length : unit -> int }

let output_of_channel chan =
  { char = (fun char -> output_char chan char)
  ; length = (fun () -> out_channel_length chan)
  }
;;

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
  }
;;

let output_int_list out ls =
  ()
  ; out.char '['
  ; List.iteri
      (fun idx n ->
        ()
        ; if idx <> 0 then (
            ()
            ; out.char ';'
            ; out.char ' '
          )
        ; String.iter out.char (string_of_int n))
      ls
  ; out.char ']'
;;

let%expect_test _ =
  ()
  ; output_int_list (output_of_channel stdout) [ 1; 2; 3 ]
  ; [%expect "[1; 2; 3]"]
;;

(*
 * QUESTIONS
 *)

(*
 * 1. Write a function to build an input from an array of characters
 *)

let input_of_chars chars : input =
  let pos = ref 0 in
  let length = Array.length chars in
  { pos = (fun () -> !pos)
  ; seek =
      (fun p ->
        if p < 0 then
          raise (Invalid_argument "seek before beginning")
        else
          pos := p)
  ; char =
      (fun () ->
        if !pos >= length then
          raise End_of_file
        else
          let c = Array.get chars !pos in
          let () = pos := !pos + 1 in
          c)
  ; char_opt =
      (fun () ->
        if !pos >= length then
          None
        else
          let c = Array.get chars !pos in
          let () = pos := !pos + 1 in
          Some c)
  ; byte =
      (fun () ->
        if !pos >= length then
          raise End_of_file
        else
          let c = Array.get chars !pos in
          let () = pos := !pos + 1 in
          Char.code c)
  ; length
  }
;;

let%expect_test _ =
  ()
  ; read_words
      (input_of_chars
         [| 'h'; 'e'; 'l'; 'l'; 'o'; ' '; 'w'; 'o'; 'r'; 'l'; 'd' |])
    |> String.concat "|"
    |> print_string
  ; [%expect "hello|world"]
;;

(*
 * 2. Write a function [input_string] of type [input -> int -> string] which returns the given number
 *    of characters from the input as a string, or fewer if the input has ended.
 *)

let input_string inp n =
  let buf = Buffer.create 20 in
  let rec inner (inp : input) = function
    | 0 -> Buffer.contents buf
    | n ->
        ()
        ; Buffer.add_char buf @@ inp.char ()
        ; inner inp (n - 1)
  in
  try inner inp n with
  | End_of_file -> Buffer.contents buf
;;

let%expect_test _ =
  ()
  ; input_string (input_of_string "abcd") 3 |> print_string
  ; [%expect "abc"]
  ; input_string (input_of_string "ab") 3 |> print_string
  ; [%expect "ab"]
;;

(*
 * 3. Extend the [input] type to include a function [input_char_opt] which returns a value of type [char option],
 *    with [None] signalling end of file.
 *    Extend the functions [input_of_channel] and [input_of_string] appropriately.
 *
 *    Done. See above.
 *)

(*
 * 4. Extend the [input] type with a function [input_byte] which returns an integer representing the next byte,
 *    or the special value -1 at end of file.
 *
 *    Comment on the usefulness of this compared to [input_char_opt] and [input_char]
 *
 *    [input_char] is the hardest to test. And hardest to use since we have to always be aware that the program may throw.
 *    [input_byte] is slightly better but carries a magic number.
 *    [input_char_opt] carries the most meaning, in the returned type.
 *
 *    Book says no advantages, appart from being "very fast".
 *)

(*
 * 5. Write an input type which raises [End_of_file] if it reaches a new line (`\n`).
 *    Use this to build a program which reads a line from standard input.
 *)

(* I'm not sure what I'm actually meant to do here! *)
type input2 =
  { pos : unit -> int; seek : int -> unit; char : unit -> char; length : int }

let input2_of_channel ch : input2 =
  { pos = (fun () -> pos_in ch)
  ; seek = seek_in ch
  ; char =
      (fun () ->
        let c = input_char ch in
        if c = '\n' then
          raise End_of_file
        else
          c)
  ; length = in_channel_length ch
  }
;;

(* Book solution below
 * Also see: ./bin/chapter04/cat_first_line.ml
 *)

exception Not_implemented

let single_line_input_of_channel ch : input =
  { pos = (fun () -> pos_in ch)
  ; seek = seek_in ch
  ; char =
      (fun () ->
        match input_char ch with
        | '\n' -> raise End_of_file
        | c -> c)
  ; length = in_channel_length ch
  ; char_opt = (fun () -> raise Not_implemented)
  ; byte = (fun () -> raise Not_implemented)
  }
;;

(*
 * 6. Write a function to build an output from a [Buffer.t].
 *    Show how this can be used to retrieve a final string after output is finished.
 *)

let output_of_buffer buf : output =
  { length = (fun () -> Buffer.length buf); char = Buffer.add_char buf }
;;

let%expect_test _ =
  let buf = Buffer.create 12 in
  ()
  ; output_int_list (output_of_buffer buf) [ 1; 2; 3; 4 ]
  ; Buffer.contents buf |> print_string
  ; [%expect "[1; 2; 3; 4]"]
  ; Buffer.length buf |> print_int
  ; [%expect "12"]
;;
