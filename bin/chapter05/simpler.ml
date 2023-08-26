open Printf

(*
  "Simpler" example adapted from chatgpt feedback.

    - all data is read into memory, then worked on so it's not efficent
    - it looks like I may interpret some bits as an signed int so this implementation is probably wrong.

  Keeping for ref...
 *)

type tcp_header =
  { source_port : int
  ; destination_port : int
  ; sequence_number : int32
  ; ack_number : int32
  ; data_offset : int
  ; flags : int
  ; window_size : int
  ; checksum : int
  ; urgent_pointer : int
  }

let decode_tcp_header (data : bytes) : tcp_header =
  let source_port =
    (* (Char.code (Bytes.get data 0) lsl 8) + Char.code (Bytes.get data 1) *)
    Bytes.get_uint16_be data 0
  in
  let destination_port =
    (* (Char.code (Bytes.get data 2) lsl 8) + Char.code (Bytes.get data 3) *)
    Bytes.get_uint16_be data 2
  in
  let sequence_number =
    (* There is no uint variant *)
    Bytes.get_int32_be data 4
  in
  let ack_number =
    (* There is no uint variant *)
    Bytes.get_int32_be data 8
  in
  let data_offset = (Char.code (Bytes.get data 12) land 0xF0) lsr 4 in
  let flags = Char.code (Bytes.get data 13) in
  let window_size =
    (Char.code (Bytes.get data 14) lsl 8) + Char.code (Bytes.get data 15)
  in
  let checksum =
    (Char.code (Bytes.get data 16) lsl 8) + Char.code (Bytes.get data 17)
  in
  let urgent_pointer =
    (Char.code (Bytes.get data 18) lsl 8) + Char.code (Bytes.get data 19)
  in

  { source_port
  ; destination_port
  ; sequence_number
  ; ack_number
  ; data_offset
  ; flags
  ; window_size
  ; checksum
  ; urgent_pointer
  }
;;

type tcp_flags =
  { urg : bool; ack : bool; psh : bool; rst : bool; syn : bool; fin : bool }

let decode_flags (flags_byte : int) : tcp_flags =
  { urg = flags_byte land 0b10000000 <> 0
  ; ack = flags_byte land 0b01000000 <> 0
  ; psh = flags_byte land 0b00100000 <> 0
  ; rst = flags_byte land 0b00010000 <> 0
  ; syn = flags_byte land 0b00001000 <> 0
  ; fin = flags_byte land 0b00000100 <> 0
  }
;;

let () =
  let data =
    Bytes.of_string
      (String.concat ""
         [ "\x00\x26" (* src port: 2 bytes *)
         ; "\xBB\x14" (* dst port: 2 bytes *)
         ; "\x62\xB7\xCC\x33" (* sequence: 4 bytes *)
         ; "\x58\x55\x1E\xED" (* ack number *)
         ; "\x50\x00\x45" (* offset, reserved, flags: 1 byte *)
         ; "\x00\x03" (* window size *)
         ; "\x78\xF7" (* checksum: 2 bytes *)
         ; "\xAC" (* urgent pointer: 2 bytes *)
         ])
  in
  let header = decode_tcp_header data in
  let flags = decode_flags header.flags in
  ()
  ; printf "Source Port: %d\n" header.source_port
  ; printf "Destination Port: %d\n" header.destination_port
  ; printf "Sequence Number: %ld\n" header.sequence_number
  ; printf "ACK Number: %ld\n" header.ack_number
  ; printf "Data Offset: %d\n" header.data_offset
  ; printf "Flags: 0x%02X\n" header.flags
  ; printf "  URG: %b\n" flags.urg
  ; printf "  ACK: %b\n" flags.ack
  ; printf "  PSH: %b\n" flags.psh
  ; printf "  RST: %b\n" flags.rst
  ; printf "  SYN: %b\n" flags.syn
  ; printf "  FIN: %b\n" flags.fin
  ; printf "Window Size: %d\n" header.window_size
  ; printf "Checksum: 0x%04X\n" header.checksum
  ; printf "Urgent Pointer: %d\n" header.urgent_pointer
;;
