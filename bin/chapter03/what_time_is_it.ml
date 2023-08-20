(*
   dune exec --display=quiet ./bin/chapter03/what_time_is_it.exe -w
 *)

(*
 * 2. Using functions from the "Time Functions" section of the documentation to the [Unix] module,
 *    write a program which, when run, returns a string containing the time and date, for example
 *    "It is 2:45 on Wednesday 8 January 2014".
 *)

let week_day (t : Unix.tm) =
  match t.tm_wday with
  | 0 -> "Sunday"
  | 1 -> "Monday"
  | 2 -> "Tuesday"
  | 3 -> "Wednesday"
  | 4 -> "Thursday"
  | 5 -> "Friday"
  | 6 -> "Saturday"
  | _ -> assert false
;;

let month (t : Unix.tm) =
  match t.tm_mon with
  | 0 -> "January"
  | 1 -> "February"
  | 2 -> "March"
  | 3 -> "April"
  | 4 -> "May"
  | 5 -> "June"
  | 6 -> "July"
  | 7 -> "August"
  | 8 -> "September"
  | 9 -> "October"
  | 10 -> "November"
  | 11 -> "December"
  | _ -> assert false
;;

let string_of_time (t : Unix.tm) =
  let open Printf in
  "It is "
  ^ sprintf "%02d" t.tm_hour
  ^ ":"
  ^ sprintf "%02d" t.tm_min
  ^ " on "
  ^ week_day t
  ^ " "
  ^ string_of_int t.tm_mday
  ^ " "
  ^ month t
  ^ " "
  ^ string_of_int (t.tm_year+1900)
  ^ "."
[@@ocamlformat "disable"]

let () =
  let now = Unix.(localtime @@ time ()) in
  print_endline @@ string_of_time now
;;
