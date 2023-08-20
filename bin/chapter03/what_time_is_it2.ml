(*
   dune exec --display=quiet ./bin/chapter03/what_time_is_it2.exe -w
 *)

(*
 * 2. Using functions from the "Time Functions" section of the documentation to the [Unix] module,
 *    write a program which, when run, returns a string containing the time and date, for example
 *    "It is 2:45 on Wednesday 8 January 2014".
 *)

open Printf

let week_day = function
  | 0 -> Ok "Sunday"
  | 1 -> Ok "Monday"
  | 2 -> Ok "Tuesday"
  | 3 -> Ok "Wednesday"
  | 4 -> Ok "Thursday"
  | 5 -> Ok "Friday"
  | 6 -> Ok "Saturday"
  | n -> Error (sprintf "Invalid week_day: %d" n)
;;

let month = function
  | 0 -> Ok "January"
  | 1 -> Ok "February"
  | 2 -> Ok "March"
  | 3 -> Ok "April"
  | 4 -> Ok "May"
  | 5 -> Ok "June"
  | 6 -> Ok "July"
  | 7 -> Ok "August"
  | 8 -> Ok "September"
  | 9 -> Ok "October"
  | 10 -> Ok "November"
  | 11 -> Ok "December"
  | n -> Error (sprintf "Invalid month: %d" n)
;;

let string_of_time (t : Unix.tm) =
  let ( let* ) = Result.bind in
  let* week_day = week_day t.tm_wday in
  let* month = month t.tm_mon in
  Ok
    ("It is "
    ^ sprintf "%02d" t.tm_hour
    ^ ":"
    ^ sprintf "%02d" t.tm_min
    ^ " on "
    ^ week_day
    ^ " "
    ^ string_of_int t.tm_mday
    ^ " "
    ^ month
    ^ " "
    ^ string_of_int (t.tm_year + 1900)
    ^ ".")
;;

let () =
  let now = Unix.(localtime @@ time ()) in
  match string_of_time now with
  | Error str -> prerr_endline ("Should never happen: " ^ str)
  | Ok str -> print_endline str
;;
