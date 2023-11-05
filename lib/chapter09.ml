(* My approach, reading the specs. *)
module Search_list_v0 = struct
  let search pat lst =
    let rec aux p = function
      | [] -> p = []
      | h :: t -> (
          match p with
          | [] -> true
          | ph :: pt ->
              if ph = h then
                aux pt t
              else
                aux pat t)
    in
    aux pat lst
  ;;

  let%test _ = true = search [ 1; 2; 3 ] [ 1; 2; 3; 5 ]
  let%test _ = true = search [ 1; 2; 3 ] [ 0; 1; 2; 3; 5 ]
  let%test _ = true = search [ 1; 2; 3 ] [ 0; 1; 2; 3 ]
  let%test _ = true = search [ 1; 2; 3 ] [ 1; 2; 3 ]
  let%test _ = false = search [ 1; 2; 3 ] [ 1; 2; 4; 3; 5 ]
  let%test _ = false = search [ 1; 2; 3 ] [ 1; 2 ]
  let%test _ = false = search [ 1; 2; 3 ] [ 2; 3 ]
  let%test _ = false = search [ 1; 2; 3 ] [ 2; 3; 1 ]
end

(* The book's approach *)
module Search_list_v1 = struct
  let rec take n = function
    | h :: t when n > 0 -> h :: take (n - 1) t
    | _ -> []
  ;;

  let rec drop n = function
    | _ :: t when n > 0 -> drop (n - 1) t
    | rest -> rest
  ;;

  let search pat lst =
    let rec aux pat len_pat lst len_lst =
      len_pat <= len_lst
      && (pat = take len_pat lst || aux pat len_pat (List.tl lst) (len_lst - 1))
    in
    aux pat (List.length pat) lst (List.length lst)
  ;;

  let%test _ = true = search [ 1; 2; 3 ] [ 1; 2; 3; 5 ]
  let%test _ = true = search [ 1; 2; 3 ] [ 0; 1; 2; 3; 5 ]
  let%test _ = true = search [ 1; 2; 3 ] [ 0; 1; 2; 3 ]
  let%test _ = true = search [ 1; 2; 3 ] [ 1; 2; 3 ]
  let%test _ = false = search [ 1; 2; 3 ] [ 1; 2; 4; 3; 5 ]
  let%test _ = false = search [ 1; 2; 3 ] [ 1; 2 ]
  let%test _ = false = search [ 1; 2; 3 ] [ 2; 3 ]
  let%test _ = false = search [ 1; 2; 3 ] [ 2; 3; 1 ]
end

module Search_list_v2 = struct
  (* We can also remove `take` to avoid the construction of intermediate lists, by
   * writing an explicit function to test the pattern against the first `len_pat`
   * items in the list.
   *)

  let rec equal lst len pat =
    len = 0
    || (List.hd lst = List.hd pat && equal (List.tl lst) (len - 1) (List.tl pat))
  ;;

  let search pat lst =
    let rec aux pat len_pat lst len_lst =
      len_pat <= len_lst
      && (equal lst len_pat pat || aux pat len_pat (List.tl lst) (len_lst - 1))
    in
    aux pat (List.length pat) lst (List.length lst)
  ;;

  let%test _ = true = search [ 1; 2; 3 ] [ 1; 2; 3; 5 ]
  let%test _ = true = search [ 1; 2; 3 ] [ 0; 1; 2; 3; 5 ]
  let%test _ = true = search [ 1; 2; 3 ] [ 0; 1; 2; 3 ]
  let%test _ = true = search [ 1; 2; 3 ] [ 1; 2; 3 ]
  let%test _ = false = search [ 1; 2; 3 ] [ 1; 2; 4; 3; 5 ]
  let%test _ = false = search [ 1; 2; 3 ] [ 1; 2 ]
  let%test _ = false = search [ 1; 2; 3 ] [ 2; 3 ]
  let%test _ = false = search [ 1; 2; 3 ] [ 2; 3; 1 ]
end

module Search_strings_v1 = struct
  (*
   * Searching for strings
   * "Our first version generalizes easily."
   * "This time, their is no problem with `String.length`, which runs in constant time."
   *)

  let rec search pat str =
    String.length pat <= String.length str
    && (pat = String.sub str 0 (String.length pat)
       || search pat (String.sub str 1 (String.length str - 1)))
  ;;

  let%test _ = true = search "abc" "abcde"
  let%test _ = true = search "abc" "xabcde"
  let%test _ = true = search "abc" "xyzabc"
  let%test _ = false = search "abc" "xyzab"
  let%test _ = false = search "abc" "xyz"
  let%test _ = false = search "abc" "xaybzc"
  let%test _ = false = search "abc" ""
end

module Search_strings_v2 = struct
  (*
   * `String.sub` is a problem however, since it create a new string at each invocation.
   * To solve this, we define `at`.
   *)
  type pat = Pat of string * int * int (* (pattern, position, length) *)
  type cursor = Cursor of int
  type pat_len = Pat_len of int

  let rec at (Cursor cursor) (Pat (pat, pos, len)) str =
    len = 0
    || pat.[pos] = str.[cursor]
         && at (Cursor (cursor + 1))
               (Pat (pat, pos + 1, len - 1))
               str
  [@@ocamlformat "disable"]

  let rec search' (Cursor cursor) pat str =
    String.length pat <= String.length str - cursor
      && begin
           at (Cursor cursor)
              (Pat (pat, 0, String.length pat))
              str
           || search' (Cursor (cursor + 1)) pat str
         end
  [@@ocamlformat "disable"]

  let search = search' (Cursor 0)
  let%test _ = true = search "abc" "abcde"
  let%test _ = true = search "abc" "xabcde"
  let%test _ = true = search "abc" "xyzabc"
  let%test _ = false = search "abc" "xyzab"
  let%test _ = false = search "abc" "xyz"
  let%test _ = false = search "abc" "xaybzc"
  let%test _ = false = search "abc" ""
end

module Search_strings_v3 = struct
  (* Implements a regex-like search:
   *   - ? : matches zero or 1 char
   *   - * : matches zero or many chars
   *   - + : matches one or many chars
   *)

  let rec at p pp s sp =
    pp > String.length p - 1 (* whole pattern used – match *)
    ||
    match
      match p.[pp] with
      | '?' ->
          if pp + 1 > String.length p - 1 then
            (* end pattern *)
            None
          else if sp > String.length s - 1 then
            (* end string *)
            Some (2, 0)
          else if p.[pp + 1] = s.[sp] then
            (* the character *)
            Some (2, 1)
          else
            (* any other character *)
            Some (2, 0)
      | c ->
          if sp < String.length s && c = s.[sp] then
            Some (1, 1)
          else
            None
    with
    | None -> (* match failure – stop *) false
    | Some (jump_p, jump_s) ->
        (* match success – continue *)
        at p (pp + jump_p) s (sp + jump_s)
  ;;

  let rec search' n p s =
    (n < String.length s || (n = 0 && String.length s = 0))
    && (at p 0 s n || search' (n + 1) p s)
  ;;

  let search = search' 0
  let%test _ = true = search "?abc" "abcde"
  let%test _ = true = search "?abc" "xbcde"
  let%test _ = false = search "?abc" "xbdde"
  let%test _ = true = search "a?bc" "ac"
  let%test _ = true = search "a?bc" "abc"
  let%test _ = true = search "abc" "abcde"
  let%test _ = true = search "abc" "xabcde"
  let%test _ = true = search "abc" "xyzabc"
  let%test _ = false = search "abc" "xyzab"
  let%test _ = false = search "abc" "xyz"
  let%test _ = false = search "abc" "xaybzc"
  let%test _ = false = search "abc" ""
end

(* Skipping the rest, not interested. What comes up is mutation heavy.
 * Overall, I'm not convinced by this chapter, I don't like this kind of code.
 * The exercises don't seem interesting to me either.
 *)
