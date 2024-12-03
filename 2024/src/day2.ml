open! Core

let parse_report line = line |> String.split ~on:' ' |> List.map ~f:Int.of_string

let report_check f =
  In_channel.with_file "input/day2.txt" ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
      let safe = line |> parse_report |> f in
      if safe then acc + 1 else acc))
;;

let criteria x1 x2 ascending =
  let distance = if ascending then x2 - x1 else x1 - x2 in
  distance < 4 && distance > 0
;;

let is_safe report =
  let rec loop ascending = function
    | [] | [ _ ] -> true
    | x1 :: (x2 :: _ as rest) ->
      if criteria x1 x2 ascending then loop ascending rest else false
  in
  match report with
  | [] | [ _ ] -> true
  | x1 :: x2 :: _ ->
    let ascending = x1 < x2 in
    loop ascending report
;;

let () = printf "part1 ans:\t%d\n" (report_check is_safe)

let is_safe_with_dampener_bf report =
  let rec loop n acc =
    let l, r = List.split_n report n in
    match r with
    | [] -> acc
    | _ :: tl -> loop (n + 1) (is_safe (l @ tl) || acc)
  in
  is_safe report || loop 0 false
;;

let is_safe_with_dampener report =
  let rec loop ascending n =
    let l, r = List.split_n report n in
    let ll = List.last_exn l in
    match r with
    | [] -> true
    | x :: tl ->
      if criteria ll x ascending
      then loop ascending (n + 1)
      else (
        let new_lst1 = List.sub l ~pos:0 ~len:(List.length l - 1) @ r in
        let new_lst2 = l @ tl in
        is_safe new_lst1 || is_safe new_lst2)
  in
  match report with
  | [] | [ _ ] | [ _; _ ] -> true
  | x1 :: x2 :: x3 :: tl ->
    let ascending = x1 < x2 in
    if criteria x1 x2 ascending
    then
      if criteria x2 x3 ascending
      then loop ascending 2
      else
        (* corner case for x3 *)
        is_safe (x1 :: x2 :: tl) || is_safe (x1 :: x3 :: tl) || is_safe (x2 :: x3 :: tl)
    else is_safe (x2 :: x3 :: tl) || is_safe (x1 :: x3 :: tl)
;;

let () = printf "part2 ans bf:\t%d\n" (report_check is_safe_with_dampener_bf)
let () = printf "part2 ans:\t%d\n" (report_check is_safe_with_dampener)
