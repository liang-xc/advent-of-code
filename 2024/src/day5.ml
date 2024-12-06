open! Core

let read_input () =
  In_channel.read_lines "input/day5.txt"
  |> List.fold ~init:([], []) ~f:(fun (rules, instructions) line ->
    if String.exists line ~f:(fun c -> Char.equal c '|')
    then (
      let rule = String.split line ~on:'|' in
      let l = rule |> List.hd_exn |> Int.of_string in
      let r = rule |> List.last_exn |> Int.of_string in
      (l, r) :: rules, instructions)
    else if String.is_empty line
    then rules, instructions
    else (
      let instruct = String.split line ~on:',' |> List.map ~f:Int.of_string in
      rules, instruct :: instructions))
;;

let is_safe rule instruction =
  let x, y = rule in
  match List.findi instruction ~f:(fun _ n -> n = x) with
  | None -> true
  | Some (i, _) ->
    (match List.findi instruction ~f:(fun _ n -> n = y) with
     | None -> true
     | Some (j, _) -> i < j)
;;

let all_safe rule_lst instruction =
  List.for_all rule_lst ~f:(fun r -> is_safe r instruction)
;;

let get_mid_exn l =
  let mid = List.length l / 2 in
  List.nth_exn l mid
;;

(* part 1 *)
let () =
  let rule_lst, instruction_lst = read_input () in
  let sum =
    List.fold instruction_lst ~init:0 ~f:(fun acc i ->
      if all_safe rule_lst i then get_mid_exn i + acc else acc)
  in
  printf "%d\n" sum
;;

let rule_based_compare rule_lst x y =
  match List.find rule_lst ~f:(fun (a, b) -> a = x && b = y) with
  | None ->
    (match List.find rule_lst ~f:(fun (a, b) -> a = y && b = x) with
     | None -> 0
     | Some _ -> 1)
  | Some _ -> -1
;;

let reorder rule_lst instruction =
  List.sort instruction ~compare:(rule_based_compare rule_lst)
;;

(* part 2 *)
let () =
  let rule_lst, instruction_lst = read_input () in
  let unsafe =
    List.filter instruction_lst ~f:(fun i ->
      not (List.for_all rule_lst ~f:(fun r -> is_safe r i)))
  in
  unsafe
  |> List.map ~f:(reorder rule_lst)
  |> List.map ~f:get_mid_exn
  |> List.fold ~init:0 ~f:( + )
  |> printf "%d\n"
;;
