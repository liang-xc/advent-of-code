open! Core
open Option.Let_syntax

let compare_pos lhs rhs =
  let lr, lc = lhs in
  let rr, rc = rhs in
  if lr > rr then 1 else if lr < rr then -1 else compare lc rc
;;

let read_input () = In_channel.read_lines "input/day8.txt" |> List.map ~f:String.to_list

let elem grid pos =
  let r, c = pos in
  let%bind row = List.nth grid r in
  List.nth row c
;;

let elem_exn grid pos =
  let r, c = pos in
  let row = List.nth_exn grid r in
  List.nth_exn row c
;;

let antinodes grid pos_from pos_to =
  let rf, cf = pos_from in
  let rt, ct = pos_to in
  if rf = rt && cf = ct
  then None
  else (
    let new_pos = (2 * rt) - rf, (2 * ct) - cf in
    match elem grid new_pos with
    | None -> None
    | Some _ -> Some new_pos)
;;

let find_others grid pos =
  let e = elem_exn grid pos in
  if Char.equal e '.'
  then []
  else
    List.foldi grid ~init:[] ~f:(fun i lst row ->
      lst
      @ List.foldi row ~init:[] ~f:(fun j acc c ->
        if Char.equal c e
        then (
          match antinodes grid pos (i, j) with
          | None -> acc
          | Some new_pos -> new_pos :: acc)
        else acc))
;;

let find_all grid ~f =
  List.foldi grid ~init:[] ~f:(fun i lst row ->
    lst @ List.foldi row ~init:[] ~f:(fun j acc _ -> f grid (i, j) @ acc))
;;

(* part 1 *)
let () =
  read_input ()
  |> find_all ~f:find_others
  |> List.dedup_and_sort ~compare:compare_pos
  |> List.length
  |> printf "%d\n"
;;

let harmonic_antinodes grid pos_from pos_to =
  let rec loop pos0 pos1 acc =
    match antinodes grid pos0 pos1 with
    | None -> acc
    | Some new_pos -> loop pos1 new_pos (new_pos :: acc)
  in
  loop pos_from pos_to [ pos_from ]
;;

let find_harmonic_antinodes grid pos =
  let e = elem_exn grid pos in
  if Char.equal e '.'
  then []
  else
    List.foldi grid ~init:[] ~f:(fun i lst row ->
      lst
      @ List.foldi row ~init:[] ~f:(fun j acc c ->
        if Char.equal c e
        then (
          let all = harmonic_antinodes grid pos (i, j) in
          if List.is_empty all then acc else all @ acc)
        else acc))
;;

(* part 2 *)
let () =
  read_input ()
  |> find_all ~f:find_harmonic_antinodes
  |> List.dedup_and_sort ~compare:compare_pos
  |> List.length
  |> printf "%d\n"
;;
