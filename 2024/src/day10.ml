open! Core
open Option.Let_syntax

let read_input () =
  In_channel.read_lines "input/day10.txt"
  |> List.map ~f:(fun s -> s |> String.to_list |> List.map ~f:Char.get_digit_exn)
;;

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

let all_endpoints grid pos =
  let rec all_route pos =
    let r, c = pos in
    let v = elem_exn grid pos in
    match v with
    | 9 -> [ pos ]
    | _ ->
      let n =
        match elem grid (r - 1, c) with
        | Some x -> if x = v + 1 then all_route (r - 1, c) else []
        | None -> []
      in
      let e =
        match elem grid (r, c + 1) with
        | Some x -> if x = v + 1 then all_route (r, c + 1) else []
        | None -> []
      in
      let s =
        match elem grid (r + 1, c) with
        | Some x -> if x = v + 1 then all_route (r + 1, c) else []
        | None -> []
      in
      let w =
        match elem grid (r, c - 1) with
        | Some x -> if x = v + 1 then all_route (r, c - 1) else []
        | None -> []
      in
      List.concat [ n; e; s; w ]
  in
  all_route pos
  |> List.dedup_and_sort ~compare:(fun (lr, lc) (rr, rc) ->
    if lr > rr then 1 else if lr < rr then -1 else compare lc rc)
  |> List.length
;;

(* part 1 *)
let () =
  let grid = read_input () in
  List.foldi grid ~init:0 ~f:(fun i racc row ->
    racc
    + List.foldi row ~init:0 ~f:(fun j acc c ->
      if c = 0 then acc + all_endpoints grid (i, j) else acc))
  |> printf "%d\n"
;;

let rec all_route grid pos =
  let r, c = pos in
  let v = elem_exn grid pos in
  match v with
  | 9 -> 1
  | _ ->
    let n =
      match elem grid (r - 1, c) with
      | Some x -> if x = v + 1 then all_route grid (r - 1, c) else 0
      | None -> 0
    in
    let e =
      match elem grid (r, c + 1) with
      | Some x -> if x = v + 1 then all_route grid (r, c + 1) else 0
      | None -> 0
    in
    let s =
      match elem grid (r + 1, c) with
      | Some x -> if x = v + 1 then all_route grid (r + 1, c) else 0
      | None -> 0
    in
    let w =
      match elem grid (r, c - 1) with
      | Some x -> if x = v + 1 then all_route grid (r, c - 1) else 0
      | None -> 0
    in
    n + e + s + w
;;

(* part 2 *)
let () =
  let grid = read_input () in
  List.foldi grid ~init:0 ~f:(fun i racc row ->
    racc
    + List.foldi row ~init:0 ~f:(fun j acc c ->
      if c = 0 then acc + all_route grid (i, j) else acc))
  |> printf "%d\n"
;;
