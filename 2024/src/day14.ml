open! Core

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, equal, sexp]
  end

  include T
  include Comparator.Make (T)
end

type robot =
  { pos : Pos.t
  ; vel : int * int
  }

let parse line =
  let lst =
    line
    |> String.split ~on:' '
    |> List.concat_map ~f:(fun part ->
      match String.split ~on:'=' part with
      | _ :: value :: _ ->
        let numbers = String.split ~on:',' value in
        List.filter_map numbers ~f:Int.of_string_opt
      | _ -> [])
  in
  { pos = List.hd_exn lst, List.nth_exn lst 1
  ; vel = List.nth_exn lst 2, List.nth_exn lst 3
  }
;;

let read_input () = In_channel.read_lines "input/day14.txt" |> List.map ~f:parse

let move robot space =
  let { pos; vel } = robot in
  let x, y = pos in
  let vx, vy = vel in
  let a, b = space in
  let x' = x + vx in
  let x' = if x' < 0 then x' + a else if x' >= a then x' - a else x' in
  let y' = y + vy in
  let y' = if y' < 0 then y' + b else if y' >= b then y' - b else y' in
  { robot with pos = x', y' }
;;

let move_secs n robot space =
  let rec aux n' r =
    if n' > 0
    then (
      let r = move r space in
      aux (n' - 1) r)
    else r
  in
  aux n robot
;;

let safety_factor robots space =
  let a, b = space in
  let am = a / 2 in
  let bm = b / 2 in
  let d1 =
    robots
    |> List.filter ~f:(fun { pos; _ } ->
      let x, y = pos in
      x > am && y > bm)
    |> List.length
  in
  let d2 =
    robots
    |> List.filter ~f:(fun { pos; _ } ->
      let x, y = pos in
      x < am && y > bm)
    |> List.length
  in
  let d3 =
    robots
    |> List.filter ~f:(fun { pos; _ } ->
      let x, y = pos in
      x < am && y < bm)
    |> List.length
  in
  let d4 =
    robots
    |> List.filter ~f:(fun { pos; _ } ->
      let x, y = pos in
      x > am && y < bm)
    |> List.length
  in
  d1 * d2 * d3 * d4
;;

(* part 1 *)
let () =
  let space = 101, 103 in
  let rb = read_input () |> List.map ~f:(fun r -> move_secs 100 r space) in
  safety_factor rb space |> printf "%d\n"
;;

let no_overlap robots =
  let rec aux pos_set = function
    | [] -> true
    | hd :: tl ->
      let { pos; _ } = hd in
      if Set.mem pos_set pos then false else aux (Set.add pos_set pos) tl
  in
  aux (Set.empty (module Pos)) robots
;;

let earliest_easter_egg robot space =
  let rec aux n r =
    if no_overlap r
    then n
    else (
      let r = List.map r ~f:(fun rb -> move rb space) in
      aux (n + 1) r)
  in
  aux 0 robot
;;

(* part 2 *)
let () =
  let space = 101, 103 in
  earliest_easter_egg (read_input ()) space |> printf "%d\n"
;;
