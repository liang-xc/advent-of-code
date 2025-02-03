open! Core
open Option.Let_syntax

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, equal, sexp]
  end

  include T
  include Comparator.Make (T)
end

let read_input () = In_channel.read_lines "input/day12.txt" |> List.map ~f:String.to_list

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

let flood_fill grid pos is_visited =
  let target = elem_exn grid pos in
  let rec aux pos' acc is_visited_set =
    if Set.mem is_visited_set pos'
    then acc, is_visited_set
    else (
      match elem grid pos' with
      | None -> acc, is_visited_set
      | Some c ->
        if Char.equal c target
        then (
          let is_visited_set = Set.add is_visited_set pos' in
          let acc = Set.add acc pos' in
          let r, c = pos' in
          let na, nv = aux (r - 1, c) acc is_visited_set in
          let sa, sv = aux (r + 1, c) na nv in
          let wa, wv = aux (r, c - 1) sa sv in
          let ea, ev = aux (r, c + 1) wa wv in
          ea, ev)
        else acc, is_visited_set)
  in
  aux pos (Set.empty (module Pos)) is_visited
;;

let scan_grid grid =
  let is_visited = Set.empty (module Pos) in
  let regions, _ =
    List.foldi grid ~init:([], is_visited) ~f:(fun r (regions, is_v) row ->
      List.foldi row ~init:(regions, is_v) ~f:(fun c (regions', is_v') _ ->
        if Set.mem is_v' (r, c)
        then regions', is_v'
        else (
          let region, is_visited' = flood_fill grid (r, c) is_v' in
          region :: regions', is_visited')))
  in
  regions
;;

let perimeter grid region =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | (r, c) :: tl ->
      let n =
        Bool.to_int
          (Option.is_none (elem grid (r - 1, c)) || not (Set.mem region (r - 1, c)))
      in
      let s =
        Bool.to_int
          (Option.is_none (elem grid (r + 1, c)) || not (Set.mem region (r + 1, c)))
      in
      let w =
        Bool.to_int
          (Option.is_none (elem grid (r, c - 1)) || not (Set.mem region (r, c - 1)))
      in
      let e =
        Bool.to_int
          (Option.is_none (elem grid (r, c + 1)) || not (Set.mem region (r, c + 1)))
      in
      aux tl (acc + n + s + w + e)
  in
  aux (Set.to_list region) 0
;;

(* part 1 *)
let () =
  let grid = read_input () in
  let regions = scan_grid grid in
  List.map regions ~f:(fun r -> perimeter grid r * Set.length r)
  |> List.fold ~init:0 ~f:( + )
  |> printf "%d\n"
;;

let corner region =
  let check_corner adjacent1 adjacent2 diag =
    let a1 = Set.mem region adjacent1 in
    let a2 = Set.mem region adjacent2 in
    let d = Set.mem region diag in
    match a1, a2, d with
    | false, false, _ ->
      (* a convex corner
       d a1
        |-------
      a2| x
      *)
      1
    | true, true, false ->
      (* a concave corner
       d| a1
      --|
      a2 x
      *)
      1
    | _ -> 0
  in
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | (r, c) :: tl ->
      let top_left = check_corner (r - 1, c) (r, c - 1) (r - 1, c - 1) in
      let top_right = check_corner (r - 1, c) (r, c + 1) (r - 1, c + 1) in
      let bottom_left = check_corner (r + 1, c) (r, c - 1) (r + 1, c - 1) in
      let bottom_right = check_corner (r + 1, c) (r, c + 1) (r + 1, c + 1) in
      aux tl (acc + top_left + top_right + bottom_left + bottom_right)
  in
  aux (Set.to_list region) 0
;;

(* part 2 *)
let () =
  let grid = read_input () in
  let regions = scan_grid grid in
  List.map regions ~f:(fun r -> corner r * Set.length r)
  |> List.fold ~init:0 ~f:( + )
  |> printf "%d\n"
;;
