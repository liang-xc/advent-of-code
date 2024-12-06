open! Core

type direction =
  | Up
  | Down
  | Left
  | Right

type position = int * int

type status =
  { dir : direction
  ; pos : position
  }

let read_input () = In_channel.read_lines "input/day6.txt" |> List.map ~f:String.to_list

let init_pos orig_grid =
  let rec search grid index =
    match grid with
    | [] -> None
    | hd :: tl ->
      (match List.findi hd ~f:(fun _ c -> Char.equal c '^') with
       | None -> search tl (index + 1)
       | Some (j, _) -> Some (index, j))
  in
  search orig_grid 0
;;

let starting_point orig_grid =
  match init_pos orig_grid with
  | None -> failwith "No ^, wrong input"
  | Some pos -> { dir = Up; pos }
;;

let elem grid r c =
  let open Option.Let_syntax in
  let%bind row = List.nth grid r in
  List.nth row c
;;

let in_front status grid =
  let r, c = status.pos in
  match status.dir with
  | Up -> elem grid (r - 1) c
  | Down -> elem grid (r + 1) c
  | Left -> elem grid r (c - 1)
  | Right -> elem grid r (c + 1)
;;

let turn_right status =
  match status.dir with
  | Up -> { status with dir = Right }
  | Down -> { status with dir = Left }
  | Left -> { status with dir = Up }
  | Right -> { status with dir = Down }
;;

let move_forward status =
  let r, c = status.pos in
  match status.dir with
  | Up -> { status with pos = r - 1, c }
  | Down -> { status with pos = r + 1, c }
  | Left -> { status with pos = r, c - 1 }
  | Right -> { status with pos = r, c + 1 }
;;

let generate_route grid =
  let rec loop status route =
    match in_front status grid with
    | None -> status.pos :: route
    | Some c ->
      if Char.equal c '#'
      then loop (turn_right status) route
      else loop (move_forward status) (status.pos :: route)
  in
  loop (starting_point grid) []
;;

let unique_pos_on_route route =
  List.dedup_and_sort route ~compare:(fun (lr, lc) (rr, rc) ->
    if lr > rr then 1 else if lr < rr then -1 else compare lc rc)
;;

(* part 1 *)
let () =
  read_input () |> generate_route |> unique_pos_on_route |> List.length |> printf "%d\n"
;;

let is_equal_pos posl posr =
  let lr, lc = posl in
  let rr, rc = posr in
  lr = rr && lc = rc
;;

let is_equal_status lhs rhs =
  if is_equal_pos lhs.pos rhs.pos
  then (
    match lhs.dir, rhs.dir with
    | Up, Up | Down, Down | Left, Left | Right, Right -> true
    | _, _ -> false)
  else false
;;

let is_trap grid init_status =
  let rec loop status route =
    if List.exists route ~f:(fun s -> is_equal_status s status)
    then true
    else (
      match in_front status grid with
      | None -> false
      | Some c ->
        if Char.equal c '#'
        then loop (turn_right status) route
        else loop (move_forward status) (status :: route))
  in
  loop init_status []
;;

let add_obstacle grid (r, c) =
  List.mapi grid ~f:(fun i row ->
    if i = r then List.mapi row ~f:(fun j x -> if j = c then '#' else x) else row)
;;

let num_traps grid =
  let init_status = starting_point grid in
  grid
  |> generate_route
  |> unique_pos_on_route
  |> List.fold ~init:0 ~f:(fun acc pos ->
    if is_equal_pos pos init_status.pos
    then acc
    else (
      let modified = add_obstacle grid pos in
      if is_trap modified init_status then acc + 1 else acc))
;;

(* part 2 *)
let () = read_input () |> num_traps |> printf "%d\n"
