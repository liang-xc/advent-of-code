open! Core

let add_n map k n =
  Map.update map k ~f:(fun orig ->
    match orig with
    | Some v -> v + n
    | None -> n)
;;

let read_input () =
  In_channel.read_all "input/day11.txt"
  |> String.strip ~drop:(fun c -> Char.equal '\n' c)
  |> String.split ~on:' '
  |> List.fold ~init:(Map.empty (module String)) ~f:(fun acc i -> add_n acc i 1)
;;

let transform map stone n =
  if String.equal stone "0"
  then add_n map "1" n
  else if String.length stone mod 2 = 0
  then (
    let half = String.length stone / 2 in
    let first = String.slice stone 0 half |> Int.of_string |> Int.to_string in
    let second =
      String.slice stone half (String.length stone) |> Int.of_string |> Int.to_string
    in
    let map = add_n map first n in
    add_n map second n)
  else add_n map (Int.to_string (Int.of_string stone * 2024)) n
;;

let blink n orig_stones =
  let rec loop curr_n curr_stones =
    if curr_n < n
    then (
      let new_line =
        Map.fold
          curr_stones
          ~init:(Map.empty (module String))
          ~f:(fun ~key ~data acc -> transform acc key data)
      in
      loop (curr_n + 1) new_line)
    else curr_stones
  in
  loop 0 orig_stones
;;

let num_of_stones = Map.fold ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)

(* part 1 *)
let () = read_input () |> blink 25 |> num_of_stones |> printf "%d\n"

(* part 2 *)
let () = read_input () |> blink 75 |> num_of_stones |> printf "%d\n"
