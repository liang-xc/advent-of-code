open! Core

let read_input () =
  In_channel.read_lines "input/day7.txt"
  |> List.map ~f:(fun row ->
    let s = String.split row ~on:':' in
    let r = List.hd_exn s |> Int.of_string in
    let v =
      s
      |> List.last_exn
      |> String.lstrip
      |> String.split ~on:' '
      |> List.map ~f:Int.of_string
    in
    r, v)
;;

let calibratable result num_lst =
  let rec loop acc nums =
    match nums with
    | [] -> acc = result
    | hd :: tl -> loop (acc + hd) tl || loop (acc * hd) tl
  in
  match num_lst with
  | [] -> false
  | hd :: tl -> loop hd tl
;;

let total_calibration ~f =
  List.fold ~init:0 ~f:(fun acc (r, nums) -> if f r nums then acc + r else acc)
;;

(* part 1 *)
let () = read_input () |> total_calibration ~f:calibratable |> printf "%d\n"

(** missing operator to concat two int *)
let ( |^| ) i j = Int.of_string (Int.to_string i ^ Int.to_string j)

let calibratable' result num_lst =
  let rec loop acc nums =
    match nums with
    | [] -> acc = result
    | hd :: tl -> loop (acc + hd) tl || loop (acc * hd) tl || loop (acc |^| hd) tl
  in
  match num_lst with
  | [] -> false
  | hd :: tl -> loop hd tl
;;

(* part 2 *)
let () = read_input () |> total_calibration ~f:calibratable' |> printf "%d\n"
