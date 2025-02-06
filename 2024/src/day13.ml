open! Core

let extract str =
  let reg = Re.Pcre.regexp "[0-9]+" in
  let num_lst = Re.matches reg str |> List.map ~f:Int.of_string in
  List.hd_exn num_lst, List.last_exn num_lst
;;

let read_input () =
  In_channel.read_lines "input/day13.txt"
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.groupi ~break:(fun i _ _ -> i mod 3 = 0)
  |> List.map ~f:(fun lines -> List.map lines ~f:extract)
  |> List.map ~f:(fun lst -> List.nth_exn lst 0, List.nth_exn lst 1, List.nth_exn lst 2)
;;

let solve ((x1, y1), (x2, y2), (x, y)) =
  let a = ((x * y2) - (y * x2)) / ((x1 * y2) - (x2 * y1)) in
  let b = ((x1 * y) - (x * y1)) / ((x1 * y2) - (x2 * y1)) in
  if (a * x1) + (b * x2) = x && (a * y1) + (b * y2) = y then Some (a, b) else None
;;

let tokens = function
  | Some (a, b) -> (3 * a) + b
  | None -> 0
;;

let find_min_tokens machines =
  machines |> List.map ~f:solve |> List.map ~f:tokens |> List.fold ~init:0 ~f:( + )
;;

(* part 1 *)
let () = read_input () |> find_min_tokens |> printf "%d\n"

(* part 2 *)
let () =
  read_input ()
  |> List.map ~f:(fun (a, b, (x, y)) -> a, b, (x + 10000000000000, y + 10000000000000))
  |> find_min_tokens
  |> printf "%d\n"
;;
