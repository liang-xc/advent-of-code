open Core

let read_input () = In_channel.read_lines "input/day4.txt"

let pad grid =
  let pad_lr = List.map grid ~f:(fun row -> "***" ^ row ^ "***") in
  let n = pad_lr |> List.hd_exn |> String.length in
  let padding = String.init n ~f:(fun _ -> '*') in
  [ padding; padding; padding ] @ pad_lr @ [ padding; padding; padding ]
;;

let get_char grid m n = String.nget (List.nth_exn grid m) n

(** [examine_x] with location [m], [n] on the padded [grid]
    the grid should be padded to avoid corner cases *)
let examine_x grid m n =
  [ String.of_list
      [ get_char grid (m - 1) n; get_char grid (m - 2) n; get_char grid (m - 3) n ]
  ; String.of_list
      [ get_char grid (m - 1) (n + 1)
      ; get_char grid (m - 2) (n + 2)
      ; get_char grid (m - 3) (n + 3)
      ]
  ; String.of_list
      [ get_char grid m (n + 1); get_char grid m (n + 2); get_char grid m (n + 3) ]
  ; String.of_list
      [ get_char grid (m + 1) (n + 1)
      ; get_char grid (m + 2) (n + 2)
      ; get_char grid (m + 3) (n + 3)
      ]
  ; String.of_list
      [ get_char grid (m + 1) n; get_char grid (m + 2) n; get_char grid (m + 3) n ]
  ; String.of_list
      [ get_char grid (m + 1) (n - 1)
      ; get_char grid (m + 2) (n - 2)
      ; get_char grid (m + 3) (n - 3)
      ]
  ; String.of_list
      [ get_char grid m (n - 1); get_char grid m (n - 2); get_char grid m (n - 3) ]
  ; String.of_list
      [ get_char grid (m - 1) (n - 1)
      ; get_char grid (m - 2) (n - 2)
      ; get_char grid (m - 3) (n - 3)
      ]
  ]
  |> List.fold ~init:0 ~f:(fun acc s -> if String.equal s "MAS" then acc + 1 else acc)
;;

let word_search grid =
  let padded = pad grid in
  List.mapi padded ~f:(fun i s ->
    s
    |> String.to_list
    |> List.mapi ~f:(fun j c -> if Char.equal c 'X' then examine_x padded i j else 0)
    |> List.fold ~init:0 ~f:( + ))
  |> List.fold ~init:0 ~f:( + )
;;

(* part 1 *)
let () = read_input () |> word_search |> printf "%d\n"

let examine_a grid m n =
  let nw = get_char grid (m - 1) (n - 1) in
  let ne = get_char grid (m - 1) (n + 1) in
  let se = get_char grid (m + 1) (n + 1) in
  let sw = get_char grid (m + 1) (n - 1) in
  let open Char in
  ((nw = 'M' && se = 'S') || (nw = 'S' && se = 'M'))
  && ((ne = 'M' && sw = 'S') || (ne = 'S' && sw = 'M'))
;;

let mas_search grid =
  let padded = pad grid in
  List.mapi padded ~f:(fun i s ->
    s
    |> String.to_list
    |> List.mapi ~f:(fun j c -> if Char.equal c 'A' then examine_a padded i j else false)
    |> List.fold ~init:0 ~f:(fun acc b -> if b then acc + 1 else acc))
  |> List.fold ~init:0 ~f:( + )
;;

(* part 2 *)
let () = read_input () |> mas_search |> printf "%d\n"
