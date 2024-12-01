open! Core

let read_input () =
  In_channel.with_file "input/day1.txt" ~f:(fun file ->
    In_channel.fold_lines file ~init:([], []) ~f:(fun (lacc, racc) line ->
      let l = Int.of_string @@ String.slice line 0 5 in
      let r = Int.of_string @@ String.slice line 8 13 in
      l :: lacc, r :: racc))
;;

(* part 1 *)
let distance (llst, rlst) =
  let sortedl = List.sort llst ~compare:Int.compare in
  let sortedr = List.sort rlst ~compare:Int.compare in
  List.zip_exn sortedl sortedr
  |> List.fold ~init:0 ~f:(fun acc (l, r) -> Int.abs (l - r) + acc)
;;

let part1 () = read_input () |> distance |> printf "part1 ans:\t%d\n"

(* part 2 *)
let similarity_score (llst, rlst) =
  List.fold llst ~init:0 ~f:(fun acc l ->
    let num_of_existence = List.count rlst ~f:(fun r -> r = l) in
    acc + (num_of_existence * l))
;;

let part2 () = read_input () |> similarity_score |> printf "part2 ans:\t%d\n"

let () =
  part1 ();
  part2 ()
;;
