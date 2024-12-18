open! Core

let read_input () =
  In_channel.read_all "input/day9.txt"
  |> String.strip ~drop:(fun c -> Char.equal '\n' c)
  |> String.to_list
  |> List.map ~f:Char.get_digit_exn
;;

let generate_file_struct digit_lst =
  let rec loop acc id = function
    | [] -> acc
    | [ c ] ->
      Doubly_linked.insert_last acc (Some id, c) |> ignore;
      acc
    | file :: space :: tl ->
      let _ = Doubly_linked.insert_last acc (Some id, file) in
      if space > 0 then ignore (Doubly_linked.insert_last acc (None, space));
      loop acc (id + 1) tl
  in
  loop (Doubly_linked.create ()) 0 digit_lst
;;

let rec fill file_struct entry =
  let id, size = Doubly_linked.Elt.value entry in
  match id with
  | None -> ()
  | Some i ->
    (match Doubly_linked.find_elt file_struct ~f:(fun (d, _) -> is_none d) with
     | None -> invalid_arg "No empty slot available"
     | Some elt ->
       let _, free_slot = Doubly_linked.Elt.value elt in
       if free_slot > size
       then (
         Doubly_linked.Elt.set elt (Some i, size);
         Doubly_linked.insert_after file_struct elt (None, free_slot - size) |> ignore;
         Doubly_linked.Elt.set entry (None, size))
       else if free_slot = size
       then (
         Doubly_linked.Elt.set elt (Some i, size);
         Doubly_linked.Elt.set entry (None, size))
       else (
         Doubly_linked.Elt.set elt (Some i, free_slot);
         Doubly_linked.Elt.set entry (Some i, size - free_slot);
         let _ = Doubly_linked.insert_after file_struct entry (None, free_slot) in
         fill file_struct entry))
;;

let rev_loop file_struct ~f =
  let rec loop curr_elt =
    let () = f file_struct curr_elt in
    match Doubly_linked.prev file_struct curr_elt with
    | None -> ()
    | Some elt -> loop elt
  in
  let curr_elt = Doubly_linked.last_elt file_struct |> Option.value_exn in
  loop curr_elt
;;

let fill_all file_struct = rev_loop file_struct ~f:fill

let checksum file_struct =
  Doubly_linked.fold file_struct ~init:(0, 0) ~f:(fun (position, sum) (id, size) ->
    match id with
    | None -> position + size, sum
    | Some id ->
      let tmp = List.init size ~f:(fun i -> position + i) in
      position + size, List.fold tmp ~init:sum ~f:(fun acc pos -> (id * pos) + acc))
  |> Tuple2.get2
;;

(* part 1 *)
let () =
  let file_struct = read_input () |> generate_file_struct in
  let () = fill_all file_struct in
  file_struct |> checksum |> printf "%d\n"
;;

let is_behind lst lelt relt =
  let rec loop elt =
    match Doubly_linked.next lst elt with
    | None -> false
    | Some e -> if Doubly_linked.Elt.equal e relt then true else loop e
  in
  loop lelt
;;

let fill_block file_struct entry =
  let id, size = Doubly_linked.Elt.value entry in
  match id with
  | None -> ()
  | Some i ->
    (match
       Doubly_linked.find_elt file_struct ~f:(fun (d, s) -> is_none d && s >= size)
     with
     | None -> ()
     | Some elt ->
       if is_behind file_struct entry elt
       then ()
       else (
         let _, free_slot = Doubly_linked.Elt.value elt in
         if free_slot > size
         then (
           let () = Doubly_linked.Elt.set elt (Some i, size) in
           let () =
             Doubly_linked.insert_after file_struct elt (None, free_slot - size) |> ignore
           in
           Doubly_linked.Elt.set entry (None, size))
         else (
           let () = Doubly_linked.Elt.set elt (Some i, size) in
           Doubly_linked.Elt.set entry (None, size))))
;;

let fill_block_all file_struct = rev_loop file_struct ~f:fill_block

(* part 2 *)
let () =
  let file_struct = read_input () |> generate_file_struct in
  let () = fill_block_all file_struct in
  file_struct |> checksum |> printf "%d\n"
;;
