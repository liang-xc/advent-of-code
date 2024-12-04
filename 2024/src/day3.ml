open Core

let read_input () = In_channel.read_all "input/day3.txt"

let extract str =
  let reg =
    let open Re in
    seq
      [ str "mul("
      ; group (repn digit 1 (Some 3))
      ; char ','
      ; group (repn digit 1 (Some 3))
      ; char ')'
      ]
    |> compile
  in
  Re.matches reg str
;;

(** [eval] a mul(X,Y) expression *)
let eval str =
  let values = String.slice str 4 (String.length str - 1) in
  let lst = String.split values ~on:',' in
  let x = lst |> List.hd_exn |> Int.of_string in
  let y = lst |> List.last_exn |> Int.of_string in
  x * y
;;

let add_up str = str |> extract |> List.map ~f:eval |> List.fold ~init:0 ~f:( + )

(* part 1 *)
let () = read_input () |> add_up |> fun i -> printf "%d\n" i

let sep_do str =
  let reg =
    let open Re in
    str "do()" |> compile
  in
  Re.split reg str
;;

let sep_dont str =
  let reg =
    let open Re in
    str "don't()" |> compile
  in
  Re.split reg str
;;

(* part 2 *)
let () =
  read_input ()
  |> sep_do
  |> List.map ~f:sep_dont
  |> List.map ~f:List.hd_exn
  |> List.map ~f:add_up
  |> List.fold ~init:0 ~f:( + )
  |> fun i -> printf "%d\n" i
;;
