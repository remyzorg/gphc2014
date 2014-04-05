open Hash_parse

type item = Dot | Sharp | Drawn

type op = 
  | Paint of int * int * int 
  | Erase of int * int

exception Sharp_found of int * int

let next_sharp w h matrix = 
  try
    for i = 0 to (h - 1) do
      for j = 0 to (w - 1) do
        match matrix.(i).(j) with 
        | Sharp -> raise @@ Sharp_found (i, j)
        | _ -> ()
      done;
    done;
  None
  with Sharp_found (i, j) -> Some (i, j)
;;

let sharps_in w h matrix x y square_w =
  let count = ref 0 in
  for i = x to (x + square_w - 1) do
    for j = y to (y + square_w - 1) do
      match matrix.(i).(j) with
      | Sharp -> count := !count + 1
      | _ -> ()
    done;
  done;
  !count
;;

let paint ops w h matrix x y size =
  let paint_w = (size - 1) / 2 in
  let ops = ref ops  in
  for i = x to (x + size - 1) do
    for j = y to (y + size - 1) do
      match matrix.(i).(j) with
      | Sharp -> matrix.(i).(j) <- Drawn
      | Dot -> ops := Erase(i, j) :: !ops
      | _ -> ()
    done;
  done; Paint(paint_w + x, paint_w + y, paint_w) :: !ops

let rec compute ops w h matrix =
  try
    let Some (x, y) = next_sharp w h matrix in
    let rec try_paint size prev_ratio prev_size =
      try
        let sharps_count = sharps_in w h matrix x y size in
        let ratio = (float sharps_count) /. (float (1 + (sharps_count - (x * y)))) in
        if ratio >= prev_ratio then
          try_paint (size + 2) ratio size
        else
         prev_size
      with Invalid_argument _ ->
        prev_size
    in
    let good_size = try_paint 3 1. 1 in
    Printf.printf "good size of %d %d is %d\n" x y good_size;
    let new_ops = paint ops w h matrix x y good_size in
    compute new_ops w h matrix
  with Match_failure _ -> ops

let _ = 
  let empty_item = Dot in
  let parse_item c =
    if c = '#' then Sharp else Dot
  in
  let width, height, result =
    task1_parse stdin empty_item parse_item
  in
  let ops = compute [] width height result in
  List.iter (fun x -> 
    match x with
    | Paint (x, y, w) -> Printf.printf "PAINTSQ %d %d %d\n" x y w
    | Erase (x, y) -> Printf.printf "ERASECELL %d %d\n" x y
  ) ops
(*  for i = 0 to (height - 1) do
    for j = 0 to (width - 1) do
      match result.(i).(j) with
      | Dot -> Printf.printf "0"
      | Sharp -> Printf.printf "1"
      | _ -> Printf.printf "x"
    done;
    print_string "\n"
  done *)
  
  
