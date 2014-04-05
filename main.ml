open Hash_parse

type inter = int
type street = int
type time_cost = int
type length = int

type problem = {
  time : int;
  cars : int;
  start : int;
  inters : (inter, float * float) Hashtbl.t;
  streets : (inter, (inter * time_cost * length) list) Hashtbl.t;
  rev_streets : (inter, (inter * time_cost * length) list) Hashtbl.t
}

let parse_problem = 
  let inters = Hashtbl.create 100 in
  let inter_parse inter line =
    let (lat :: long :: _) = 
      List.map float_of_string Str.(split (regexp " ") line) 
    in
    Hashtbl.add inters inter(lat, long)
  in
  let streets = Hashtbl.create 100 in
  let rev_streets = Hashtbl.create 100 in
  let street_parse street line =
    let (from :: to_ :: dir :: time_cost :: length :: _) =
      List.map int_of_string Str.(split (regexp " ") line)
    in
    let add_edge from to_ =
      let inters = 
        try 
          Hashtbl.find streets from 
        with Not_found -> []
      in
      let rev_inters =
        try
          Hashtbl.find rev_streets to_
        with Not_found -> []
      in
      let new_inters = (to_, time_cost, length) :: inters in
      let new_rev_inters = (from, time_cost, length) :: rev_inters in
      Hashtbl.add streets from new_inters;
      Hashtbl.add rev_streets to_ new_rev_inters
    in
    add_edge from to_;
    if dir = 2 then add_edge to_ from
  in
  let (time, cars, start) = task2_parse stdin inter_parse street_parse in
  { time = time ; cars = cars ; start = start; 
    inters = inters ; streets = streets ; rev_streets = rev_streets }
;;

exception Finished

let rec street_in_path from to_ path =
  match path with
  | [] -> false
  | x :: y :: _ when x = from && y = to_ -> true
  | _ :: tail -> street_in_path from to_ tail
;;

let global_visited  : (int * int, unit) Hashtbl.t= Hashtbl.create 1000

let in_any_other x y =
  try
    Hashtbl.find global_visited (x, y) ; true
  with Not_found -> false

let random_path problem =
  let rec random_path round path1 path2 length time curr1 curr2 last_good =    
    let next streets path curr =
      let targets = Hashtbl.find streets curr in 
      (* let targets = List.filter (fun (n, _, _) -> not (List.mem n path)) targets in*)
      if targets = [] then 
        raise Finished
      else
        List.nth targets (Random.int (List.length targets))
    in
    try
      let (next1, time1, length1) = next problem.streets path1 curr1 in
      let (next2, time2, length2) = next problem.rev_streets path2 curr2 in
      let total_time = time1 + time2 + time in
      let length1 = if in_any_other curr1 next1 then 0 else length1 in
      let length2 = if in_any_other curr2 next2 then 0 else length2 in
      let total_length = length1 + length2 + length in
      if not @@ Hashtbl.mem global_visited (curr1, next1) then
        Hashtbl.add global_visited (curr1, next1) ();
      if not @@ Hashtbl.mem global_visited (next2, curr2) then
        Hashtbl.add global_visited (next2, curr2) ();
      let path1 = next1 :: path1 in
      let path2 = next2 :: path2 in
      if total_time > problem.time then
        raise Finished
      else
        let last_good = 
          if next1 = next2 then 
            total_length, (List.rev path1) @ (List.tl path2) 
          else
            if next1 = List.hd path2 then
              total_length, (List.rev path1) @ (List.tl (List.tl path2))
            else
              last_good
        in
        random_path (round + 1) path1 path2 total_length total_time next1 
          next2 last_good
    with Finished -> last_good
  in
  let rec rand_all_vs v acc = 
    if v = 0 then
      acc
    else 
      let v_result = 
        random_path 0 [problem.start] [problem.start] 0 0 
          problem.start problem.start (0, [problem.start])
      in
      rand_all_vs (v - 1) (v_result :: acc)
  in
  rand_all_vs problem.cars []

let write_paths paths =
  let out = open_out "out.txt" in
  Printf.fprintf out "%d\n" (List.length paths);
  List.iter (fun (_, xs) -> 
    Printf.fprintf out "%d\n" (List.length xs);
    List.iter (fun x -> Printf.fprintf out "%d\n" x) xs) paths;
  close_out out

let total_length paths =
  List.fold_left (fun acc (length, xs) -> acc + length) 0 paths

let _ =
  let problem = parse_problem in
(*  Hashtbl.iter (fun x (lat, long) -> 
    Printf.printf "inter : %d %f %f\n" x lat long)
    problem.inters;
  Hashtbl.iter (fun from tos ->
      List.iter (fun (to_, time_cost, length) ->
        Printf.printf "street : %d %d 1 %d %d\n" from to_ time_cost length) tos)
    problem.streets; *)
  Random.init 42;
  let good_one = ref None in
  let good_size = ref 0 in
  for i = 0 to 10000 do
    let result = random_path problem in
    let length = total_length result in
    if length > !good_size then begin 
      good_one := Some result;
      good_size := length;
      write_paths result;
      Format.printf "good : %d@." length
    end;
    (* Printf.printf "total >>>> %d\n" length *)
  done;
  (* Printf.printf "total finish >>>> %d\n" !good_size *)
