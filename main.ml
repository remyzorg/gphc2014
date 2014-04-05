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
  streets : (inter, (inter * time_cost * length) list) Hashtbl.t
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
      let new_inters = (to_, time_cost, length) :: inters in
      Hashtbl.add streets from new_inters 
    in
    add_edge from to_;
    if dir = 2 then add_edge to_ from
  in
  let (time, cars, start) = task2_parse stdin inter_parse street_parse in
  { time = time ; cars = cars ; start = start; 
    inters = inters ; streets = streets }
;;

let random_path problem =
  Random.init 42;
  let random_path path1 path2 time curr1 curr2 last_good =    
    let next curr =
      let targets = Hashtbl.find curr in 
      List.nth (Random.int (List.length targets))
    in
    let (next1, time1, length1) = next curr1 in
    let (next2, time2, length2) = next curr2 in
    let total_time = time1 + time2 + time in
    let path1 = next1 :: path1 in
    let path2 = next2 :: path2 in
    if total_time > problem.time then
      last_good
    else
      let last_good = 
        if next1 = next2 then 
          last_good = (List.rev path1) @ (List.tail path2) 
        else
          last_good
      in
      random_path path1 path2 total_time next1 next2 last_good
  in
  let rand_all_vs v acc = 
    if v = 0 then
      acc
    else 
      let v_result = 
        random_path [] [] 0 problem.start problem.start [problem.start]
      in
      rand_all_vs (v - 1) (v_result :: acc)
  in
  rand_all_vs problem.cars []
  
      
      

let compute_random problem = 
  

let _ =
  let problem = parse_problem in
  Hashtbl.iter (fun x (lat, long) -> 
    Printf.printf "inter : %d %f %f\n" x lat long)
    problem.inters;
  Hashtbl.iter (fun from tos ->
      List.iter (fun (to_, time_cost, length) ->
        Printf.printf "street : %d %d 1 %d %d\n" from to_ time_cost length) tos)
    problem.streets;
  let result = compute_random problem;
