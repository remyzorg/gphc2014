
open Format




let read () =
  let rl = ref [] in
  try
    let rec loop () =
      let line = input_line stdin in
      rl := line :: !rl; loop ()
    in loop ()
  with End_of_file -> List.rev !rl





let pp_list l =
  List.iter (Format.printf "%s\n") l

let split = Str.split (Str.regexp_string " ");;

module IntMap = Map.Make (struct
  type t = int
  let compare = compare
end)

module RelMap = Map.Make (struct
  type t = int * int
  let compare = compare
end)

let cons l =
  let n, m, tmax, cars, origin =
    match split (List.hd l) with
    | ni :: nr :: time :: cars :: origin :: [] ->
      (int_of_string ni, int_of_string nr, time, cars, origin)
    | _ -> assert false
  in
  let rec step (vertexs, edges) i l =
    match l with
    | [] -> vertexs, edges
    | h :: t ->
      let c =
        if i < n then
          let lat, long =
            match split h with | lat :: long :: [] -> lat, long | _ -> assert false
          in
          IntMap.add i ((float_of_string lat, float_of_string long), []) vertexs, edges
        else
          let v1, v2, ((orien, t, dist) as data) = begin match split h with
          | v1 :: v2 :: orien :: time :: dist :: [] ->
            int_of_string v1, int_of_string v2,
             (int_of_string orien, int_of_string time, dist)
          | _ -> assert false
          end in
          let new_map =
            if orien = 1 then
              let coo, l = try IntMap.find v1 vertexs with Not_found -> (0., 0.), [] in
              IntMap.add v1 (coo, v2 :: l) vertexs
            else
              let coo1, l1 = try IntMap.find v1 vertexs with Not_found -> (0., 0.), [] in
              let coo2, l2 = try IntMap.find v2 vertexs with Not_found -> (0., 0.), [] in
              IntMap.add v2 (coo2, v1 :: l2) (IntMap.add v1 (coo1, v2 :: l1) vertexs)
          in
          new_map, RelMap.add (max v1 v2, min v1 v2) data edges
      in
      step c (i + 1) t
  in
  n, m, int_of_string tmax, int_of_string cars,
  int_of_string origin, step (IntMap.empty, RelMap.empty) 0 (List.tl l)



module IntSet = Set.Make (struct
  type t = int
  let compare = compare
end)


module CoupleSet = Set.Make (struct
  type t = int * int
  let compare = compare
end)

let status = Hashtbl.create 8

exception Continue
exception Done


let directions =
  let h = Hashtbl.create 4 in
  Hashtbl.add h 1 0;
  Hashtbl.add h 2 0;
  Hashtbl.add h 3 0;
  Hashtbl.add h 4 0;
  h


let dir (lat1, long1) (lat2, long2) =
  if lat1 > lat2 then
    if long1 > long2 then 1
    else 2
  else if long1 > long2 then 3
  else 4



(* let score visited source target slong slat tlong tlat = *)
(*   let dirheur = Hashtbl.find directions (dir (slong, slat) (tlong, tlat)) in *)
(*   let seen = if CoupleSet.mem (max source target, min source target) visited *)
(*     then 1000 else 0 in *)
(*   (if dirheur = 0 then 0 else 100 / dirheur) + seen *)

(*           let rec find_next heur minimum = function *)
(*             | [] -> *)
(*               if heur = max_int then begin *)
(*                 Random.self_init (); *)
(*                 List.nth neightbour (Random.int (List.length neightbour)) *)
(*                 end *)
(*               else minimum *)
(*             | h :: t -> *)
(*               let (hlong, hlat), _= IntMap.find h vertexes in *)
(*               let newheur = score visited current h long lat hlong hlat in *)
(*               if newheur < heur then find_next newheur h t *)
(*               else find_next heur minimum t *)
(*           in *)


let update cars start vertexes edges tmax =
  for i = 1 to cars do
    Hashtbl.add status i (start, 0, [start])
  done;
  let rec update visited =
    let continue = ref false in
    let setref = ref visited in
    for k = 1 to 8 do
      let current, elapsed, road = Hashtbl.find status k in
      let mycontinue = elapsed <= tmax in
      continue := !continue || mycontinue;
      if mycontinue then
        try
          let (long, lat), neightbour = IntMap.find current vertexes in

          let rec find_next heur minimum = function
            | [] ->
              if heur = max_int then begin
                Random.self_init ();
                List.nth neightbour (Random.int (List.length neightbour))
                end
              else minimum
            | h :: t ->
              if CoupleSet.mem (max current h, min h current) !setref
              then find_next heur minimum t else
              let (hlong, hlat), _ = IntMap.find h vertexes in
              let dirint = dir (long, lat) (hlong, hlat) in
              let dirheur = Hashtbl.find directions dirint in
              if dirheur < heur then find_next dirheur h t
              else find_next heur minimum t
          in
          let n = find_next max_int (List.hd neightbour) neightbour in

          setref := CoupleSet.add (max current n, min n current) !setref;
                (* printf "//BEFORE %d\n" elapsed; *)
          let _, cost, _ = RelMap.find (max current n, min current n) edges in
          if cost + elapsed > tmax then continue := false
          else Hashtbl.replace status k (n, cost + elapsed, n :: road)

        with Not_found -> assert false
    done;
  (* Hashtbl.iter (fun car (_, elapsed, road) -> *)
  (*   printf "(%d, %d) : %d @\n" car elapsed tmax; *)
  (* ) status; *)
    if !continue then update !setref
  in update (CoupleSet.empty)

(* 4516 10500 *)





let create_out cars start =
  printf "%d\n" cars;
  Hashtbl.iter (fun car (_, _, road) ->
    printf "%d\n" (List.length road);
    List.iter (printf "%d\n") (List.rev road)
  ) status





let _ =
  let n, m, tmax, cars, start, (vs, es) = cons (read ()) in
  (* RelMap.iter (fun (v1, v2) _ -> *)
  (*   if v1 = 4516 || v2 = 4516 then printf "%d %d\n " v1 v2; *)
  (* ) es; *)
  update cars start vs es tmax;
  create_out cars start
