

let jam_parse lines_per_case line_parse input case_analysis =
  try
    let cases_num = int_of_string @@ input_line input in
    for case = 1 to cases_num  do
      let lines = ref [] in
      let case_param = int_of_string @@ input_line input in
      for line = 1 to (lines_per_case case case_param) do
        lines := (line_parse (case, case_param, line, input_line input)) :: !lines
      done;
      (case_analysis case case_param lines)
    done;
  with End_of_file -> 
    Printf.printf "Warning : end of file met !\n"
;;

let task1_parse input empty_item parse_item =
  let (y_str :: x_str :: _) = Str.(split (regexp " ") @@ input_line input) in 
  let (y, x) = (int_of_string y_str), (int_of_string x_str) in
  let result = Array.make_matrix y x empty_item in
  for i = 0 to (y - 1) do
    let line = input_line input in
    for j =0 to (x - 1) do
      result.(i).(j) <- parse_item line.[j]
    done;
  done;
  x, y, result
;; 

let task2_parse input parse_inter parse_street parse_params = 
  let (inters :: streets :: time :: cars :: start :: _) =
    List.map int_of_string Str.(split (regexp " ") @@ input_line input)
  in
  for inter = 0 to (inters - 1) do
    parse_inter inter (input_line input)
  done;
  for street = 0 to (streets - 1) do
    parse_street street (input_line input)
  done;
  (time, cars, start)
    
