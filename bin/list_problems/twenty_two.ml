let ( -- ) start stop =
  let rec aux acc dir i =
    if i = stop then i :: acc |> List.rev else aux (i :: acc) dir (i + dir)
  in
  if start > stop then aux [] (-1) start else aux [] 1 start

let print_list f l = List.iter (fun x -> f x) l

let print_problem_example () =
  print_string "Example:\n";
  print_string "range 1 10\n";
  print_string "  [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]\n";
  print_string "range 10 1\n";
  print_string "  [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]\n";
  print_newline ()

let run () =
  print_problem_example ();
  let res = 1 -- 10 in
  print_list
    (fun x ->
      print_int x;
      print_string " ")
    res;
  print_newline ();

  let res = 10 -- 1 in
  print_list
    (fun x ->
      print_int x;
      print_string " ")
    res;
  print_newline ()
