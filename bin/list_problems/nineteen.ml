let rotate l n =
  let rec aux l n acc =
    match l with
    | [] -> acc
    | h :: t ->
        if n = 1 then t @ List.rev (h :: acc) else aux t (n - 1) (h :: acc)
  in
  let count = if n > 0 then n else n + List.length l in
  aux l count []

let print_list f l = List.iter (fun x -> f x) l

let print_problem_example () =
  print_string "Example:\n";
  print_string "rotate [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] 3\n";
  print_string "  [10; 1; 2; 3; 4; 5; 6; 7; 8; 9]\n";
  print_string "rotate [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] -3\n";
  print_string "  [9; 10; 1; 2; 3; 4; 5; 6; 7; 8]\n";
  print_newline ()

let run () =
  print_problem_example ();
  let l = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  print_list
    (fun x ->
      print_int x;
      print_string " ")
    l;
  print_newline ();
  print_list
    (fun x ->
      print_int x;
      print_string " ")
    (rotate l 3);
  print_newline ()
