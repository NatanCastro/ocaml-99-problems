let split_list l len =
  let rec aux i acc = function
    | [] -> (List.rev acc, [])
    | h :: t as l ->
        if i = 0 then (List.rev acc, l) else aux (i - 1) (h :: acc) t
  in
  aux len [] l

let print_list l f = List.iter (fun x -> f x) l

let print_problem_example () =
  print_endline "split a list into two parts";
  print_endline "the length of the first part is given";
  print_endline "example:";
  print_endline "list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]";
  print_endline "length = 8";
  print_endline "first part = [1; 2; 3; 4; 5; 6; 7; 8]";
  print_endline "second part = [9; 10]";
  print_newline ()

let run () =
  print_problem_example ();
  print_newline ();
  let l = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  let len = 8 in
  let first, second = split_list l len in
  print_list first (fun x ->
      print_int x;
      print_string "; ");
  print_newline ();
  print_list second (fun x ->
      print_int x;
      print_string "; ");
  print_newline ()
