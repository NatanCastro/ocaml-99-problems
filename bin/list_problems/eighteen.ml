let slice l start stop =
  let rec aux l start stop acc =
    match l with
    | [] -> acc
    | h :: t ->
        if start = 0 && stop > 0 then aux t 0 (stop - 1) (h :: acc)
        else if stop = 0 then List.rev acc
        else aux t (start - 1) (stop - 1) acc
  in
  let list_length = List.length l in
  let valid_range =
    start <= stop || start <= list_length || stop <= list_length
  in
  match valid_range with true -> aux l start stop [] | false -> []

let print_list l f = List.iter (fun x -> f x) l

let print_problem_example () =
  print_endline "extract a slice from a list";
  print_endline "example:";
  print_endline "list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]";
  print_endline "start = 3";
  print_endline "stop = 6";
  print_endline "slice = [4; 5; 6]";
  print_newline ()

let run () =
  print_problem_example ();
  print_newline ();
  let l = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  let start = 3 in
  let stop = 6 in
  let slice = slice l start stop in
  print_list slice (fun x ->
      print_int x;
      print_string "; ");
  print_newline ()
