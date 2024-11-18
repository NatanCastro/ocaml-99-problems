let phi n =
  let rec aux acc d =
    if d < n then aux (if Coprime.coprime n d then acc + 1 else acc) (d + 1)
    else acc
  in
  if n = 1 then 1 else aux 0 1

let print_list l f = List.iter (fun x -> f x) l

let print_problem_example () =
  print_string "Example:\n";
  print_string "phi 10\n";
  print_string "  4\n";
  print_newline ()

let run () =
  print_problem_example ();
  let n = 10 in
  let res = phi n in
  print_int res;
  print_newline ()
