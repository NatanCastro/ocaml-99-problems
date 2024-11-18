let coprime x y = Gcd.gcd x y = 1

let print_problem_example () =
  print_string "Example:\n";
  print_string "coprime 10 12\n";
  print_string "  false\n";
  print_newline ()

let run () =
  print_problem_example ();
  let x = 10 in
  let y = 12 in
  let res = coprime x y in
  Printf.printf "%b\n" res;
  print_newline ()
