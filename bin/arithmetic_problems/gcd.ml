let rec gcd x y = if y = 0 then x else gcd y (x mod y)

let print_problem_example () =
  print_string "Example:\n";
  print_string "gcd 12 18\n";
  print_string "  6\n";
  print_string "gcd 18 12\n";
  print_string "  6\n";
  print_newline ()

let run () =
  print_problem_example ();
  let x = 12 in
  let y = 18 in
  let res = gcd x y in
  print_int res;
  print_newline ()
let rec gcd a b =
  match b with
  | 0 -> a
  | _ ->
    gcd a (a mod b)

let run () =
  let common_divisors = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  List.iter (fun a -> gcd a 9 |> print_int; print_newline ()) common_divisors;
