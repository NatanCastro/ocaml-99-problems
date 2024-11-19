let prime_factors n =
  let rec aux acc div n =
    if n = 1. then acc
    else if int_of_float div |> Prime.is_prime |> not then aux acc (div +. 1.) n
    else if mod_float n div <> 0. then aux acc (div +. 1.) n
    else aux (acc @ [ int_of_float div ]) div (n /. div)
  in
  aux [] 2. (float_of_int n)

let print_list f l = List.iter (fun x -> Printf.printf "%s" (f x)) l

let print_problem_example () =
  print_endline "Example:";
  print_endline "prime_factors 120";
  print_endline "[2, 2, 2, 3, 5]";
  print_newline ()

let run () =
  print_problem_example ();
  prime_factors 120 |> print_list (fun x -> string_of_int x ^ " ");
  print_newline ()
