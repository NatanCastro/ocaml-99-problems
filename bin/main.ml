let print_problems () =
  print_endline "select a problem";
  print_endline "1 - List problems";
  print_endline
    "17. Split a list into two parts; the length of the first part is given";
  print_endline "18. Extract a slice from a list.";
  print_endline "19. Rotate a list N places to the left.";
  print_endline "20. Remove the K'th element from a list.";
  print_endline "21. Insert an element at a given position into a list.";
  print_endline
    "22. Create a list containing all integers within a given range.";
  print_endline
    "23. Extract a given number of randomly selected elements from a list.";
  print_endline "24. Lotto: Draw N different random numbers from the set 1..M.";
  print_endline "25. Generate a random permutation of the elements of a list.";
  print_endline "2 - Arithmetic problems";
  print_endline "31. Determine whether a given integer number is prime.";
  print_endline
    "32. Determine the greatest common divisor of two positive integer numbers.";
  print_endline
    "33. Determine whether two positive integer numbers are coprime.";
  print_endline "0. exit";
  print_endline "enter a problem number: ";
  let input = read_line () in
  print_newline ();
  input

let run_problem problem =
  match problem with
  | "0" -> exit 0
  | "17" -> List_problems.Seventeen.run ()
  | "18" -> List_problems.Eighteen.run ()
  | "19" -> List_problems.Nineteen.run ()
  | "20" -> List_problems.Twenty.run ()
  | "21" -> List_problems.Twenty_one.run ()
  | "22" -> List_problems.Twenty_two.run ()
  | "23" -> List_problems.Twenty_three.run ()
  | "24" -> List_problems.Twenty_four.run ()
  | "25" -> List_problems.Twenty_five.run ()
  | "31" -> Arithmetic_problems.Prime.run ()
  | "32" -> Arithmetic_problems.Gcd.run ()
  | "33" -> Arithmetic_problems.Coprime.run ()
  | _ -> print_endline "invalid input"

let () =
  let input = print_problems () in
  run_problem input
