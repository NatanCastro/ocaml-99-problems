let () = 
  print_endline "select a problem";
  print_endline "0. exit";
  print_endline "17. seventeen problem";
  print_endline "18. eighteen problem";
  print_endline "19. nineteen problem";
  print_endline "20. twenty problem";
  print_endline "21. twenty one problem";
  print_endline "22. twenty two problem";
  print_endline "23. twenty three problem";
  print_endline "24. twenty four problem";
  print_endline "25. twenty five problem";

  let input = read_line () in

  match input with
    | "0" -> exit 0
    | "17" -> print_endline "seventeen problem"; List_problems.Seventeen.run ()
    | "18" -> print_endline "eighteen problem"; List_problems.Eighteen.run ()
    | "19" -> print_endline "nineteen problem"; List_problems.Nineteen.run ()
    | "20" -> print_endline "twenty problem"; List_problems.Twenty.run ()
    | "21" -> print_endline "twenty one problem"; List_problems.Twenty_one.run ()
    | "22" -> print_endline "twenty two problem"; List_problems.Twenty_two.run ()
    | "23" -> print_endline "twenty three problem"; List_problems.Twenty_three.run ()
    | "24" -> print_endline "twenty four problem"; List_problems.Twenty_four.run ()
    | "25" -> print_endline "twenty five problem"; List_problems.Twenty_five.run ()
    | _ -> print_endline "invalid input";
