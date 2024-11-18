let permutation l =
  Random.self_init ();
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t
  in
  let extract_rand list len = extract [] (Random.int len) list in
  let rec aux n acc list len =
    if n = 0 then acc
    else
      let picked, rest = extract_rand list len in
      aux (n - 1) (picked :: acc) rest (len - 1)
  in
  let len = List.length l in
  aux len [] l len

let print_list f l = List.iter (fun x -> f x) l

let print_problem_example () =
  print_string "Example:\n";
  print_string "permutation [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]\n";
  print_string "  [3; 4; 1; 10; 5; 9; 2; 7; 6; 8]\n";
  print_newline ()

let run () =
  print_problem_example ();
  let l = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  let res = permutation l in
  print_list
    (fun x ->
      print_int x;
      print_string " ")
    res;
  print_newline ()
