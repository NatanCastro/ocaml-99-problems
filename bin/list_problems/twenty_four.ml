open Twenty_two

let lotto_select count stop =
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
  let list = 1 -- stop in
  let len = List.length list in
  aux (min stop count) [] list len

let print_list f l = List.iter (fun x -> f x) l

let print_problem_example () =
  print_string "Example:\n";
  print_string "lotto_select 4 100\n";
  print_string "  [6; 2; 90; 10]\n";
  print_newline ()

let run () =
  print_problem_example ();
  let n = 4 in
  let res = lotto_select n 100 in
  print_list
    (fun x ->
      print_int x;
      print_string " ")
    res;
  print_newline ()
