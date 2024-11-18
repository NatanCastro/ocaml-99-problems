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

let run () =
  let n = 5 in
  let res = permutation (Twenty_two.range 1 n) in
  print_list
    (fun x ->
      print_int x;
      print_string " ")
    res;
  print_newline ()
