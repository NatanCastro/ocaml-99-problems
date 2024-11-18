(**
   23. Extract a given number of randomly selected elements from a list. (medium)
      The selected items shall be returned in a list. We use the Random module but do not initialize it with Random.self_init for reproducibility.

    # rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
    - : string list = ["g"; "d"; "a"]
  *)

let rand_select list n =
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
  let len = List.length list in
  aux (min n len) [] list len

let print_list f l = List.iter (fun x -> f x) l

let run () =
  let n = 4 in
  let res = rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] n in
  print_list
    (fun x ->
      print_string x;
      print_string " ")
    res;
  print_newline ()
