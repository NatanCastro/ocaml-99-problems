let insert_at item pos l =
  let rec aux item pos acc = function
    | [] -> List.rev acc
    | h :: t ->
        if pos = 0 then aux item (pos - 1) (item :: h :: acc) t
        else aux item (pos - 1) (h :: acc) t
  in
  let len = List.length l - 1 in
  if pos > len then l @ [ item ]
  else if pos < 0 then
    let new_pos = (pos mod len) + len in
    aux item new_pos [] l
  else aux item pos [] l

let print_list f l = List.iter (fun x -> f x) l

let run () =
  let l = insert_at 1 7 [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  print_list (fun x -> Printf.printf "%d " x) l;
  print_newline ();

  let l = insert_at 1 (-16) [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  print_list (fun x -> Printf.printf "%d " x) l;
  print_newline ()
