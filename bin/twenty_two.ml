
let range start stop =
  let rec aux acc dir i =
    if i = stop then
      i :: acc |> List.rev
    else
      aux (i :: acc) dir (i + dir)
  in
  if start > stop then aux [] (-1) start else  aux [] 1 start

let print_list f l =
  List.iter (fun x -> f x) l

let run () =
  let n = 9 in
  let res = range 1 n in
  print_list (fun x -> print_int x; print_string " ") res;
  print_newline ();

  let res = range n 1 in
  print_list (fun x -> print_int x; print_string " ") res;
  print_newline ()

