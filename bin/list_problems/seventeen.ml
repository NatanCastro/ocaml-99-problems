(* 
  17. Split a list into two parts; the length of the first part is given. (easy)
  If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty.
*)


let split_list l len =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l else aux (i-1) (h :: acc) t  in
 aux len [] l

let print_list l f = List.iter (fun x -> f x) l

let run () =
  let l = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
  let count = 8 in
  let (left, right) = split_list l count in
  print_list left (fun x -> Printf.printf "%d " x);
  print_string "\n";
  print_list right (fun x -> Printf.printf "%d " x);
  print_string "\n"
