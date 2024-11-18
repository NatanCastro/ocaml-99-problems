let range ?(from = 1) ?(step = 1) until =
  let cmp =
    match step with
    | i when i < 0 -> ( > )
    | i when i > 0 -> ( < )
    | _ -> raise (Invalid_argument "step cannot be 0")
  in
  Seq.unfold
    (function i when cmp i until -> Some (i, i + step) | _ -> None)
    from

let is_dividle x y = Float.rem x y = 0.

let is_prime x =
  let aux x y =
    let divisors = range ~from:2 ~step:1 (int_of_float y) |> List.of_seq in
    List.fold_left
      (fun acc divisor -> acc && not (is_dividle x (float_of_int divisor)))
      true divisors
  in
  aux (float_of_int x) (float_of_int x |> (fun x -> x /. 2.) |> Float.round)

let run () =
  print_endline "prime problem";
  let number = 10 in
  if is_prime number then print_endline "is prime"
  else print_endline "is not prime"
