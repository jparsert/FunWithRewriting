

let bottom_symbol = "⊥"

let max_of_int_list lst =
  let rec aux acc = function
    | [] -> acc
    | x::xs -> if x > acc then aux x xs else aux acc xs
  in
  aux 0 lst
