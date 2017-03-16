open List
open Term
open Utils


type t = (Term.variable * Term.t)

let string_of_substitution ((t1,t2):t) = (Term.string_of_variable t1) ^ Utils.r_arrow_b_smb ^ (Term.string_of_term t2)

let string_of_substitution_list lst =  fold_left (fun a b -> if a="" then b else a ^ "," ^ b) "" @@ map string_of_substitution lst

let rec apply_subst ((trigger, result) as t : t) (term:Term.t) =
  match term with
  | Var v -> if trigger=v then result else (Var v)
  | Fun (f, args) -> Fun (f, List.map (apply_subst t) args)

let rec find_matching_subst (f:Term.variable) = function
  | [] -> raise Not_found
  | (v , t)::xs -> if f=v then (v,t) else find_matching_subst f xs

let rec apply_substitutions (subts : t list) (term:Term.t) =
  match term with
  | Fun (f, args) -> Fun (f, List.map (apply_substitutions subts) args)
  | Var v -> match
      try Some (find_matching_subst v subts)
      with _ -> None
    with
    | None -> Var v
    | Some (v , replace) -> replace
