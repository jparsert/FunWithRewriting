open List
open Term
open Utils


type t = (Term.t * Term.t)

let string_of_substitution ((t1,t2):t) = (Term.string_of_term t1) ^ Utils.r_arrow_b_smb ^ (Term.string_of_term t2)

let string_of_substitution_list lst =  fold_left (fun a b -> if a="" then b else a ^ "," ^ b) "" @@ map string_of_substitution lst
