open Term

open Substitution

open Matching

open Rewriting


(* requires complete rethinking *)
let rec compute (t:Term.t) (trs:Rewriting.system) =
  let rec aux (ter:Term.t) = function
    | [] -> ter
    | (cond,conc)::xs -> match Matching.matching ter cond with
      | Bottom -> match ter with (*aux ter xs*)
        | Var v -> aux ter xs
        | Fun (f,args) -> List.map (fun x -> aux x trs) args
      | Solution xs -> Substitution.apply_substitutions xs ter
  in
  let x = (aux t trs) in
  if Term.same_term t x then t else compute x trs
