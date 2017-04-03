open Term

type rule = (Term.t * Term.t)

type position = int list

type system = rule list

type reduct = position * Substitution.t * rule * Term.t



let root_rewrite (term:Term.t) ((rm,tl):rule) =
  match Matching.matching rm term with
  | None -> term
  | Some s -> Substitution.apply_substitutions s tl
open Str


(*
let rec rewrite_ (term:Term.t) (sys:system) =
  match sys with
  | [] -> (match term with
      | Fun (_, args) -> List.map (|> rewrite_ sys) args
      | _ -> failwith "Error ocurred, or not, I dont know")
  | x :: xs -> match Matching.matching x term with
    | None -> rewrite_ term sys
*)
