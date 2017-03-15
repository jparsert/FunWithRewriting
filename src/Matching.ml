open Term
open List
open Rewriting
open Utils
open Substitution

type result = Bottom | Solution of Substitution.t list

let rec matching t1 t2 =
  let rec sol_union acc = function
    | [] -> Solution acc
    | (Solution x)::xs -> sol_union (x@acc) xs
    | Bottom::xs -> Bottom
  in
  match (t1, t2) with
  | (Var _, Var _) as v -> Solution [v]
  | (Var v, Fun f) as s -> Solution [s]
  | (Fun f, Var v) -> Solution [(Var v,Fun f)]
  | (Fun (f, _ ) , Fun (g,_)) when f<>g -> Bottom
  | (Fun (f, args1) , Fun(g, args2)) -> sol_union []  @@ List.map2 (matching) args1 args2

let rec string_of_matching = function
  | Bottom -> Utils.bottom_symbol
  | Solution x -> Substitution.string_of_substitution_list x
