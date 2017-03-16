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
  | (Var v1, Var v2) -> Solution [(v1, Var v2)]
  | (Var v, Fun f) -> Solution [(v, Fun f)]
  | (Fun f, Var v) -> Solution [(v,Fun f)]
  | (Fun (f, _ ) , Fun (g,_)) when f<>g -> Bottom
  | (Fun (f, args1) , Fun(g, args2)) -> sol_union []  @@ List.map2 (matching) args1 args2

let rec string_of_matching = function
  | Bottom -> Utils.bottom_symbol
  | Solution x -> Substitution.string_of_substitution_list x
