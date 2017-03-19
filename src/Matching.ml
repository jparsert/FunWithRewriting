open Term
open List
open Rewriting
open Utils
open Substitution

type result = Bottom | Solution of Substitution.t list

let rec matching s t =
  let rec sol_union acc = function
    | [] -> Solution acc
    | (Solution x)::xs -> sol_union(x@acc) xs
    | Bottom::xs -> Bottom
  in
  let rec unpack_option acc = function
    | None -> Bottom
    | Some x -> sol_union [] x
  in
  match (s, t) with
  | (Var v1, Var v2) -> Solution [(v1, Var v2)]
  | (Var v, Fun f) -> Solution [(v, Fun f)]
  | (Fun f, Var v) -> Bottom
  | (Fun (f, _) , Fun (g,_)) when f<>g -> Bottom
  | (Fun (f, args1) , Fun(g, args2)) -> unpack_option []
                                          (try Some (List.map2 (matching) args1 args2) with
                                          _ -> None)

let rec string_of_matching = function
  | Bottom -> Utils.bottom_symbol
  | Solution x -> Substitution.string_of_substitution_list x
