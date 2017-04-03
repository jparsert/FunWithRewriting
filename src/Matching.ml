open Term
open List
open Utils

(* Esigma . s sigma = t*)
let rec matching s t =
  let rec sol_union acc = function
    | [] -> Some acc
    | (Some x)::xs -> sol_union(x@acc) xs
    | None::xs -> None
  in
  let rec unpack_option acc = function
    | None -> None
    | Some x -> sol_union [] x
  in
  match (s, t) with
  | (Var v1, Var v2) -> Some [(v1, Var v2)]
  | (Var v, Fun f) -> Some [(v, Fun f)]
  | (Fun f, Var v) -> None
  | (Fun (f, _) , Fun (g,_)) when f<>g -> None
  | (Fun (f, args1) , Fun(g, args2)) -> unpack_option []
                                          (try Some (List.map2 (matching) args1 args2) with
                                          _ -> None)

let rec string_of_matching = function
  | None -> Utils.bottom_symbol
  | Some x -> Substitution.string_of_substitution_list x
