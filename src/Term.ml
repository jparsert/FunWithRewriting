open String
open List
open Utils

type t =  Var of string | Fun of (string * t list)

let rec string_of_arglist str = function
  | [] -> ""
  | x::[] -> (str x)
  | x::y::[] -> (str x) ^","^ (str y)
  | x::y::xs -> (str x) ^","^ (str y) ^ ","^(string_of_arglist str xs)

let rec string_of_term = function
  | Var v -> v
  | Fun (f, []) -> f
  | Fun (f,args) -> f^ "(" ^ (string_of_arglist (string_of_term) args) ^ ")"

let rec var = function
  | Var v -> [v]
  | Fun (_, args) -> fold_left (@) [] (map var args)

let rec functions = function
  | Fun (f , args) -> fold_left (@) [f] (map functions args)
  | _ -> []

let rec root = function
  | Var v -> v
  | Fun (f,_) -> f

let rec size t =
  let rec aux acc = function
    | Var _ -> acc+1
    | Fun (f, args) -> 1 + (fold_left (+) 0 (map (aux 0) args))
  in
  aux 0 t

let rec norm t =
  let rec aux acc = function
    | Var _ -> acc
    | Fun (f, args) -> 1 + (fold_left (+) 0 (map (aux 0) args))
  in
  aux 0 t

let rec height = function
  | Var v -> 1
  | Fun (f, args) -> 1 + (max_of_int_list @@ map height args)
