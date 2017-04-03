open String
open List
open Utils
open Str

type variable = string
type position = int list

type t =  Var of variable | Fun of (string * t list)

let rec string_of_arglist str lst=
  fold_left (fun a b -> if a="" then b else a ^ "," ^ b) "" @@ map str lst

let  string_of_variable x = x

let rec string_of_term = function
  | Var v -> v
  | Fun (f, []) -> f
  | Fun (f, x::y::[]) when Str.string_match (Utils.infix_symbols_regexp) f 0 -> "("^ string_of_term x ^ f ^ string_of_term y ^ ")"
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

let rec same_term t1 t2 = match (t1, t2) with
  | (Var v1, Var v2) when v1=v2 -> true
  | (Fun (f1,args1) , Fun (f2, args2)) when f1=f2 -> (List.fold_left (&&) true @@ List.map2 same_term  args1 args2)
  | _ -> false

let rec positions term : int list list=
  let rec aux acc = function
    | 0 -> rev acc
    | n -> aux (acc@[n]) (n-1)
  in
  match term with
  | Var v -> [[]]
  | Fun (f, args) -> concat ([[[]]] @ (map2 (fun t n -> map (fun x -> n::x)  t) (map positions args) (aux [] (length args))))

let rec get_term_at (term:t) (pos:position)=
  match pos with
  | [] -> term
  | n::xs when n > 0 -> (match term with
    | Var v -> failwith "Wrong argument"
    | Fun (_, args) -> get_term_at (nth args (n-1)) xs)
  | _ -> failwith "Wrong argument"

let rec string_of_position (pos:position) =
  match pos with
  | [] -> Utils.empty_set
  | x :: xs -> string_of_int x ^ ":" ^(string_of_position xs)

let rec string_of_positions (pos: position list) = fold_left (fun a b -> a ^ " " ^ b) "" (map string_of_position pos)

let rec equals t1 t2 =
  match (t1, t2)with
  | (Var x , Var y) when x=y -> true
  | (Fun (f1, args1) , Fun(f2, args2)) when f2=f1 -> fold_left (&&) true (map2 equals args1 args2)
  | _ -> false
