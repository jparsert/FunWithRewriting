open Term
open List
open Rewriting
open Utils
open Substitution

type result = Bottom | Solution of Substitution.t list


(* matching r t looks for a substitution s surch that (r * s) = t  *)
val matching : Term.t -> Term.t -> result

(* transoforms the rsult of a matching to a string  *)
val string_of_matching : result -> string
