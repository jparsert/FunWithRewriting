open Term
open List
open Rewriting
open Utils
open Substitution

type result = Bottom | Solution of Substitution.t list


val matching : Term.t -> Term.t -> result

val string_of_matching : result -> string
