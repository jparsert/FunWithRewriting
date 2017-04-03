open Term
open List
open Utils


(* matching r t looks for a substitution s surch that (r * s) = t  *)
val matching : Term.t -> Term.t ->(Term.variable * Term.t) list option

(* transoforms the rsult of a matching to a string  *)
val string_of_matching : (Term.variable * Term.t) list option -> string
