
(* This ADT expresses a Term*)
type t = Var of string | Fun of (string * t list)

(* To_string function of term*)
val string_of_term : t -> string

(* list of variables in term t*)
val var : t -> string list

(* list of functions in term t*)
val functions : t -> string list

(* returns the root symbol of term t*)
val root : t -> string

(* returns the size of a term*)
val size : t -> int

(* returns the norm of a term *)
val norm : t -> int
