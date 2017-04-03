
(* Variable type of term *)
type variable = string

(* This ADT expresses a Term*)
type t = Var of variable | Fun of (string * t list)

type position = int list

val string_of_variable : variable -> string

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

(* checks for structural equivalence *)
val same_term : t -> t -> bool

val positions : t -> int list list

val get_term_at : t-> position -> t

val string_of_position : position -> string

val string_of_positions : position list -> string

val equals : t -> t -> bool
