
type t = (Term.variable * Term.t)

val string_of_substitution : t -> string

val string_of_substitution_list : t list -> string

val apply_substitutions : t list -> Term.t -> Term.t
