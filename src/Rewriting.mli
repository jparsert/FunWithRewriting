open Term

type rule = (Term.t * Term.t)

type system = rule list

val root_rewrite : Term.t -> rule -> Term.t
