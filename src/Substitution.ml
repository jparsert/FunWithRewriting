open Term

type t = (Term.t * Term.t)

let string_of_substitution ((t1,t2):t) = (Term.string_of_term t1) ^ "|=>" ^(Term.string_of_term t2)
