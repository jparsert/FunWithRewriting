
module M : Map.S with type key = string

val get: string -> int M.t -> int
