
module M : Map.S with type key = string

include module type of M
  
val get: string -> int t -> int
