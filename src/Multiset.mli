open Batteries

module M : Map.S with type key = string

(*include module type of M*)

val empty : 'a M.t

val get: M.key -> int M.t -> int

val remove : M.key -> 'a M.t -> 'a M.t

val insert : M.key -> int M.t -> int M.t

val fold : (M.key -> 'a -> 'b -> 'b) -> 'a M.t -> 'b -> 'b

val is_empty : 'a M.t -> bool

val to_list : int M.t -> M.key list

val sum : int M.t -> int M.t -> int M.t

val difference : int M.t -> int M.t -> int M.t

val from_list : M.key list -> int M.t

val insert_list : M.key list -> int M.t -> int M.t
