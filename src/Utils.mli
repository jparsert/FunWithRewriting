open Str


(* Symbol for bottom ⊥ meant for nice output *)
val bottom_symbol : string

(* List of symbols with same meaning for input, meaning bottom*)
val bottom_symbols : string list

(* Symbol for substitution  ⟼ meant for nice output *)
val r_arrow_b_smb : string

(* List of symbols with same meaning for input, meaning substitution*)
val r_arrow_b_symbs : string list

val empty_set : string

val empty_set_symbs : string list

val infix_symbols_regexp : Str.regexp

(*Get the maximum element of integer list*)
val max_of_int_list : int list -> int

val print_int_list : int list -> unit
