(*module StringMap = struct

  type t = string

  let compare = String.compare

end*)

module M = Map.Make (String)

include M

let get key m =
  try (M.find key m) with
  | Not_found -> 0
