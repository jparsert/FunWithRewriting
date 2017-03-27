open Batteries
open Map

module type Comparable =
sig
  type t

  val compare : t -> t -> int

end


(*
module type MS =
sig
  type t

  val empty : 'a Map.t

  val get : t -> (t , int) Map.t -> int

  val remove : t -> (t , int) Map.t ->  (t , 'b) Map.t

  val insert : t -> (t , 'b) Map.t ->  (t , 'b) Map.t

  val fold : t -> (t , 'b) Map.t ->  (t , 'b) Map.t

  val is_empty : (t , 'b) Map.t ->  bool

  val to_list :  (t , 'b) Map.t -> t list

  val sum : (t , 'b) Map.t ->  (t , 'b) Map.t ->  (t , 'b) Map.t

  val difference : (t , 'b) Map.t ->  (t , 'b) Map.t ->  (t , 'b) Map.t

  val from_list : t list ->  (t , 'b) Map.t

  val insert_list : t list ->  (t , 'b) Map.t ->  (t , 'b) Map.t
end
*)


module Multiset(T:Comparable)= struct

  module M = Map.Make(T)

  include M

  let empty = M.empty

  let get key m =
    try (M.find key m) with
    | Not_found -> 0

  let remove key m = M.remove key m

  let insert e set = M.add e ((get e set)+1) set

  let fold f set start = M.fold f set start

  let is_empty set = M.is_empty set

  let to_list set = M.fold (fun a b c-> (List.make b a) @c) set []

  let sum set1 set2 =
    let aux key x1 x2 = match x1 ,x2 with
      | Some x , Some y -> Some (x + y)
      | Some x , None -> Some x
      | None , Some x -> Some x
      | _ -> None
    in
    M.merge (aux) set1 set2

  let difference set1 set2 =
    let aux key x1 x2 = match x1 , x2 with
      | Some x , Some y -> Some  (max (x - y) 0)
      | Some x , None -> Some x
      | None , Some x -> None
      | _ -> None
    in
    M.merge (aux) set1 set2

  let from_list lst =
    let rec aux accum = function
      | [] -> accum
      | x :: xs -> aux (insert x accum) xs
    in
    aux empty lst

  let insert_list lst set = sum (from_list lst) set

end
