open Term
open Rewriting
open Utils
open Substitution
open Matching

let _ = print_string @@ (Term.string_of_term @@ Fun ("f" , [Var "x" ; Var "y"]))^"\n"
let _ = print_string @@ (Matching.string_of_matching @@ Matching.matching (Var "A") (Var "B"))^"\n"
let _ = print_string @@ (Matching.string_of_matching @@ Matching.matching (Var "A") (Var "B")) ^"\n"
let _ = print_string @@ (Matching.string_of_matching @@
                        Matching.matching (Fun ("f", [Var "X" ; Var "Y"])) (Fun ("f", [Fun ("a",[]);Fun ("b",[])])))^"\n"
let _ = print_string @@ (Matching.string_of_matching @@ Matching.matching (Fun ("f", [Var "A"])) (Fun ("g", [Var "C"])))^"\n"

let _ = let s = [("x", Var "a");("y", Fun ("f",[Var "x"]))] in
  let t = Fun ("g" ,[ Var "a" ; Var "x" ; Var "y"]) in
  let subst = apply_substitutions s t in
  print_string @@ (string_of_substitution_list s)^"\n";
  print_string @@ (string_of_term t)^"\n";
  print_string @@ (Term.string_of_term subst)^"\n"
(*
let _ =
  let ter = Fun ("f" , [Var "x"]) in
  let trs = [(Var "x" , Fun ("g" , []))] in
  print_string @@ (Term.string_of_term @@ Computation.compute ter trs) ^"\n"
*)
