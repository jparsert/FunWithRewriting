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
