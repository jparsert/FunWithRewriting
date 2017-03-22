open Term
open Substitution

let _ = print_string @@ (Term.string_of_term @@ Fun ("f" , [Var "x" ; Var "y"]))^"\n"
let _ = print_string @@ (Matching.string_of_matching @@ Matching.matching (Var "A") (Var "B"))^"\n"
let _ = print_string @@ (Matching.string_of_matching @@ Matching.matching (Var "A") (Var "B")) ^"\n"
let _ = print_string @@ (Matching.string_of_matching @@
                        Matching.matching (Fun ("f", [Var "X" ; Var "Y"])) (Fun ("f", [Fun ("a",[]);Fun ("b",[])])))^"\n"
let _ = print_string @@ (Matching.string_of_matching @@ Matching.matching (Fun ("f", [Var "A"])) (Fun ("g", [Var "C"])))^"\n"

let _ =
  let t1 = Fun ("+" , [Var "x" ; Fun ("s" , [Fun ("+",[Var "y" ; Var "z"])])]) in
  let t2 = Fun ("+" ,[Fun ("s", [Var "y"]) ; Fun ("s", [Fun ("+" , [Fun ("+", [Var "x";Fun ("s", [Fun ("0",[])])]) ; Var "z"])])]) in
  print_string @@ (Matching.string_of_matching @@  Matching.matching t1 t2)^"\n"

let _ =
  let t1 = Fun ("*" , [Fun ("-", [Var "x"]) ; Fun ("*" , [Var "x" ; Var "y"])]) in
  let t2 = Fun ("*" , [Fun ("-", [Fun ("e",[]);Var "x"]) ; Fun ("*", [Fun ("*" , [Fun ("e", []) ; Fun ("e" , [])]) ; Var "x"])]) in
  print_string @@ (Matching.string_of_matching @@  Matching.matching t1 t2)^"\n"

let _ =
  let s = [("x", Var "a");("y", Fun ("f",[Var "x"]))] in
  let t = Fun ("g" ,[ Var "a" ; Var "x" ; Var "y"]) in
  let subst = apply_substitutions s t in
  print_string @@ (string_of_substitution_list s)^"\n";
  print_string @@ (string_of_term t)^"\n";
  print_string @@ (Term.string_of_term subst)^"\n"

let mp = Multiset.empty
let mp = Multiset.insert "a" mp
let _ = print_int @@ Multiset.get "a" mp
let mp = Multiset.insert "a" mp
let _ = print_int @@ Multiset.get "a" mp
let _ = print_string @@ List.fold_left (^) "" (Multiset.to_list mp)

let _ = print_string "\n"

let mq = Multiset.empty
let mq = Multiset.insert "a" mq
let mq = Multiset.insert "z" mq


let _ = print_string @@ List.fold_left (^) "" (Multiset.to_list (Multiset.sum mp mq))
let _ = print_int @@ Multiset.get "z" mq

let _ = print_string "\n"

let tst1 = Multiset.from_list ["a";"a";"a";"b";"d";"d";"d"]
let tst2 = Multiset.from_list ["a";"c";"c"]

let _ = print_string @@ List.fold_left (^) "" (Multiset.to_list tst1)
let _ = print_string "\n"

let _ = print_string @@ List.fold_left (^) "" (Multiset.to_list tst2)
let _ = print_string "\n"

let _ = print_string @@ List.fold_left (^) "" (Multiset.to_list (Multiset.difference tst1 tst2))
let _ = print_string "\n"
let _ = print_string @@ List.fold_left (^) "" (Multiset.to_list (Multiset.sum tst1 tst2))

let _ = print_string "\n"
