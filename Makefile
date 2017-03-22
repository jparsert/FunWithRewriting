all:
	ocamlbuild -use-ocamlfind -package batteries -package str  Main.native
