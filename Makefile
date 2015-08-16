compile=ocamlbuild -use-ocamlfind -I src -I tests -lflags -cclib,-laspell

all:
	$(compile) suggestion.native
