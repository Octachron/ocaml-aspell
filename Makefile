compile=ocamlbuild -use-ocamlfind -I src -lflags -cclib,-laspell

all:
	$(compile) aspell.native
