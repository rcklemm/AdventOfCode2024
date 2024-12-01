all: day1

day1: day1.ml
	ocamlfind ocamlopt -o day1.elf -linkpkg -package core str.cmxa day1.ml
	rm -f day1.cmi day1.cmx day1.o	

clean:
	rm -f day1.elf
