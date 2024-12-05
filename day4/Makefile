all: day4

day4: day4.ml
	ocamlfind ocamlopt -o day4.elf -linkpkg -package core str.cmxa day4.ml
	rm -f day4.cmi day4.cmx day4.o	

clean:
	rm -f day4.elf
