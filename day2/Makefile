all: day2

day2: day2.ml
	ocamlfind ocamlopt -o day2.elf -linkpkg -package core str.cmxa day2.ml
	rm -f day2.cmi day2.cmx day2.o	

clean:
	rm -f day2.elf
