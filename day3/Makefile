all: day3

day3: day3.ml
	ocamlfind ocamlopt -o day3.elf -linkpkg -package core str.cmxa day3.ml
	rm -f day3.cmi day3.cmx day3.o	

clean:
	rm -f day3.elf
