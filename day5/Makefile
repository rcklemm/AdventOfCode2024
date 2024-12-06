all: day5

day5: day5.ml
	ocamlfind ocamlopt -o day5.elf -linkpkg -package core str.cmxa day5.ml
	rm -f day5.cmi day5.cmx day5.o	

clean:
	rm -f day5.elf
