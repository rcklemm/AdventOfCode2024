all: day8

day8: day8.ml
	ocamlfind ocamlopt -o day8.elf -linkpkg -package core day8.ml
	rm -f day8.cmi day8.cmx day8.o	

clean:
	rm -f day8.elf
