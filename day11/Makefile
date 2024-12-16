all: day11

day11: day11.ml
	ocamlfind ocamlopt -o day11.elf -linkpkg -package core day11.ml
	rm -f day11.cmi day11.cmx day11.o	

clean:
	rm -f day11.elf
