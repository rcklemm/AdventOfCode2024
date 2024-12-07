all: day6

day6: day6.ml
	ocamlfind ocamlopt -o day6.elf -linkpkg -package core day6.ml
	rm -f day6.cmi day6.cmx day6.o	

clean:
	rm -f day6.elf
