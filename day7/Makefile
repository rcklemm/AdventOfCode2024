all: day7

day7: day7.ml
	ocamlfind ocamlopt -o day7.elf -linkpkg -package core day7.ml
	rm -f day7.cmi day7.cmx day7.o	

clean:
	rm -f day7.elf
