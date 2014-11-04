all: lib tests

lib:
	ocamlbuild -use-ocamlfind openrisc.cma openrisc.cmxa

tests:
	ocamlbuild -use-ocamlfind test.byte

clean:
	ocamlbuild -clean
	-find . -name "*~" | xargs rm -f
	-rm -f *.byte
	-rm -f *.native
