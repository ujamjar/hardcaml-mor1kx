all: lib tests

lib:
	ocamlbuild -use-ocamlfind openrisc.cma openrisc.cmxa

tests:
	ocamlbuild -use-ocamlfind \
		top.native test.native \
		top.byte test.byte statemachine.byte

clean:
	ocamlbuild -clean
	-find . -name "*~" | xargs rm -f
	-rm -f *.byte
	-rm -f *.native
