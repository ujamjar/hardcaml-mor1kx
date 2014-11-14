.PHONY: all ext lib tests clean

all: lib tests

ext:
	ocamlbuild -use-ocamlfind pa_vprec.cmo

lib: ext
	ocamlbuild -use-ocamlfind openrisc.cma openrisc.cmxa

install_ext:
	ocamlfind install hardcaml-vprec ext/META _build/ext/pa_vprec.cmo

uninstall_ext:
	ocamlfind remove hardcaml-vprec

tests:
	ocamlbuild -use-ocamlfind \
		top.native test.native \
		top.byte test.byte statemachine.byte

clean:
	ocamlbuild -clean
	-find . -name "*~" | xargs rm -f
	-rm -f *.byte
	-rm -f *.native
