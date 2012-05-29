OCAMLFIND=ocamlfind
OCAMLC=${OCAMLFIND} ocamlc
OCAMLOPT=${OCAMLFIND} ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I ../src
PACKAGES=-package str -package oUnit
OCAMLFLAGS=-g -w A $(INCLUDES) ${PACKAGES}
OCAMLOPTFLAGS=$(INCLUDES) ${PACKAGES}
CMX_DEPS=../src/getopt.cmx ../src/datalog.cmx
TESTS=test_getopt.test test_datalog.test

.PHONY: all clean depend test prepare-test

all: test

test: prepare-test ${TESTS}

prepare-test:
	rm -f *.test

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .test

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.test:
	$(OCAMLOPT) -o $@ $(OCAMLFLAGS) $(CMX_DEPS) $< -linkpkg
	./$@

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f *.cm[iox]
	rm -f *.o
	rm -f *.test

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
