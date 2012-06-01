OCAMLFIND=ocamlfind
OCAMLC=${OCAMLFIND} ocamlc
OCAMLOPT=${OCAMLFIND} ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I ../src
PACKAGES=-package str -package oUnit
OCAMLFLAGS=-g -w A $(INCLUDES) ${PACKAGES}
OCAMLOPTFLAGS=$(INCLUDES) ${PACKAGES}
CMX_DEPS=../src/strlib.cmx ../src/error.cmx ../src/getopt.cmx ../src/compare.cmx \
	../src/variable.cmx ../src/base.cmx ../src/env.cmx \
	../src/stratification.cmx \
	../src/simple_table.cmx ../src/combined_table.cmx ../src/database.cmx \
	../src/eval.cmx
TESTS=test_getopt.test test_table.test test_eval.test test_stratification.test

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
