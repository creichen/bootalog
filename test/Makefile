OCAMLFIND=ocamlfind
OCAMLFLAGS=-w-E
OCAMLC=${OCAMLFIND} ocamlc ${OCAMLFLAGS}
OCAMLOPT=${OCAMLFIND} ocamlopt ${OCAMLFLAGS}
OCAMLDEP=ocamldep
INCLUDES=-I ../src
PACKAGES=-package str -package oUnit -package num
OCAMLFLAGS=-g -w A-E $(INCLUDES) ${PACKAGES}
OCAMLOPTFLAGS=$(INCLUDES) ${PACKAGES}
CMX_DEPS=../src/strlib.cmx ../src/getopt.cmx ../src/compare.cmx ../src/option.cmx \
	../src/dynamic_programming.cmx \
	../src/variable.cmx \
	../src/var_set.cmx \
	../src/atom.cmx \
	../src/tuple.cmx \
	../src/predicate.cmx \
	../src/predicate_set.cmx \
	../src/fact.cmx \
	../src/base_literal.cmx \
	../src/base_rule.cmx \
	../src/errors.cmx \
	../src/stratum.cmx \
	../src/stratified_ruleset.cmx \
	../src/rule_set.cmx \
	../src/base.cmx env.cmx \
	../src/primop_interface.cmx primops.cmx \
	../src/literal.cmx \
	../src/access_path.cmx \
	../src/rule.cmx \
	../src/stratification.cmx \
	../src/simple_table.cmx ../src/combined_table.cmx ../src/database.cmx \
	../src/program.cmx ../src/lexer.cmx ../src/frontend.cmx ../src/parser.cmx \
	../src/eval.cmx
CMX_HELPERS=stratum_test_helper.cmx error_test_helper.cmx
TESTS=test_getopt.test test_table.test test_eval.test test_stratification.test \
	test_strlib.test test_frontend.test test_dynamic_programming.test \
	test_access_modes.test test_primops.test

.PHONY: all clean depend test prepare-test

all: test

test: prepare-test ${TESTS}

prepare-test: ${CMX_HELPERS}
	rm -f *.test

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .test

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.test:
	$(OCAMLOPT) -o $@ $(OCAMLFLAGS) $(CMX_DEPS) $(CMX_HELPERS) $< -linkpkg
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
