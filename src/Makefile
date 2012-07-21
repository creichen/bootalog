OCAMLFIND=ocamlfind
OCAMLFLAGS=
OCAMLC=${OCAMLFIND} ocamlc ${OCAMLFLAGS}
OCAMLOPT=${OCAMLFIND} ocamlopt ${OCAMLFLAGS}
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
INCLUDES=                # all relevant -I options here
PACKAGES=-package str -package num
OCAMLFLAGS=-g -w A-E $(INCLUDES) ${PACKAGES}
OCAMLOPTFLAGS=$(INCLUDES) ${PACKAGES}
BINARY=bootalog

# prog1 should be compiled to bytecode, and is composed of three
# units: mod1, mod2 and mod3.

# The list of object files for prog1
#PROG1_OBJS=mod1.cmo mod2.cmo mod3.cmo

#prog1: $(PROG1_OBJS)
#	$(OCAMLC) -o prog1 $(OCAMLFLAGS) $(PROG1_OBJS)

# sass should be compiled to native-code, and is composed of two
# units: mod4 and mod5.

# The list of object files for prog2
DATALOG_OBJS=strlib.cmx getopt.cmx compare.cmx option.cmx \
	dynamic_programming.cmx \
	variable.cmx \
	var_set.cmx \
	atom.cmx \
	tuple.cmx \
	predicate.cmx \
	predicate_set.cmx \
	fact.cmx \
	base_literal.cmx \
	base_rule.cmx \
	errors.cmx \
	stratum.cmx \
	stratified_ruleset.cmx \
	rule_set.cmx \
	base.cmx env.cmx \
	primop_interface.cmx primops.cmx \
	literal.cmx \
	access_path.cmx \
	rule.cmx \
	stratification.cmx \
	simple_table.cmx combined_table.cmx database.cmx \
	eval.cmx \
	program.cmx lexer.cmx parser.cmx frontend.cmx \
	repl.cmx

bootalog: $(DATALOG_OBJS)
	$(OCAMLOPT) -o ${BINARY} $(OCAMLFLAGS) $(DATALOG_OBJS) -linkpkg

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

%.ml: %.mll
	${OCAMLLEX} ${OCAMLLEXFLAGS} -o $@ $<

%.ml: %.mly
	${OCAMLYACC} ${OCAMLLEXFLAGS} $<

%.mli: %.mly
	${OCAMLYACC} ${OCAMLLEXFLAGS} $<

# Clean up
clean:
	rm -f ${BINARY}
	rm -f *.cm[iox]
	rm -f *.o

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
