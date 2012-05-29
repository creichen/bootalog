OCAMLFIND=ocamlfind
OCAMLC=${OCAMLFIND} ocamlc
OCAMLOPT=${OCAMLFIND} ocamlopt
OCAMLDEP=ocamldep
INCLUDES=                # all relevant -I options here
PACKAGES=-package str
OCAMLFLAGS=-g -w A $(INCLUDES) ${PACKAGES}
OCAMLOPTFLAGS=$(INCLUDES) ${PACKAGES}

# prog1 should be compiled to bytecode, and is composed of three
# units: mod1, mod2 and mod3.

# The list of object files for prog1
#PROG1_OBJS=mod1.cmo mod2.cmo mod3.cmo

#prog1: $(PROG1_OBJS)
#	$(OCAMLC) -o prog1 $(OCAMLFLAGS) $(PROG1_OBJS)

# sass should be compiled to native-code, and is composed of two
# units: mod4 and mod5.

# The list of object files for prog2
DATALOG_OBJS=strlib.cmx error.cmx getopt.cmx compare.cmx \
	variable.cmx base.cmx env.cmx \
	stratification.cmx \
	simple_table.cmx combined_table.cmx database.cmx \
	eval.cmx \
	repl.cmx

datalog: $(DATALOG_OBJS)
	$(OCAMLOPT) -o  $(OCAMLFLAGS) $(DATALOG_OBJS) -linkpkg

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f datalog
	rm -f *.cm[iox]
	rm -f *.o

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
