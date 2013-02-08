.PHONY: default all native test clean depend

default: all

all:	native depend
	(cd src; make)

native:
	(cd native/src; make libnsys.a)

test: all
	(cd native; make check)
	(cd test; make test)

clean:
	(cd native; make clean)
	(cd src; make clean)
	(cd test; make clean)

depend:
	touch src/.depend
	touch test/.depend
	(cd src; make depend)
	(cd test; make depend)
