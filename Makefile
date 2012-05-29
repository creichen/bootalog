default: all

all:	depend
	(cd src; make)

test: all
	(cd test; make test)

clean:
	(cd src; make clean)
	(cd test; make clean)

depend:
	touch src/.depend
	touch test/.depend
	(cd src; make depend)
	(cd test; make depend)
