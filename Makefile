HC	= ghc
HFLAGS	= -O2

CC	= gcc
CFLAGS	= -O2 -I/usr/local/include

LDFLAGS	= -L/usr/local/lib -lgsl

COBJ	= gsl_interp.o

MODULE	= GSLInterp

.PHONY:	test
test:	test.x
	./test.x

test.x:	$(COBJ) $(MODULE).hs Test.hs 
	$(HC) $(HFLAGS) --make $(MODULE).hs Test.hs $(COBJ) $(LDFLAGS) -o $@

.PHONY: doc
doc:	$(MODULE).hs
	mkdir -p doc
	haddock -h -o doc $(MODULE).hs

.PHONY:	clean
clean:
	rm -f $(COBJ) Test.o Test.hi $(MODULE).o $(MODULE).hi
