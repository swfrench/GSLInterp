# haskell compiler and compilation flags
HC	= ghc
HFLAGS	= -O2

# C compiler and compilation flags
CC	= gcc
CFLAGS	= -O2

# If needed, path for unusually-located GSL installations
GSLPATH =

LDFLAGS	= -lgsl

COBJ	= gsl_interp.o

MODULE	= GSLInterp

ifneq ($(strip $(GSLPATH)),)
	CFLAGS += -I$(GSLPATH)/include
	LDFLAGS += -L$(GSLPATH)/lib
endif

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
