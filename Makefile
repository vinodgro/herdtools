##Top level makefile for local installaton of source tree
SRC=herd gen litmus tools
PREFIX=$$HOME

luc all install clean:
	for d in $(SRC) ; \
	do $(MAKE) $(MFLAGS) PREFIX=$(PREFIX) -C $$d $@ ; done