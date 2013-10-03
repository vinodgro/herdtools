PREFIX=/usr/local
DIRS=litmus gen

default: all

all install clean:
	for d in $(DIRS) ; \
	do ( cd $$d && $(MAKE) $(MFLAGS) PREFIX=$(PREFIX) $@ ) ; done

