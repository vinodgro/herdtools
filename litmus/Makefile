PREFIX=$$HOME
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib/litmus
CFG=conti.cfg saumur.cfg saumur.close.cfg doko.cfg squale.cfg hpcx.cfg \
 vargas.cfg vargas.cache.cfg vargas.close.cfg vargas16.cfg \
 abducens.cfg  abducens.big.cfg abducens.cache.cfg abducens.close.cfg \
 chianti.cfg portable.cfg beaune.cfg \
 power7.cfg power7.32.cfg power.cfg g5.cfg\
 ARM.cfg ARM_ADB.cfg cortex9.cfg cortex9-O0.cfg\
 dragon.cfg dragon-arm.cfg dragon-thumb.cfg\
 trimslice.cfg\
 tst.cfg auto.cfg power7.smt.cfg power6.smt.cfg \
 mp.cfg ipad.cfg asus.cfg asus-debian.cfg asus-debian-thumb.cfg cortexA15.cfg \
 mike.cfg youpla.cfg hopla.cfg corei7.cfg x86.cfg nexus10.cfg nexus7.cfg \
 odroid-xu.cfg odroid-xu-ubuntu.cfg

OCBOPT=-no-ocamlfind
OCB=ocamlbuild $(OCBOPT) -cflags -w,A-4-9-27-29-45 -classic-display
all: native

install:
	mkdir -p $(BINDIR)
	cp litmus $(BINDIR)
	mkdir -p $(LIBDIR)
	cp _utils.c _utils.h $(LIBDIR)
	cp _outs.c _outs.h $(LIBDIR)
	cp _aix_affinity.c _linux_affinity.c _affinity.h $(LIBDIR)
	cp _ppc_barrier.c  _x86_barrier.c  _arm_barrier.c $(LIBDIR)
	cp _ppc_barrier_lab.c $(LIBDIR)
	cp _show.awk _showC.awk _toh.sh $(LIBDIR)
	cp $(CFG) $(LIBDIR)

luc: all install

#To build several targets with ocamlbuild
PROGS=litmus.byte

byte native d.byte: config ./pp2ml.byte
	$(OCB) $(PROGS:.byte=.$@)
	@for prog in $(PROGS:.byte=.$@) ; \
	do ln -sf $$prog `basename $$prog .$@` ; done

clean::
	/bin/rm -f *~ litmus mcycles *.native *.byte *.d.byte

clean::
	$(OCB) -clean

SVNVERSION=sh -c "git rev-parse HEAD" 2>/dev/null || echo exported

.PHONY: config

config:
	@echo "(* GENERATED, do not edit *)" > config.ml
	@echo 'let version = "'`head -1 lib/VERSION.txt`'"' >> config.ml
	@echo "let libdir = \"$(LIBDIR)\"" >> config.ml
	@export LC_ALL=C && echo "let rev = \"`$(SVNVERSION)`\"" >> config.ml

./pp2ml.byte: ./pp2ml.mll
	$(OCB) $@

clean::
	/bin/rm -f ./pp2ml.byte

clean::
	/bin/rm -f config.ml

