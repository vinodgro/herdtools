PREFIX=$$HOME
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib/herd
J=-j 2
OCB=ocamlbuild -cflags -w,A-4-9-29-45 -classic-display $(J)
EXTRAPROGS=
PROGS=herd.byte
MDL=x86tso.cat herd.cat herdcat.cat ppc.cat sc.cat LL.cat arm.cat power.cat \
 atom.cat pretty.cat prettycat.cat fulleieio.cat armllh.cat uniproc.cat uniproccat.cat \
 minimal.cat minimalcat.cat qualcomm.cat \
 sccat.cat

CFG=apoil.cfg enslip.cfg web.cfg doc.cfg
DOT=test7.dot doc.dot

all: native

install:
	mkdir -p $(LIBDIR)
	cp $(MDL) $(DOT) $(CFG) $(LIBDIR)
#backward compatibility
	@for m in $(MDL:.cat=);\
	do ln -sf $(LIBDIR)/$$m.cat $(LIBDIR)/$$m.mdl; done
	@mkdir -p $(BINDIR)
	@for prog in $(PROGS:.byte=) ; \
	do cp $$prog $(BINDIR); done

luc: native install


byte native d.byte: version
	$(OCB) $(PROGS:.byte=.$@)
	@for prog in $(PROGS:.byte=.$@) ; \
	do ln -sf $$prog `basename $$prog .$@` ; done

clean::
	-$(OCB) -clean

clean::
	/bin/rm -f *~
	@for prog in $(PROGS:.byte=) ; \
	do /bin/rm -f $$prog $$prog.byte $$prog.native ; done

SVNVERSION=sh -c "LC_ALL=C git rev-parse HEAD" 2>/dev/null || echo exported

version:
	@echo "(* GENERATED, DO NOT EDIT *)" > version.ml
	@echo  'let version = "'`head -1 lib/VERSION.txt`'"' >> version.ml
	@export LC_ALL=C && echo  'let rev = "'`$(SVNVERSION)`'"' >> version.ml
	@@echo "let libdir = \"$(LIBDIR)\"" >> version.ml

clean::
	/bin/rm -f version.ml

