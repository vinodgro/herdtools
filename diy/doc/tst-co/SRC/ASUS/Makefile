GCC=arm-linux-gnueabi-gcc
GCCOPTS=-D_GNU_SOURCE -Wall -std=gnu99 -mcpu=cortex-a9 -O2 -pthread
LINKOPTS=-static
SRC=\
 R+dmbs.c\
 MP+dmbs.c\
 R+dmb+pos.c\
 MP+dmb+pos.c\
 WW+dmb.c\
 2+2W+dmbs.c\
 S+dmbs.c\
 2+2W+dmb+pos.c\
 S+dmb+pos.c\
 WR+dmb.c\
 SB+dmbs.c\
 SB+dmb+pos.c\
 R+pos+dmb.c\
 RW+dmb.c\
 LB+dmbs.c\
 S+pos+dmb.c\
 LB+dmb+pos.c\
 W+RW+dmb.c\
 MP+pos+dmb.c\
 W+RR+dmb.c\
 R+poss.c\
 MP+poss.c\
 WW.c\
 2+2W+poss.c\
 S+poss.c\
 WR.c\
 SB+poss.c\
 RW.c\
 LB+poss.c\
 W+RW.c\
 W+RR.c\

T=$(SRC:.c=.t)
H=$(SRC:.c=.h)
OBJ=$(SRC:.c=.o)
EXE=run.exe

all: $(EXE)

clean:
	/bin/rm -f *.o *.s *.t *.exe *~ $(H)

cleansource:
	/bin/rm -f *.o *.c *.h *.s *~

affinity.o: affinity.c
	$(GCC) $(GCCOPTS) -O2 -c affinity.c

outs.o: outs.c
	$(GCC) $(GCCOPTS) -O2 -c outs.c

utils.o: utils.c
	$(GCC) $(GCCOPTS) -O2 -c utils.c

UTILS=affinity.o outs.o utils.o

$(EXE): $(UTILS) $(OBJ) run.o
	$(GCC)  $(GCCOPTS) $(LINKOPTS) -o $@ $(UTILS) $(OBJ) run.o

%.o:%.c
	$(GCC) $(GCCOPTS) $(LINKOPTS) -c -o $@ $<

%.s:%.c
	$(GCC) -DASS $(GCCOPTS) -S $<

%.t:%.s
	awk -f show.awk $< > $@

%.h:%.t
	sh toh.sh $< > $@

W+RR.o: W+RR.h W+RR.c
W+RW.o: W+RW.h W+RW.c
LB+poss.o: LB+poss.h LB+poss.c
RW.o: RW.h RW.c
SB+poss.o: SB+poss.h SB+poss.c
WR.o: WR.h WR.c
S+poss.o: S+poss.h S+poss.c
2+2W+poss.o: 2+2W+poss.h 2+2W+poss.c
WW.o: WW.h WW.c
MP+poss.o: MP+poss.h MP+poss.c
R+poss.o: R+poss.h R+poss.c
W+RR+dmb.o: W+RR+dmb.h W+RR+dmb.c
MP+pos+dmb.o: MP+pos+dmb.h MP+pos+dmb.c
W+RW+dmb.o: W+RW+dmb.h W+RW+dmb.c
LB+dmb+pos.o: LB+dmb+pos.h LB+dmb+pos.c
S+pos+dmb.o: S+pos+dmb.h S+pos+dmb.c
LB+dmbs.o: LB+dmbs.h LB+dmbs.c
RW+dmb.o: RW+dmb.h RW+dmb.c
R+pos+dmb.o: R+pos+dmb.h R+pos+dmb.c
SB+dmb+pos.o: SB+dmb+pos.h SB+dmb+pos.c
SB+dmbs.o: SB+dmbs.h SB+dmbs.c
WR+dmb.o: WR+dmb.h WR+dmb.c
S+dmb+pos.o: S+dmb+pos.h S+dmb+pos.c
2+2W+dmb+pos.o: 2+2W+dmb+pos.h 2+2W+dmb+pos.c
S+dmbs.o: S+dmbs.h S+dmbs.c
2+2W+dmbs.o: 2+2W+dmbs.h 2+2W+dmbs.c
WW+dmb.o: WW+dmb.h WW+dmb.c
MP+dmb+pos.o: MP+dmb+pos.h MP+dmb+pos.c
R+dmb+pos.o: R+dmb+pos.h R+dmb+pos.c
MP+dmbs.o: MP+dmbs.h MP+dmbs.c
R+dmbs.o: R+dmbs.h R+dmbs.c

