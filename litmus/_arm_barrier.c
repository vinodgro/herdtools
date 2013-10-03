/**********************/
/* User level barrier */
/**********************/


typedef struct {
  volatile int c,sense;
} sense_t;

static void barrier_init (sense_t *p) {
  p->c = N;
  p->sense = 0;
}

static void barrier_wait(sense_t *p, int *mySense) {
  int r1,r2,r3;
asm __volatile__ (  
"dmb\n\t"
"ldr %[r2],[%[ms]]\n\t"
"dmb\n\t"
"0:\n\t"
"ldrex %[r1],[%[c]]\n\t"
"sub %[r1],%[r1],#1\n\t"
"strex %[r3],%[r1],[%[c]]\n\t"
"cmp %[r3],#0\n\t"
"bne 0b\n\t"
"cmp %[r1],#0\n\t"
"bne 1f\n\t"
"dmb\n\t"
"mov %[r1],%[n]\n\t"
"str %[r1],[%[c]]\n\t"
"dmb\n\t"
"mvns %[r1],%[r2]\n\t"
"str %[r1],[%[s]]\n\t"
"b 2f\n\t"
"1:\n\t"
"dmb\n\t"
"ldr %[r1],[%[s]]\n\t"
"cmp %[r1],%[r2]\n\t"
"beq 1b\n\t"
"2:\n\t"
"dmb\n\t"
"mvns %[r1],%[r2]\n\t"
"str %[r1],[%[ms]]\n\t"
"dsb\n\t"
: [r1] "=&r" (r1), [r2] "=&r" (r2), [r3] "=&r" (r3)
: [c] "r" (&p->c), [s] "r" (&p->sense), [ms] "r" (mySense), [n] "n" (N)
: "r0","memory") ;
}
