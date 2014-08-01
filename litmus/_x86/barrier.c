/**********************/
/* User level barrier */
/**********************/

typedef struct {
  volatile int c,sense ;
  int n ;
} sense_t ;


static void barrier_init(sense_t *p, int n) {  
  p->n = p->c = n ;
  p->sense = 0 ;
}

static void barrier_wait(sense_t *p) {
  int sense = p->sense ;
  int r1,r2 ;
  asm __volatile__ (
    "movl $-1,%[eax]\n\t"
    "lock xaddl %[eax],%[c]\n\t"
    "subl $1,%[eax]\n\t"
    "je 0f\n"
    "1:\n\t"
    "cmpl %[ms],%[s]\n\t"
    "je 1b\n\t"
    "jmp 2f\n"
    "0:\n\t"
    "movl %[np],%[r2]\n\t"
    "movl %[r2],%[c]\n\t"
    "xorl %[eax],%[eax]\n\t"
    "testl %[ms],%[ms]\n\t"
    "sete %%al\n\t"
    "movl %[eax],%[s]\n\t"
    "mfence\n"
    "2:\n\t"
    : [eax] "=&a" (r1), [r2] "=&r" (r2)
    : [ms] "r" (sense), [s] "m" (p->sense), [c] "m" (p->c), [np] "m" (p->n)
    : "memory") ;
}

