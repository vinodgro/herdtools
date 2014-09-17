/************/
/* Instance */
/************/

typedef struct {
  int *mem;
  log_t out;
  tb_t next_tb;
  hash_t t;
  sense_t b;
} ctx_t ;


static void instance_init (ctx_t *p, int *mem) {
  p->mem = mem ;
  hash_init(&p->t) ;
  barrier_init(&p->b,N) ;
}

/******************/
/* Global context */
/******************/

#define LINESZ (LINE/sizeof(int))
#define MEMSZ ((NVARS*NEXE+1)*LINESZ)

static int mem[MEMSZ] ;

typedef struct {
  /* Topology */
  int *inst, *role ;
  char **group ;
  count_t *ngroups ;
  /* memory */
  int *mem ;
  /* Runtime control */
  int verbose ;
  int size,ninst ;
  /* Synchronisation for all threads */
  volatile int go ; /* First synchronisation */
  sense_t gb ;    /* All following synchronisation */
  /* Att instance contexts */
  ctx_t ctx[NEXE] ; /* All test instance contexts */
} global_t ;

static global_t global  = { inst, role, group, ngroups, mem, } ;

static void init_global(global_t *g) {
  /* Global barrier */
  barrier_init(&g->gb,AVAIL) ;
  /* Align  to cache line */
  uintptr_t x = (uintptr_t)(g->mem) ;
  x += LINE-1 ; x /=  LINE ; x *= LINE ;
  int *m = (int *)x ;

  /* Instance contexts */
  for (int k = 0 ; k < NEXE ; k++) {
    instance_init(&g->ctx[k],m) ;
    m += NVARS*LINESZ ;
  }

  /* Topology */
  g->inst = inst ;
  g->role = role ;
}

/******************/
/* Thread context */
/******************/

typedef struct {
  int id ;
  int role ;
  ctx_t *ctx ;
} thread_ctx_t ;

