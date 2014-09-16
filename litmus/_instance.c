/* Instance context */
/************/
/* Instance */
/************/

typedef struct {
  int *mem;
  log_t out;
  tb_t next_tb;
  hash_t t;
  barrier_t b;
} inst_t ;


static void init_instance (inst_t *p, int *mem) {
  p->mem = mem ;
  init_hash(&p.t) ;
  init_barrier(&b,N)
}

/* And complete context */
static int *mem[MEMSZ];

typedef struct {
  int size,ninst ;
  inst_t inst[NINST] ;
} global_t ;
