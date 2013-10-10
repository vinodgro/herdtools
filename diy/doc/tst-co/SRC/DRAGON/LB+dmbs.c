/* Parameters */
#define SIZE_OF_TEST 10000
#define NUMBER_OF_RUN 100
#define AVAIL 2
#define STRIDE 1
#define MAX_LOOP 0
#define N 2
#define AFF_INCR (0)
/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>
#include <time.h>
#include <limits.h>
#include "utils.h"
#include "outs.h"
#include "affinity.h"

/* params */
typedef struct {
  int verbose;
  int size_of_test,max_run;
  int stride;
  int aff_rand, ncpus, ncpus_used;
  int do_change;
} param_t;


/* Full memory barrier */
inline static void mbar(void) {
  asm __volatile__ ("dsb" ::: "memory");
}

/* Barriers macros */
inline static void barrier_wait(unsigned int id, unsigned int k, int volatile *b) {
  if ((k % N) == id) {
    *b = 1 ;
  } else {
    while (*b == 0) ;
  }
}
/**********************/
/* Context definition */
/**********************/

typedef struct {
/* Shared variables */
  int *x;
/* Final content of observed  registers */
  int *out_0_r0;
  int *out_1_r0;
/* Check data */
  pb_t *fst_barrier;
  po_t *s_or;
  int* cpy_x[N] ;
/* Barrier for litmus loop */
  int volatile *barrier;
  st_t seed;
/* Parameters */
  param_t *_p;
} ctx_t;

inline static int final_cond() {
  int cond;
  cond = 1;
  return cond;
}

/**********************/
/* Outcome collection */
/**********************/
#define NOUTS 3
typedef int outcome_t[NOUTS];

static const int out_0_r0_f = 0 ;
static const int out_1_r0_f = 1 ;
static const int x_f = 2 ;


typedef struct hist_t {
  outs_t *outcomes ;
  count_t n_pos,n_neg ;
} hist_t ;

static hist_t *alloc_hist(void) {
  hist_t *p = malloc_check(sizeof(*p)) ;
  p->outcomes = NULL ;
  p->n_pos = p->n_neg = 0 ;
  return p ;
}

static void free_hist(hist_t *h) {
  free_outs(h->outcomes) ;
  free(h) ;
}

static void add_outcome(hist_t *h, count_t v, outcome_t o, int show) {
  h->outcomes = add_outcome_outs(h->outcomes,o,NOUTS,v,show) ;
}

static void merge_hists(hist_t *h0, hist_t *h1) {
  h0->n_pos += h1->n_pos ;
  h0->n_neg += h1->n_neg ;
  h0->outcomes = merge_outs(h0->outcomes,h1->outcomes,NOUTS) ;
}

static count_t sum_hist(hist_t *h) {
  return sum_outs(h->outcomes) ;
}


static void do_dump_outcome(FILE *fhist, int *o, count_t c, int show) {
  fprintf(fhist,"%-6"PCTR"%c>0:R0=%i; 1:R0=%i; x=%i;\n",c,show ? '*' : ':',o[out_0_r0_f],o[out_1_r0_f],o[x_f]);
}

static void just_dump_outcomes(FILE *fhist, hist_t *h) {
  outcome_t buff ;
  dump_outs(fhist,do_dump_outcome,h->outcomes,buff,NOUTS) ;
}

static void dump_hist(FILE *fhist,hist_t *h) {
  fprintf(fhist,"Test LB+dmbs Required\n") ;
  fprintf(fhist,"Histogram (%"PCTR" states)\n",finals_outs(h->outcomes)) ;
  just_dump_outcomes(fhist,h) ;
  int cond = h->n_neg == 0;
  fprintf(fhist,"%s\n",cond?"Ok":"No");
  fprintf(fhist,"\nWitnesses\n");
  fprintf(fhist,"Positive: %"PCTR", Negative: %"PCTR"\n",h->n_pos,h->n_neg);
  fprintf(fhist,"Condition forall (true) is %svalidated\n",h->n_neg == 0 ? "" : "NOT ");
}

/*******************************************************/
/* Context allocation, freeing and reinitialization    */
/*******************************************************/

static void init(ctx_t *_a) {
  int size_of_test = _a->_p->size_of_test;

  _a->seed = rand();
  _a->out_0_r0 = malloc_check(size_of_test*sizeof(*(_a->out_0_r0)));
  _a->out_1_r0 = malloc_check(size_of_test*sizeof(*(_a->out_1_r0)));
  _a->x = malloc_check(size_of_test*sizeof(*(_a->x)));
  _a->fst_barrier = pb_create(N);
  _a->s_or = po_create(N);
  for (int _p = N-1 ; _p >= 0 ; _p--) {
    _a->cpy_x[_p] = malloc_check(size_of_test*sizeof(*(_a->cpy_x[_p])));
  }
  _a->barrier = malloc_check(size_of_test*sizeof(*(_a->barrier)));
}

static void finalize(ctx_t *_a) {
  free((void *)_a->x);
  free((void *)_a->out_0_r0);
  free((void *)_a->out_1_r0);
  pb_free(_a->fst_barrier);
  po_free(_a->s_or);
  for (int _p = N-1 ; _p >= 0 ; _p--) {
    free((void *)_a->cpy_x[_p]);
  }
  free((void *)_a->barrier);
}

static void reinit(ctx_t *_a) {
  for (int _i = _a->_p->size_of_test-1 ; _i >= 0 ; _i--) {
    _a->x[_i] = 0;
    _a->out_0_r0[_i] = -239487;
    _a->out_1_r0[_i] = -239487;
    _a->barrier[_i] = 0;
  }
}

/**************************************/
/* Prefetch (and check) global values */
/**************************************/

static void check_globals(ctx_t *_a) {
  int *x = _a->x;
  for (int _i = _a->_p->size_of_test-1 ; _i >= 0 ; _i--) {
    if (rand_bit(&(_a->seed)) && x[_i] != 0) err(2,"check_globals failed");
  }
  pb_wait(_a->fst_barrier);
}


static void stabilize_globals(int _id, ctx_t *_a) {
  int size_of_test = _a->_p->size_of_test;

  int *x = _a->x;
  int **cpy_x = _a->cpy_x;

  pb_wait(_a->fst_barrier); 
  for ( ; ; ) {
    for (int _i = size_of_test-1 ; _i >= 0 ; _i--) {
      cpy_x[_id][_i] = x[_i];
    }
    po_reinit(_a->s_or);
    int _found;
    int _nxt_id = (_id+1) % N;
    _found = 0;
    for (int _i = size_of_test-1 ; _i >= 0 && !_found ; _i--) {
      if (cpy_x[_id][_i] != cpy_x[_nxt_id][_i]) { _found = 1; break; }
    }
    if (_found) { fprintf(stderr,"%i: Stabilizing final state!\n",_id); }
    if (!po_wait(_a->s_or,_found)) return ;
  }
}

/***************/
/* Litmus code */
/***************/

typedef struct {
  int th_id; /* I am running on this thread */
  int *cpu; /* On this cpu */
  ctx_t *_a;   /* In this context */
} parg_t;

static void *P0(void *_vb) {
  mbar();
  parg_t *_b = (parg_t *)_vb;
  ctx_t *_a = _b->_a;
  int _ecpu = _b->cpu[_b->th_id];
  force_one_affinity(_ecpu,AVAIL,_a->_p->verbose,"LB+dmbs");
  check_globals(_a);
  int _th_id = _b->th_id;
  int volatile *barrier = _a->barrier;
  int _size_of_test = _a->_p->size_of_test;
  int _stride = _a->_p->stride;
  int *x = _a->x;
  int *out_0_r0 = _a->out_0_r0;
  for (int _j = _stride ; _j > 0 ; _j--) {
    for (int _i = _size_of_test-_j ; _i >= 0 ; _i -= _stride) {
      barrier_wait(_th_id,_i,&barrier[_i]);
      int trashed_r1;
asm __volatile__ (
"\n"
"@START _litmus_P0\n"
"@_litmus_P0_0\n\t"
"ldr %[r0],[%[r2]]\n"
"@_litmus_P0_1\n\t"
"dmb\n"
"@_litmus_P0_2\n\t"
"mov %[r1],#1\n"
"@_litmus_P0_3\n\t"
"str %[r1],[%[r2]]\n"
"@END_litmus\n\t"
:[r0] "=&r" (out_0_r0[_i]),[r1] "=&r" (trashed_r1)
:[r2] "r" (&x[_i])
:"cc","memory"
);
    }
  }
  stabilize_globals(0,_a);
  mbar();
  return NULL;
}

static void *P1(void *_vb) {
  mbar();
  parg_t *_b = (parg_t *)_vb;
  ctx_t *_a = _b->_a;
  int _ecpu = _b->cpu[_b->th_id];
  force_one_affinity(_ecpu,AVAIL,_a->_p->verbose,"LB+dmbs");
  check_globals(_a);
  int _th_id = _b->th_id;
  int volatile *barrier = _a->barrier;
  int _size_of_test = _a->_p->size_of_test;
  int _stride = _a->_p->stride;
  int *x = _a->x;
  int *out_1_r0 = _a->out_1_r0;
  for (int _j = _stride ; _j > 0 ; _j--) {
    for (int _i = _size_of_test-_j ; _i >= 0 ; _i -= _stride) {
      barrier_wait(_th_id,_i,&barrier[_i]);
      int trashed_r1;
asm __volatile__ (
"\n"
"@START _litmus_P1\n"
"@_litmus_P1_0\n\t"
"ldr %[r0],[%[r2]]\n"
"@_litmus_P1_1\n\t"
"dmb\n"
"@_litmus_P1_2\n\t"
"mov %[r1],#2\n"
"@_litmus_P1_3\n\t"
"str %[r1],[%[r2]]\n"
"@END_litmus\n\t"
:[r0] "=&r" (out_1_r0[_i]),[r1] "=&r" (trashed_r1)
:[r2] "r" (&x[_i])
:"cc","memory"
);
    }
  }
  stabilize_globals(1,_a);
  mbar();
  return NULL;
}

typedef struct {
  pm_t *p_mutex;
  pb_t *p_barrier;
  param_t *_p;
  int z_id;
  int *cpus;
} zyva_t;

#define NT N

static void *zyva(void *_va) {
  zyva_t *_a = (zyva_t *) _va;
  param_t *_b = _a->_p;
  pb_wait(_a->p_barrier);
  pthread_t thread[NT];
  parg_t parg[N];
  f_t *fun[] = {&P0,&P1};
  hist_t *hist = alloc_hist();
  ctx_t ctx;
  ctx._p = _b;

  init(&ctx);
  for (int _p = N-1 ; _p >= 0 ; _p--) {
    parg[_p].th_id = _p; parg[_p]._a = &ctx;
    parg[_p].cpu = &(_a->cpus[0]);
  }

  for (int n_run = 0 ; n_run < _b->max_run ; n_run++) {
    if (_b->aff_rand) {
      pb_wait(_a->p_barrier);
      if (_a->z_id == 0) perm_prefix_ints(&ctx.seed,_a->cpus,_b->ncpus_used,_b->ncpus);
      pb_wait(_a->p_barrier);
    } else {
    }
    if (_b->verbose>1) fprintf(stderr, "Run %i of %i\r", n_run, _b->max_run);
    reinit(&ctx);
    if (_b->do_change) perm_funs(&ctx.seed,fun,N);
    for (int _p = NT-1 ; _p >= 0 ; _p--) {
      launch(&thread[_p],fun[_p],&parg[_p]);
    }
    if (_b->do_change) perm_threads(&ctx.seed,thread,NT);
    for (int _p = NT-1 ; _p >= 0 ; _p--) {
      join(&thread[_p]);
    }
    /* Log final states */
    for (int _i = _b->size_of_test-1 ; _i >= 0 ; _i--) {
      int _out_0_r0_i = ctx.out_0_r0[_i];
      int _out_1_r0_i = ctx.out_1_r0[_i];
      int _x_i = ctx.x[_i];
      outcome_t o;
      int cond;

      for (int _p = N-1 ; _p >= 0 ; _p--) {
        if (_x_i != ctx.cpy_x[_p][_i]) err(1,"LB+dmbs: global x unstabilized") ;
      }
      cond = final_cond();
      o[out_0_r0_f] = _out_0_r0_i;
      o[out_1_r0_f] = _out_1_r0_i;
      o[x_f] = _x_i;
      add_outcome(hist,1,o,!cond);
      if (cond) { hist->n_pos++; } else { hist->n_neg++; }
    }
  }

  finalize(&ctx);
  return hist;
}

#ifdef ASS
static void ass(FILE *out) { }
#else
#include "LB+dmbs.h"
#endif

static void report(FILE *out) {
  fprintf(out,"%s\n","%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
  fprintf(out,"%s\n","% Results for src/LB+dmbs.litmus %");
  fprintf(out,"%s\n","%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
  fprintf(out,"%s\n","ARM LB+dmbs");
  fprintf(out,"%s\n","\"DMBsRW Rfe DMBsRW Rfe\"");
  fprintf(out,"%s\n","{0:R2=x; 1:R2=x;}");
  fprintf(out,"%s\n"," P0          | P1          ;");
  fprintf(out,"%s\n"," LDR R0,[R2] | LDR R0,[R2] ;");
  fprintf(out,"%s\n"," DMB         | DMB         ;");
  fprintf(out,"%s\n"," MOV R1,#1   | MOV R1,#2   ;");
  fprintf(out,"%s\n"," STR R1,[R2] | STR R1,[R2] ;");
  fprintf(out,"%s\n","");
  fprintf(out,"%s\n","locations [x;  0:R0;  1:R0; ]");
  fprintf(out,"%s\n","forall (true)");
  fprintf(out,"Generated assembler\n");
  ass(out);
}

static int run(cmd_t *cmd,cpus_t *def_all_cpus,FILE *out) {
  if (cmd->prelude) report(out);
  tsc_t start = timeofday();
  param_t prm ;
/* Set some parameters */
  prm.verbose = cmd->verbose;
  prm.size_of_test = cmd->size_of_test;
  prm.max_run = cmd->max_run;
  prm.stride = cmd->stride;
  prm.do_change = 1;
  if (cmd->fix) prm.do_change = 0;
/* Computes number of test concurrent instances */
  int n_avail = cmd->avail > 0 ? cmd->avail : cmd->aff_cpus->sz;
  if (n_avail >  cmd->aff_cpus->sz) fprintf(stderr,"Warning: avail=%i, available=%i\n",n_avail, cmd->aff_cpus->sz);
  int n_exe;
  if (cmd->n_exe > 0) {
    n_exe = cmd->n_exe;
  } else {
    n_exe = n_avail < N ? 1 : n_avail / N;
  }
/* Set affinity parameters */
  cpus_t *all_cpus = cmd->aff_cpus;
  int aff_cpus_sz = cmd->aff_mode == aff_random ? max(all_cpus->sz,N*n_exe) : N*n_exe;
  int aff_cpus[aff_cpus_sz];
  prm.aff_rand = cmd->aff_mode == aff_random;
  prm.ncpus = aff_cpus_sz;
  prm.ncpus_used = N*n_exe;
/* Show parameters to user */
  if (prm.verbose) {
    fprintf(stderr, "LB+dmbs: n=%i, r=%i, s=%i",n_exe,prm.max_run,prm.size_of_test);
    fprintf(stderr,", st=%i",prm.stride);
    if (cmd->aff_mode == aff_incr) {
      fprintf(stderr, ", i=%i",cmd->aff_incr);
    } else if (cmd->aff_mode == aff_random) {
      fprintf(stderr,", +ra");
    } else if (cmd->aff_mode == aff_custom) {
      fprintf(stderr,", +ca");
    }
    fprintf(stderr,", p='");
    cpus_dump(stderr,cmd->aff_cpus);
    fprintf(stderr,"'");
    fprintf(stderr,"\n");
  }
  if (cmd->aff_mode == aff_random) {
    for (int k = 0 ; k < aff_cpus_sz ; k++) {
      aff_cpus[k] = all_cpus->cpu[k % all_cpus->sz];
    }
  }
  pthread_t th[n_exe];
  zyva_t zarg[n_exe];
  pm_t *p_mutex = pm_create();
  pb_t *p_barrier = pb_create(n_exe);
  int next_cpu = 0;
  int delta = cmd->aff_incr;
  if (delta <= 0) {
    for (int k=0 ; k < all_cpus->sz ; k++) all_cpus->cpu[k] = -1;
    delta = 1;
  } else {
    delta %= all_cpus->sz;
  }
  int start_scan=0, max_start=gcd(delta,all_cpus->sz);
  int *aff_p = aff_cpus;
  for (int k=0 ; k < n_exe ; k++) {
    zyva_t *p = &zarg[k];
    p->_p = &prm;
    p->p_mutex = p_mutex; p->p_barrier = p_barrier; 
    p->z_id = k;
    p->cpus = aff_p;
    if (cmd->aff_mode != aff_incr) {
      aff_p += N;
    } else {
      for (int i=0 ; i < N ; i++) {
        *aff_p = all_cpus->cpu[next_cpu]; aff_p++;
        next_cpu += delta; next_cpu %= all_cpus->sz;
        if (next_cpu == start_scan) {
          start_scan++ ; start_scan %= max_start;
          next_cpu = start_scan;
        }
      }
    }
    launch(&th[k],zyva,p);
  }

  count_t n_outs = prm.size_of_test; n_outs *= prm.max_run;
  hist_t *hist = (hist_t *)join(&th[0]);
  for (int k=1 ; k < n_exe ; k++) {
    hist_t *hk = (hist_t *)join(&th[k]);
    if (sum_hist(hk) != n_outs || hk->n_pos + hk->n_neg != n_outs) {
      err(1,"sum_hist");
    }
    merge_hists(hist,hk);
    free_hist(hk);
  }
  cpus_free(all_cpus);
  tsc_t total = timeofday() - start;
  pm_free(p_mutex);
  pb_free(p_barrier);

  n_outs *= n_exe ;
  if (sum_hist(hist) != n_outs || hist->n_pos + hist->n_neg != n_outs) {
    err(1,"sum_hist") ;
  }
  dump_hist(out,hist);
  count_t p_true = hist->n_pos, p_false = hist->n_neg;
  free_hist(hist);
  fprintf(out,"%s=%s\n","Hash","3cc05340792f4d468fe242f4feaf1a48");
  fprintf(out,"%s=%s\n","Cycle","Rfe DMBsRW Rfe DMBsRW");
  fprintf(out,"Relax LB+dmbs %s %s\n",p_true > 0 ? "Ok" : "No","");
  fprintf(out,"%s=%s\n","Safe","Rfe DMBsRW");
  fprintf(out,"%s=%s\n","Com","Rf Rf");
  fprintf(out,"%s=%s\n","Orig","DMBsRW Rfe DMBsRW Rfe");
  fprintf(out,"Observation LB+dmbs %s %"PCTR" %"PCTR"\n",!p_true ? "Never" : !p_false ? "Always" : "Sometimes",p_true,p_false) ;
  fprintf(out,"Time LB+dmbs %.2f\n",total / 1000000.0) ;
  fflush(out);
  return(p_true && p_false);
}

int LB_2B_dmbs(int argc, char **argv, FILE *out) {
  cpus_t *def_all_cpus = read_force_affinity(AVAIL,0);
  cmd_t def = { 0, NUMBER_OF_RUN, SIZE_OF_TEST, STRIDE, AVAIL, 0, 0, aff_incr, 0, AFF_INCR, def_all_cpus, -1, MAX_LOOP, NULL, NULL, -1, -1, -1, 0, 1};
  cmd_t cmd = def;
  parse_cmd(argc,argv,&def,&cmd);
  run(&cmd,def_all_cpus,out);
  if (def_all_cpus != cmd.aff_cpus) cpus_free(def_all_cpus);
  return  EXIT_SUCCESS;
}
