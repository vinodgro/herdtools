/***************/
/* Entry point */
/***************/

static scan_t arg[AVAIL];
static pthread_t th[AVAIL];

int RUN(int argc,char **argv,FILE *out) {
#ifdef OUT
  parse_param(*argv,global.parse,PARSESZ,argv+1) ;
#ifdef PRELUDE
  prelude(out) ;
#endif
  tsc_t start = timeofday();
#endif
  global.nexe = NEXE ;
  global.noccs = NOCCS ;
  global.nruns = NUMBER_OF_RUN ;
  for (int id=0; id < AVAIL ; id++) {
    arg[id].id = id;
    arg[id].g = &global;
  }
  for (int id=0; id < AVAIL ; id++) launch(&th[id],scan,&arg[id]);
  for (int id=0; id < AVAIL ; id++) join(&th[id]);

  int nexe = global.nexe ;
  hash_init(&global.hash) ;  
  for (int k=0 ; k < nexe ; k++) {
    hash_adds(&global.hash,&global.ctx[k].t) ;
  }
#ifdef OUT
  tsc_t total = timeofday()-start;
  count_t p_true = 0, p_false = 0;
  for (int k = 0 ; k < HASHSZ ; k++) {
    entry_t *e = &global.hash.t[k];
    if (e->c > 0) {
      if (e->ok) {
        p_true += e->c ;
      } else {
        p_false += e->c;
      }
    }
  }
  postlude(out,&global,p_true,p_false,total);
#endif
  return EXIT_SUCCESS;
}

#ifdef MAIN
int main (int argc,char **argv) {
  return RUN(argc,argv,stdout) ;
}
#endif
