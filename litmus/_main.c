/***************/
/* Entry point */
/***************/

static scan_t arg[AVAIL];
static pthread_t th[AVAIL];
static hash_t hash ;

int main(int argc,char **argv) {
  global.nexe = NEXE ;
  global.noccs = NOCCS ;
  global.nruns = NUMBER_OF_RUN ;
  for (int id=0; id < AVAIL ; id++) {
    arg[id].id = id;
    arg[id].g = &global;
  }
  for (int id=0; id < AVAIL ; id++) launch(&th[id],scan,&arg[id]);
  for (int id=0; id < AVAIL ; id++) join(&th[id]);

  hash_init(&hash) ;  
  for (int k=0 ; k < NEXE ; k++) {
    hash_adds(&hash,&global.ctx[k].t) ;
  }
  pp_hash(stdout,&hash,group);
  fprintf(stdout,"** condition **\n") ;
  for (int k=0 ; k < NEXE ; k++) {
    pp_hash_ok(stdout,&global.ctx[k].t,group);
  }

  return 0;
}
