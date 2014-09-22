typedef struct {
  log_t key ;
  param_t p ;
  count_t c ;
  int ok ;
} entry_t ;

static void pp_entry(FILE *fp,entry_t *p, char **group) {
  fprintf(fp,"%-6" PCTR "%c>",p->c,p->ok ? '*' : ':') ;
  pp_log(fp,&p->key) ;
  fprintf(fp," # ") ;
  pp_param(fp,&p->p) ;
  if (group) fprintf(fp," %s",group[p->p.part]);
  fprintf(fp,"\n") ;
}

typedef struct {
  int nhash ;
  entry_t t[HASHSZ] ;
} hash_t ;

static void pp_hash(FILE *fp,hash_t *t,char **group) {
  for (int k = 0 ; k < HASHSZ ; k++) {
    entry_t *p = t->t+k ;
    if (p->c > 0) pp_entry(fp,p,group) ;
  }
}

#if 0
static void pp_hash_ok(FILE *fp,hash_t *t,char **group) {
  for (int k = 0 ; k < HASHSZ ; k++) {
    entry_t *p = t->t+k ;
    if (p->c > 0 && p->ok) pp_entry(fp,p,group) ;
  }
}
#endif


static void hash_init(hash_t *t) {
  t->nhash = 0 ;
  for (int k = 0 ; k < HASHSZ ; k++) t->t[k].c = 0 ;
}


static void hash_add(hash_t *t,log_t *key, param_t *v,count_t c,int ok) {
  uint32_t h = hash_log(key) ;
  h = h % HASHSZ ;
  for (int k = 0 ; k < HASHSZ ;  k++) {
    entry_t *p = t->t + h ;
    if (p->c == 0) { /* New entry */
      p->key = *key ;
      p->p = *v ;
      p->c = c ;
      p->ok = ok ;
      t->nhash++ ;
      return ;
    } else if (eq_log(key,&p->key)) {
      p->c += c ;
      return ;
    }
    h++ ;
    h %= HASHSZ ;
  }
  fprintf(stderr,"Hash table is full\n") ;
  exit(2) ;
}

static void hash_adds(hash_t *t, hash_t *f) {
  for (int k = 0 ; k < HASHSZ ; k++) {
    entry_t *p = f->t+k ;
    if (p->c > 0) {
      hash_add(t,&p->key,&p->p,p->c,p->ok) ;
    }
  }
}
