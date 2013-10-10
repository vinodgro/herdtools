#include <stdio.h>
#include <stdlib.h>

/* Declarations of tests entry points */
extern int R_2B_dmbs(int argc,char **argv,FILE *out);
extern int MP_2B_dmbs(int argc,char **argv,FILE *out);
extern int R_2B_dmb_2B_pos(int argc,char **argv,FILE *out);
extern int MP_2B_dmb_2B_pos(int argc,char **argv,FILE *out);
extern int WW_2B_dmb(int argc,char **argv,FILE *out);
extern int _2_2B_2W_2B_dmbs(int argc,char **argv,FILE *out);
extern int S_2B_dmbs(int argc,char **argv,FILE *out);
extern int _2_2B_2W_2B_dmb_2B_pos(int argc,char **argv,FILE *out);
extern int S_2B_dmb_2B_pos(int argc,char **argv,FILE *out);
extern int WR_2B_dmb(int argc,char **argv,FILE *out);
extern int SB_2B_dmbs(int argc,char **argv,FILE *out);
extern int SB_2B_dmb_2B_pos(int argc,char **argv,FILE *out);
extern int R_2B_pos_2B_dmb(int argc,char **argv,FILE *out);
extern int RW_2B_dmb(int argc,char **argv,FILE *out);
extern int LB_2B_dmbs(int argc,char **argv,FILE *out);
extern int S_2B_pos_2B_dmb(int argc,char **argv,FILE *out);
extern int LB_2B_dmb_2B_pos(int argc,char **argv,FILE *out);
extern int W_2B_RW_2B_dmb(int argc,char **argv,FILE *out);
extern int MP_2B_pos_2B_dmb(int argc,char **argv,FILE *out);
extern int W_2B_RR_2B_dmb(int argc,char **argv,FILE *out);
extern int R_2B_poss(int argc,char **argv,FILE *out);
extern int MP_2B_poss(int argc,char **argv,FILE *out);
extern int WW(int argc,char **argv,FILE *out);
extern int _2_2B_2W_2B_poss(int argc,char **argv,FILE *out);
extern int S_2B_poss(int argc,char **argv,FILE *out);
extern int WR(int argc,char **argv,FILE *out);
extern int SB_2B_poss(int argc,char **argv,FILE *out);
extern int RW(int argc,char **argv,FILE *out);
extern int LB_2B_poss(int argc,char **argv,FILE *out);
extern int W_2B_RW(int argc,char **argv,FILE *out);
extern int W_2B_RR(int argc,char **argv,FILE *out);

/* Date function */
#include <time.h>
static void my_date(FILE *out) {
  time_t t = time(NULL);
  fprintf(out,"%s",ctime(&t));
}

/* Postlude */
static void end_report(int argc,char **argv,FILE *out) {
  fprintf(out,"%s\n","Revision 15, version 5.99");
  fprintf(out,"%s\n","Command line: litmus -mach dragon-arm -driver C -mem direct -st 1 -o SRC/DRAGON src/@all");
  fprintf(out,"%s\n","Parameters");
  fprintf(out,"%s\n","#define SIZE_OF_TEST 10000");
  fprintf(out,"%s\n","#define NUMBER_OF_RUN 100");
  fprintf(out,"%s\n","#define AVAIL 2");
  fprintf(out,"%s\n","#define STRIDE 1");
  fprintf(out,"%s\n","#define MAX_LOOP 0");
  fprintf(out,"%s\n","/* gcc options: -D_GNU_SOURCE -Wall -std=gnu99 -march=armv7-a -marm -O2 -pthread */");
  fprintf(out,"%s\n","/* gcc link options: -static */");
  fprintf(out,"%s\n","/* barrier: user */");
  fprintf(out,"%s\n","/* launch: changing */");
  fprintf(out,"%s\n","/* affinity: incr0 */");
  fprintf(out,"%s\n","/* prealloc: false */");
  fprintf(out,"%s\n","/* memory: direct */");
  fprintf(out,"%s\n","/* stride: 1 */");
  fprintf(out,"%s\n","/* safer: write */");
  fprintf(out,"%s\n","/* preload: random */");
  fprintf(out,"%s\n","/* speedcheck: no */");
  fprintf(out,"%s\n","/* proc used: 2 */");
/* Command line options */
  fprintf(out,"Command:");
  for ( ; *argv ; argv++) {
    fprintf(out," %s",*argv);
  }
  putc('\n',out);
}

/* Run all tests */
static void run(int argc,char **argv,FILE *out) {
  my_date(out);
  R_2B_dmbs(argc,argv,out);
  MP_2B_dmbs(argc,argv,out);
  R_2B_dmb_2B_pos(argc,argv,out);
  MP_2B_dmb_2B_pos(argc,argv,out);
  WW_2B_dmb(argc,argv,out);
  _2_2B_2W_2B_dmbs(argc,argv,out);
  S_2B_dmbs(argc,argv,out);
  _2_2B_2W_2B_dmb_2B_pos(argc,argv,out);
  S_2B_dmb_2B_pos(argc,argv,out);
  WR_2B_dmb(argc,argv,out);
  SB_2B_dmbs(argc,argv,out);
  SB_2B_dmb_2B_pos(argc,argv,out);
  R_2B_pos_2B_dmb(argc,argv,out);
  RW_2B_dmb(argc,argv,out);
  LB_2B_dmbs(argc,argv,out);
  S_2B_pos_2B_dmb(argc,argv,out);
  LB_2B_dmb_2B_pos(argc,argv,out);
  W_2B_RW_2B_dmb(argc,argv,out);
  MP_2B_pos_2B_dmb(argc,argv,out);
  W_2B_RR_2B_dmb(argc,argv,out);
  R_2B_poss(argc,argv,out);
  MP_2B_poss(argc,argv,out);
  WW(argc,argv,out);
  _2_2B_2W_2B_poss(argc,argv,out);
  S_2B_poss(argc,argv,out);
  WR(argc,argv,out);
  SB_2B_poss(argc,argv,out);
  RW(argc,argv,out);
  LB_2B_poss(argc,argv,out);
  W_2B_RW(argc,argv,out);
  W_2B_RR(argc,argv,out);
  end_report(argc,argv,out);
  my_date(out);
}

int main(int argc,char **argv) {
  run(argc,argv,stdout);
  return 0;
}
