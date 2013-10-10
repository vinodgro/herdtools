static void ass(FILE *out) {
  fprintf(out,"%s\n","@START _litmus_P0");
  fprintf(out,"%s\n","	mov r1,#1");
  fprintf(out,"%s\n","	str r1,[r2]");
  fprintf(out,"%s\n","	mov r3,#2");
  fprintf(out,"%s\n","	str r3,[r2]");
}
