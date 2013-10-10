static void ass(FILE *out) {
  fprintf(out,"%s\n","@START _litmus_P0");
  fprintf(out,"%s\n","	mov r8,#1");
  fprintf(out,"%s\n","	str r8,[r1]");
  fprintf(out,"%s\n","	dmb");
  fprintf(out,"%s\n","	ldr r5,[r1]");
}
