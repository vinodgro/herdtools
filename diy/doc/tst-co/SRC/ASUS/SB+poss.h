static void ass(FILE *out) {
  fprintf(out,"%s\n","@START _litmus_P1");
  fprintf(out,"%s\n","	mov ip,#2");
  fprintf(out,"%s\n","	str ip,[r1]");
  fprintf(out,"%s\n","	ldr r4,[r1]");
  fprintf(out,"%s\n","@START _litmus_P0");
  fprintf(out,"%s\n","	mov ip,#1");
  fprintf(out,"%s\n","	str ip,[r1]");
  fprintf(out,"%s\n","	ldr r4,[r1]");
}
