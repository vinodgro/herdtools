FILE=$1
cat <<EOF
static void ass(FILE *out) {
EOF
awk '{printf("  fprintf(out,\"%%s\\n\",\"%s\");\n",$0) }' $1
cat <<EOF
}
EOF