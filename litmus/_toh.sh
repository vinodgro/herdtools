FILE=$1
cat <<EOF
static void ass(FILE *out) {
EOF
sed $1 -e 's|"|\\"|g' | awk '{printf("  fprintf(out,\"%%s\\n\",\"%s\");\n",$0) }'
cat <<EOF
}
EOF
