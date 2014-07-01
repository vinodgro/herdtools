{
module Make(O:LexUtils.Config) = struct
open CPP11Parser
open Lexing
open LexMisc

module LU = LexUtils.Make(O)


(* Compiled efficiently by the next version of ocaml *)
let tr_name = function
| "volatile" -> VOLATILE
| "unsigned" -> UNSIGNED
| "signed" -> SIGNED
| "_Atomic" -> ATOMIC
| "char" -> CHAR
| "short" -> SHORT
| "int" ->  INT
| "long" -> LONG
| "float" -> FLOAT
| "double" -> DOUBLE
| "_Bool" -> BOOL
| "atomic_bool" -> ATOMIC_TYPE "_Bool"
| "atomic_char" -> ATOMIC_TYPE "char"
| "atomic_schar" -> ATOMIC_TYPE "signed char"
| "atomic_uchar" -> ATOMIC_TYPE "unsigned char"
| "atomic_short" -> ATOMIC_TYPE "short"
| "atomic_ushort" -> ATOMIC_TYPE "unsigned short"
| "atomic_int" -> ATOMIC_TYPE "int"
| "atomic_uint" -> ATOMIC_TYPE "unsigned int"
| "atomic_long" -> ATOMIC_TYPE "long"
| "atomic_ulong" -> ATOMIC_TYPE "unsigned long"
| "atomic_llong" -> ATOMIC_TYPE "long long"
| "atomic_ullong" -> ATOMIC_TYPE "unsigned long long"
| "atomic_char16_t" -> ATOMIC_TYPE "__CHAR16_TYPE__"
| "atomic_char32_t" -> ATOMIC_TYPE "__CHAR32_TYPE__"
| "atomic_wchar_t" -> ATOMIC_TYPE "__WCHAR_TYPE__"
| "atomic_int_least8_t" -> ATOMIC_TYPE "__INT_LEAST8_TYPE__"
| "atomic_uint_least8_t" -> ATOMIC_TYPE "__UINT_LEAST8_TYPE__"
| "atomic_int_least16_t" -> ATOMIC_TYPE "__INT_LEAST16_TYPE__"
| "atomic_uint_least16_t" -> ATOMIC_TYPE "__UINT_LEAST16_TYPE__"
| "atomic_int_least32_t" -> ATOMIC_TYPE "__INT_LEAST32_TYPE__"
| "atomic_uint_least32_t" -> ATOMIC_TYPE "__UINT_LEAST32_TYPE__"
| "atomic_int_least64_t" -> ATOMIC_TYPE "__INT_LEAST64_TYPE__"
| "atomic_uint_least64_t" -> ATOMIC_TYPE "__UINT_LEAST64_TYPE__"
| "atomic_int_fast8_t" -> ATOMIC_TYPE "__INT_FAST8_TYPE__"
| "atomic_uint_fast8_t" -> ATOMIC_TYPE "__UINT_FAST8_TYPE__"
| "atomic_int_fast16_t" -> ATOMIC_TYPE "__INT_FAST16_TYPE__"
| "atomic_uint_fast16_t" -> ATOMIC_TYPE "__UINT_FAST16_TYPE__"
| "atomic_int_fast32_t" -> ATOMIC_TYPE "__INT_FAST32_TYPE__"
| "atomic_uint_fast32_t" -> ATOMIC_TYPE "__UINT_FAST32_TYPE__"
| "atomic_int_fast64_t" -> ATOMIC_TYPE "__INT_FAST64_TYPE__"
| "atomic_uint_fast64_t" -> ATOMIC_TYPE "__UINT_FAST64_TYPE__"
| "atomic_intptr_t" -> ATOMIC_TYPE "__INTPTR_TYPE__"
| "atomic_uintptr_t" -> ATOMIC_TYPE "__UINTPTR_TYPE__"
| "atomic_size_t" -> ATOMIC_TYPE "__SIZE_TYPE__"
| "atomic_ptrdiff_t" -> ATOMIC_TYPE "__PTRDIFF_TYPE__"
| "atomic_intmax_t" -> ATOMIC_TYPE "__INTMAX_TYPE__"
| "atomic_uintmax_t" -> ATOMIC_TYPE "__UINTMAX_TYPE__"
|  x -> IDENTIFIER x

}

let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = (alpha | '_') (alpha|digit|'_')*
let num = digit+

rule token = parse
| [' ''\t'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "/*"      { LU.skip_c_comment lexbuf ; token lexbuf }
| "//"      { LU.skip_c_line_comment lexbuf ; token lexbuf } 
| '-' ? num as x { CONSTANT (int_of_string x) }
| 'P' (num as x) { PROC (int_of_string x) }
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| ':' { COLON }
| '*' { STAR }
| '(' { LPAR }
| ')' { RPAR }
| '{' { LBRACE }
| '}' { RBRACE }
| "while" { WHILE }
| "if"    { IF }
| "else"  { ELSE }
| '=' {EQ}
| "==" {EQ_OP}
| '.' {DOT}
| "memory_order_acquire" {MEMORDER (CPP11Base.Acq)}
| "memory_order_release" {MEMORDER (CPP11Base.Rel)}
| "memory_order_acq_rel" {MEMORDER (CPP11Base.Acq_Rel)}
| "memory_order_seq_cst" {MEMORDER (CPP11Base.SC)}
| "memory_order_relaxed" {MEMORDER (CPP11Base.Rlx)}
| "memory_order_consume" {MEMORDER (CPP11Base.Con)}
| "fence" { FENCE }
| "atomic_load"  { LD }
| "atomic_store" { ST }
| "atomic_load_explicit"  { LD_EXPLICIT }
| "atomic_store_explicit" { ST_EXPLICIT }
| "lock"  { LOCK }
| "WCAS"  { WCAS }
| "SCAS"  { SCAS }
| "unlock"    { UNLOCK }
| "LK" { LK }
| name as x
    { tr_name x  }
| eof { EOF }
| "" { LexMisc.error "C lexer" lexbuf }

{
let token lexbuf =
   let tok = token lexbuf in
   if O.debug then begin
     Printf.eprintf
       "%a: Lexed '%s'\n"
       Pos.pp_pos2
       (lexeme_start_p lexbuf,lexeme_end_p lexbuf)
       (lexeme lexbuf)
   end ;
   tok

end
}
