{
module Make(O:LexUtils.Config) = struct
open CParser
open Lexing
open LexMisc

module LU = LexUtils.Make(O)


(* Compiled efficiently by the next version of ocaml *)
let tr_name = function
| "volatile" -> VOLATILE
| "unsigned" -> UNSIGNED
| "signed" -> SIGNED
| "_Atomic" -> ATOMIC
| "long" -> LONG
| "int" -> INT
| "double" -> DOUBLE
| "_Bool" -> BOOL
| "atomic_bool" -> ATOMIC_NAME "_Atomic _Bool"
| "atomic_char" -> ATOMIC_NAME "_Atomic char"
| "atomic_schar" -> ATOMIC_NAME "_Atomic signed char"
| "atomic_uchar" -> ATOMIC_NAME "_Atomic unsigned char"
| "atomic_short" -> ATOMIC_NAME "_Atomic short"
| "atomic_ushort" -> ATOMIC_NAME "_Atomic unsigned short"
| "atomic_int" -> ATOMIC_NAME "_Atomic int"
| "atomic_uint" -> ATOMIC_NAME "_Atomic unsigned int"
| "atomic_long" -> ATOMIC_NAME "_Atomic long"
| "atomic_ulong" -> ATOMIC_NAME "_Atomic unsigned long"
| "atomic_llong" -> ATOMIC_NAME "_Atomic long long"
| "atomic_ullong" -> ATOMIC_NAME "_Atomic unsigned long long"
| "atomic_char16_t" -> ATOMIC_NAME "_Atomic __CHAR16_TYPE__"
| "atomic_char32_t" -> ATOMIC_NAME "_Atomic __CHAR32_TYPE__"
| "atomic_wchar_t" -> ATOMIC_NAME "_Atomic __WCHAR_TYPE__"
| "atomic_int_least8_t" -> ATOMIC_NAME "_Atomic __INT_LEAST8_TYPE__"
| "atomic_uint_least8_t" -> ATOMIC_NAME "_Atomic __UINT_LEAST8_TYPE__"
| "atomic_int_least16_t" -> ATOMIC_NAME "_Atomic __INT_LEAST16_TYPE__"
| "atomic_uint_least16_t" -> ATOMIC_NAME "_Atomic __UINT_LEAST16_TYPE__"
| "atomic_int_least32_t" -> ATOMIC_NAME "_Atomic __INT_LEAST32_TYPE__"
| "atomic_uint_least32_t" -> ATOMIC_NAME "_Atomic __UINT_LEAST32_TYPE__"
| "atomic_int_least64_t" -> ATOMIC_NAME "_Atomic __INT_LEAST64_TYPE__"
| "atomic_uint_least64_t" -> ATOMIC_NAME "_Atomic __UINT_LEAST64_TYPE__"
| "atomic_int_fast8_t" -> ATOMIC_NAME "_Atomic __INT_FAST8_TYPE__"
| "atomic_uint_fast8_t" -> ATOMIC_NAME "_Atomic __UINT_FAST8_TYPE__"
| "atomic_int_fast16_t" -> ATOMIC_NAME "_Atomic __INT_FAST16_TYPE__"
| "atomic_uint_fast16_t" -> ATOMIC_NAME "_Atomic __UINT_FAST16_TYPE__"
| "atomic_int_fast32_t" -> ATOMIC_NAME "_Atomic __INT_FAST32_TYPE__"
| "atomic_uint_fast32_t" -> ATOMIC_NAME "_Atomic __UINT_FAST32_TYPE__"
| "atomic_int_fast64_t" -> ATOMIC_NAME "_Atomic __INT_FAST64_TYPE__"
| "atomic_uint_fast64_t" -> ATOMIC_NAME "_Atomic __UINT_FAST64_TYPE__"
| "atomic_intptr_t" -> ATOMIC_NAME "_Atomic __INTPTR_TYPE__"
| "atomic_uintptr_t" -> ATOMIC_NAME "_Atomic __UINTPTR_TYPE__"
| "atomic_size_t" -> ATOMIC_NAME "_Atomic __SIZE_TYPE__"
| "atomic_ptrdiff_t" -> ATOMIC_NAME "_Atomic __PTRDIFF_TYPE__"
| "atomic_intmax_t" -> ATOMIC_NAME "_Atomic __INTMAX_TYPE__"
| "atomic_uintmax_t" -> ATOMIC_NAME "_Atomic __UINTMAX_TYPE__"
|  x -> NAME x

}

let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = (alpha | '_') (alpha|digit|'_')*
let num = digit+

rule token = parse
| [' ''\t'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| '-' ? num as x { NUM (int_of_string x) }
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
| '.' {DOT}
| "memory_order_acquire" {MEMORDER (CBase.Acq)}
| "memory_order_release" {MEMORDER (CBase.Rel)}
| "memory_order_acq_rel" {MEMORDER (CBase.Acq_Rel)}
| "memory_order_seq_cst" {MEMORDER (CBase.SC)}
| "memory_order_relaxed" {MEMORDER (CBase.Rlx)}
| "memory_order_consume" {MEMORDER (CBase.Con)}
| "mutex"      {LOCATIONKIND (CBase.LK_mutex)}
| "atomic"     {LOCATIONKIND (CBase.LK_atomic)}
| "nonatomic"  {LOCATIONKIND (CBase.LK_nonatomic)}
| "fence" { FENCE }
| "atomic_load"  { LD }
| "atomic_store" { ST }
| "lock"  { LOCK }
| "WCAS"  { WCAS }
| "SCAS"  { SCAS }
| "unlock"    { UNLOCK }
| "LK" { LK }
| name as x { 
  match CBase.parse_reg x with
  | Some r -> ARCH_REG r
  | None -> tr_name x 
  }
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
