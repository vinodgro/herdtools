(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

{
(* Compiled efficiently by the next version of ocaml *)
let tr_name = function
| "volatile" -> CParser.VOLATILE
| "unsigned" -> CParser.UNSIGNED
| "signed" -> CParser.SIGNED
| "_Atomic" -> CParser.ATOMIC
| "long" -> CParser.LONG
| "double" -> CParser.DOUBLE
| "_Bool" -> CParser.BOOL
| "atomic_bool" -> CParser.ATOMIC_NAME "_Atomic _Bool"
| "atomic_char" -> CParser.ATOMIC_NAME "_Atomic char"
| "atomic_schar" -> CParser.ATOMIC_NAME "_Atomic signed char"
| "atomic_uchar" -> CParser.ATOMIC_NAME "_Atomic unsigned char"
| "atomic_short" -> CParser.ATOMIC_NAME "_Atomic short"
| "atomic_ushort" -> CParser.ATOMIC_NAME "_Atomic unsigned short"
| "atomic_int" -> CParser.ATOMIC_NAME "_Atomic int"
| "atomic_uint" -> CParser.ATOMIC_NAME "_Atomic unsigned int"
| "atomic_long" -> CParser.ATOMIC_NAME "_Atomic long"
| "atomic_ulong" -> CParser.ATOMIC_NAME "_Atomic unsigned long"
| "atomic_llong" -> CParser.ATOMIC_NAME "_Atomic long long"
| "atomic_ullong" -> CParser.ATOMIC_NAME "_Atomic unsigned long long"
| "atomic_char16_t" -> CParser.ATOMIC_NAME "_Atomic __CHAR16_TYPE__"
| "atomic_char32_t" -> CParser.ATOMIC_NAME "_Atomic __CHAR32_TYPE__"
| "atomic_wchar_t" -> CParser.ATOMIC_NAME "_Atomic __WCHAR_TYPE__"
| "atomic_int_least8_t" -> CParser.ATOMIC_NAME "_Atomic __INT_LEAST8_TYPE__"
| "atomic_uint_least8_t" -> CParser.ATOMIC_NAME "_Atomic __UINT_LEAST8_TYPE__"
| "atomic_int_least16_t" -> CParser.ATOMIC_NAME "_Atomic __INT_LEAST16_TYPE__"
| "atomic_uint_least16_t" -> CParser.ATOMIC_NAME "_Atomic __UINT_LEAST16_TYPE__"
| "atomic_int_least32_t" -> CParser.ATOMIC_NAME "_Atomic __INT_LEAST32_TYPE__"
| "atomic_uint_least32_t" -> CParser.ATOMIC_NAME "_Atomic __UINT_LEAST32_TYPE__"
| "atomic_int_least64_t" -> CParser.ATOMIC_NAME "_Atomic __INT_LEAST64_TYPE__"
| "atomic_uint_least64_t" -> CParser.ATOMIC_NAME "_Atomic __UINT_LEAST64_TYPE__"
| "atomic_int_fast8_t" -> CParser.ATOMIC_NAME "_Atomic __INT_FAST8_TYPE__"
| "atomic_uint_fast8_t" -> CParser.ATOMIC_NAME "_Atomic __UINT_FAST8_TYPE__"
| "atomic_int_fast16_t" -> CParser.ATOMIC_NAME "_Atomic __INT_FAST16_TYPE__"
| "atomic_uint_fast16_t" -> CParser.ATOMIC_NAME "_Atomic __UINT_FAST16_TYPE__"
| "atomic_int_fast32_t" -> CParser.ATOMIC_NAME "_Atomic __INT_FAST32_TYPE__"
| "atomic_uint_fast32_t" -> CParser.ATOMIC_NAME "_Atomic __UINT_FAST32_TYPE__"
| "atomic_int_fast64_t" -> CParser.ATOMIC_NAME "_Atomic __INT_FAST64_TYPE__"
| "atomic_uint_fast64_t" -> CParser.ATOMIC_NAME "_Atomic __UINT_FAST64_TYPE__"
| "atomic_intptr_t" -> CParser.ATOMIC_NAME "_Atomic __INTPTR_TYPE__"
| "atomic_uintptr_t" -> CParser.ATOMIC_NAME "_Atomic __UINTPTR_TYPE__"
| "atomic_size_t" -> CParser.ATOMIC_NAME "_Atomic __SIZE_TYPE__"
| "atomic_ptrdiff_t" -> CParser.ATOMIC_NAME "_Atomic __PTRDIFF_TYPE__"
| "atomic_intmax_t" -> CParser.ATOMIC_NAME "_Atomic __INTMAX_TYPE__"
| "atomic_uintmax_t" -> CParser.ATOMIC_NAME "_Atomic __UINTMAX_TYPE__"
|  x -> CParser.NAME x

}
let num = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let name = (alpha (num|alpha)*)

rule main = parse
| [' ''\t']+ { main lexbuf }
| '\n' { Lexing.new_line lexbuf ; main lexbuf ; }
| 'P' (num+ as x) { CParser.PROC (int_of_string x) }
| '(' { CParser.LPAREN }
| ')' { CParser.RPAREN }
| ',' { CParser.COMMA }
| '{'
    { let buf = Buffer.create 4096 in
      get_body 0 buf lexbuf;
      CParser.BODY (Buffer.contents buf)
    }
| '*' { CParser.STAR }
| name as x { tr_name x }
| eof { CParser.EOF }
| "" { LexMisc.error "CLexer" lexbuf }

and get_body i buf = parse
| '\n' as lxm
    { Lexing.new_line lexbuf ;
      Buffer.add_char buf lxm ;
      get_body i buf lexbuf ; }
| '{' as lxm
    { Buffer.add_char buf lxm;
      get_body (succ i) buf lexbuf
    }
| '}' as lxm
    { if i > 0 then begin
       Buffer.add_char buf lxm;
       get_body (pred i) buf lexbuf
     end
    }
| eof { LexMisc.error "eof in body" lexbuf }
| _ as lxm { Buffer.add_char buf lxm; get_body i buf lexbuf }
