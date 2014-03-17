(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

{

module Make(O:LexUtils.Config) = struct
open Lexing
open LexMisc
open ScopeTreeParser
module LU = LexUtils.Make(O)
}

let digit = [ '0'-'9' ]
let hexadigit = [ '0'-'9' 'a'-'f' 'A'-'F']
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_' | '/' | '.' | '-')*
let decimal = '-' ? digit+
let hexadecimal = ("0x"|"0X") hexadigit+
let num = decimal | hexadecimal

rule token = parse
| [' ''\t'] { token lexbuf }
| eof { EOF }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| 'P' (decimal as x)
    { PROC (int_of_string x) }
| ':' { COLON }
| '('  { LPAR }
| ')' { RPAR }
| "scopeTree" { SCOPETREE }
| "global"  { GLOBAL }
| "shared"|"local" { SHARED }
| "kernel" { KERNEL }
| "device" {DEVICE }
| "cta" | "block" | "work_group" { CTA }
| "warp" | "sub_group" { WARP }
| "thread" { THREAD }
| ',' { COMMA }
| name as name { NAME name }

{
 let token lexbuf =
   let tok = token lexbuf in
   if O.debug then begin
     Printf.eprintf
       "%a: Lexed '%s'\n"
       Pos.pp_pos2
       (lexeme_start_p lexbuf,lexeme_end_p lexbuf)
       (lexeme lexbuf) ;
     Printf.eprintf
       "LOC=%a\n" Pos.debug_pos (lexeme_start_p lexbuf)
   end ;
   tok
end
}
