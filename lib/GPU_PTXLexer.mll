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
open GPU_PTXParser
module GPU_PTX = GPU_PTXBase
module LU = LexUtils.Make(O)
}


let digit =  [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_' | '/' | '-' )*
let num = "0x"?digit+

rule token = parse
| [' ''\t''\r'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| '-' ? num as x { NUM (int_of_string x) }
| 'P' (num as x)
    { PROC (int_of_string x) }
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| ':' { COLON }
| '(' { LPAR }
| ')' { RPAR }
| ']' { RBRAC }
| '[' { LBRAC }
| '{' { LBRACE }
| '}' { RBRACE }
| "membar" { MEMBAR }
| ".cta" {CTA (GPU_PTX.CTA)}
| ".gl"  {GL (GPU_PTX.GL)}
| ".sys" {SYS (GPU_PTX.SYS)}
| "ld" {LD}
| "st" {ST}
| "mov" {MOV}
| "add" {ADD}
| "and" {AND}
| "cvt" {CVT}
| ".ca"  {CA (GPU_PTX.CA)}
| ".cg"  {CG (GPU_PTX.CG)}
| ".cv"  {CV (GPU_PTX.CV)}
| ".wb"  {WB (GPU_PTX.WB)}
| ".wt"  {WT (GPU_PTX.WT)}
| ".s32" {S32 (GPU_PTX.S32)}
| ".b64" {B64 (GPU_PTX.B64)}
| ".b32" {B32 (GPU_PTX.B32)}
| ".u64" {U64 (GPU_PTX.U64)}
| ".u32" {U32 (GPU_PTX.U32)}
| ".volatile" {VOL}
| ".shared" {SH (GPU_PTX.Shared)}
| ".global" {GLOB (GPU_PTX.Global)}
| name as x
  { match GPU_PTX.parse_reg x with
  | Some r -> ARCH_REG r
  | None -> NAME x }
| eof { EOF }
| ""  { error "GPU_PTX lexer" lexbuf }

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

