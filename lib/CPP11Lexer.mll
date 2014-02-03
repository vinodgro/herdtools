
{
module Make(O:LexUtils.Config) = struct
open Lexing
open LexMisc
open CPP11Parser
module CPP11 = CPP11Base
module LU = LexUtils.Make(O)
}


let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = (alpha | '_') (alpha|digit|'_' | '/' | '-')*
let num = digit+

rule token = parse
| [' ''\t'] { token lexbuf }
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
| '=' {EQ}
| '.' {DOT}
| "mo_acquire" {MEMORDER (CPP11Base.Acq)}
| "mo_release" {MEMORDER (CPP11Base.Rel)}
| "mo_acq_rel" {MEMORDER (CPP11Base.Acq_Rel)}
| "mo_seq_cst"      {MEMORDER (CPP11Base.SC)}
| "mo_relaxed" {MEMORDER (CPP11Base.Rlx)}
| "mo_consume" {MEMORDER (CPP11Base.Con)}
| "fence" { FENCE }
| "load"  { LD }
| "store"    { ST }
| name as x
  { match CPP11.parse_reg x with
  | Some r -> ARCH_REG r
  | None -> NAME x }
| eof { EOF }
| ""  { error "CPP11 lexer" lexbuf }

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

