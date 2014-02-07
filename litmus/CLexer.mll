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

let num = (['0'-'9']+)
let alpha = ['a'-'z' 'A'-'Z' '_']
let name = (alpha+ ['0'-'9']* alpha*)

rule main = parse
| [' ''\t']+ { main lexbuf }
| '\n' { Lexing.new_line lexbuf ; main lexbuf ; }
| 'P' (num as x) { CParser.PROC (int_of_string x) }
| '(' { CParser.LPAREN }
| ')' { CParser.RPAREN }
| ',' { CParser.COMMA }
| '{'
    { let buf = Buffer.create 4096 in
      get_body 0 buf lexbuf;
      CParser.BODY (Buffer.contents buf)
    }
| '*' { CParser.STAR }
| "volatile" { CParser.VOLATILE }
| name as x { CParser.NAME x }
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
