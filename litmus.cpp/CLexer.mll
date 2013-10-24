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
| ' ' { main lexbuf }
| "int" { CParser.INT }
| name as x { CParser.NAME x }

and get_body i buf = parse
| '{' { Buffer.add_string buf (Lexing.lexeme lexbuf); get_body (succ i) buf lexbuf }
| '}'
    { if i > 0 then begin
       Buffer.add_string buf (Lexing.lexeme lexbuf);
       get_body (pred i) buf lexbuf
     end
    }
| eof { failwith "eof in get_body" }
| _ { Buffer.add_string buf (Lexing.lexeme lexbuf); get_body i buf lexbuf }
