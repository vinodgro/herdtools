(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

{
open Printf

let fconcat d1 d2 = match d1 with
| "." -> d2
| _ ->
    if Filename.is_relative d2 then
      Filename.concat d1 d2
    else
      d2

let do_include lex fname =
  Misc.input_protect 
    (fun chan -> lex (Lexing.from_channel chan))
    fname

}

let space = [' ''\t']
let non_space = [^' ''\t''\n']

rule main dir get acc = parse
| "#include" space+ (non_space+ as fname) '\n'
    {
     let dir0 = Filename.dirname fname in
     let d = fconcat dir dir0 in
     let n = fconcat dir fname in
     let acc = do_include (main d get acc) n in
     main dir get acc lexbuf
   }
| '#' [^'\n']* '\n'
   { main dir get acc lexbuf }
  
| (non_space+ as name) space+
  (non_space+ as key) space+
  ("" | (non_space [^'\n']+) as v)
  '\n'
{ main dir get (get acc name key v) lexbuf }

| eof { acc }

| [^'\n']* as lxm '\n' { failwith (sprintf "LexHint: %s" lxm) }

{
 let read fname get acc =
   let dir = Filename.dirname fname in
   do_include (main dir get acc) fname
}
