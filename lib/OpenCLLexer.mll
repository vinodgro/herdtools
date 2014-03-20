
{
module Make(O:LexUtils.Config) = struct
open Lexing
open LexMisc
open OpenCLParser
module OpenCL = OpenCLBase
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
| "acq" {MEMORDER (OpenCLBase.Acq)}
| "rel" {MEMORDER (OpenCLBase.Rel)}
| "acq_rel" {MEMORDER (OpenCLBase.Acq_Rel)}
| "sc"      {MEMORDER (OpenCLBase.SC)}
| "rlx" {MEMORDER (OpenCLBase.Rlx)}
| "wi"        {MEMSCOPE (OpenCLBase.S_workitem)}
| "sg"        {MEMSCOPE (OpenCLBase.S_subgroup)}
| "wg"       {MEMSCOPE (OpenCLBase.S_workgroup)}
| "dev"          {MEMSCOPE (OpenCLBase.S_device)}
| "all_dev" {MEMSCOPE (OpenCLBase.S_all_svn_devices)}
| "global" { MEMREGION (OpenCLBase.GlobalMem) }
| "local" { MEMREGION (OpenCLBase.LocalMem) }
| "fence" { FENCE }
| "load"  { LD }
| "store"    { ST }
| "scopeTree" { SCOPETREE }
| "kernel" { KERNEL }
| "device" {DEVICE }
| "cta" | "block" | "work_group" { CTA }
| "warp" | "sub_group" { WARP }
| "thread" { THREAD }
| name as x
  { match OpenCL.parse_reg x with
  | Some r -> ARCH_REG r
  | None -> NAME x }
| eof { EOF }
| ""  { error "OpenCL lexer" lexbuf }

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

