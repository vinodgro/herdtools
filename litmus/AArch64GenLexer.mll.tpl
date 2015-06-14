(**************************************************************************)
(*                                  DIY                                   *)
(*                                                                        *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.          *)
(* Shaked Flur, Susmit Sarkar, Peter Sewell, University of Cambridge, UK. *)
(*                                                                        *)
(*  Copyright 2015 Institut National de Recherche en Informatique et en   *)
(*  Automatique and the authors. All rights reserved.                     *)
(*  This file is distributed  under the terms of the Lesser GNU General   *)
(*  Public License.                                                       *)
(**************************************************************************)

{
module Make(O:LexUtils.Config) = struct
open Lexing
open LexMisc
open AArch64GenParser
module AArch64 = AArch64GenBase
module LU = LexUtils.Make(O)

let rec big_num_of_hex_string x =
  let x_length = String.length x in
  if x_length <= 7 then
    let int_value = int_of_string ("0x" ^ x) in
    Nat_big_num.of_int int_value
  else
    let ls_chunk = String.sub x (x_length - 7) 7 in
    let ls_value = int_of_string ("0x" ^ ls_chunk) in
    let ls_value = Nat_big_num.of_int ls_value in
    let ms_value = big_num_of_hex_string (String.sub x 0 (x_length - 7)) in
    let ms_value = Nat_big_num.shift_left ms_value (7 * 4) in
    Nat_big_num.add ms_value ls_value


let instruction_table = Hashtbl.create 300
let () =
  List.iter (fun (kwd, tok) -> Hashtbl.add instruction_table kwd tok)
  [
  (* #include "./src_aarch64_hgen/lexer.hgen" *)
  ]
}
let digit = [ '0'-'9' ]
let hex_digit = (digit|[ 'a'-'f' 'A'-'F' ])
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_' | '/' | '.' | '-')*
let num = digit+
let hex_num = hex_digit+

rule token = parse
| [' ''\t'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { LU.skip_comment lexbuf ; token lexbuf }
| '-' ? num as x { try NUM (int_of_string x) with
                   | _ -> BIG_NUM (Nat_big_num.of_string x)}
| "#0x" (hex_num as x) {try NUM (int_of_string ("0x" ^ x)) with
                    | _ -> BIG_NUM (big_num_of_hex_string x)}
                    (*| _ -> NUM 0}*)
| '#' ('-' ? num as x) { try NUM (int_of_string x) with
                         | _ -> BIG_NUM (Nat_big_num.of_string x)}
| 'P' (num as x)
    { PROC (int_of_string x) }
| "X%" (name as name) { SYMB_XREG (X (Symbolic_reg name)) }
| "W%" (name as name) { SYMB_WREG (W (Symbolic_reg name)) }
| ';' { SEMI }
| ',' { COMMA }
| '|' { PIPE }
| '[' { LBRK }
| ']' { RBRK }
| ':' { COLON }
| '!' { EXCL }
| name as x
  {
    try
    (* Generated instructions *)
    Hashtbl.find instruction_table x
    with Not_found ->
      match AArch64.parse_xreg x with
      | Some Ireg r -> ARCH_XREG (X (Ireg r))
      | Some ZR ->     ARCH_XREG (X ZR)
      | Some SP ->     ARCH_XREG (X SP)
      | None -> begin
          match AArch64.parse_wreg x with
          | Some Ireg r -> ARCH_WREG (W (Ireg r))
          | Some ZR ->     ARCH_WREG (W ZR)
          | Some SP ->     ARCH_WREG (W SP)
          | None ->        NAME x
      end
  }
| eof { EOF }
| ""  { error "AArch64 lexer" lexbuf }




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

