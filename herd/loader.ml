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

(************************************************)
(* "load" program in memory, somehow abstracted *)
(************************************************)
module type S = sig
  type nice_prog
  type program
  type start_points

  val load : nice_prog -> program * start_points
end

module Make(A:Arch.S) : S
with type nice_prog = A.nice_prog
and type program = A.program
and type start_points = A.start_points =

  struct

    type nice_prog = A.nice_prog
    type program = A.program
    type start_points = A.start_points
    module V = A.V

    let rec load_code addr mem code = match code with
    | [] -> mem,[],addr
    | ins::code ->
	load_ins addr mem ins code

    and load_ins addr mem ins code = match ins with
    | A.Nop -> load_code addr mem code 
    | A.Instruction ins ->
      let mem,start,lastaddr = load_code (addr+4) mem code in
      let start' = A.Code_ins (SymbConstant.intToV addr,ins) :: start in
      mem, start', lastaddr
    | A.Label (lbl,ins) ->
	let mem,start,lastaddr = load_ins addr mem ins code in
	if A.LabelMap.mem lbl mem then
	  Warn.user_error
	    "Label %s occurs more that once" lbl ;
	A.LabelMap.add lbl start mem,start,lastaddr
    | A.Macro (_,_) -> assert false
    | A.Choice (ins,p1,p2) -> 
      let start_addr1 = addr+4 in
      let mem,start1,last_addr1 = load_code start_addr1 mem p1 in
      let start_addr2 = last_addr1+4 in
      let mem,start2,last_addr2 = load_code start_addr2 mem p2 in
      let start_addr3 = last_addr2 + 4 in
      let mem,start,last_addr = load_code start_addr3 mem code in
      let start' = [A.Code_ins (SymbConstant.intToV addr, ins (* not right yet *) )] @ 
                   start1 @ start2 @ start in
      mem, start', last_addr
    | A.Loop (ins, p) ->
      let start_addr1 = addr+4 in
      let mem,start1,last_addr = load_code start_addr1 mem p in
      let start_addr2 = last_addr + 4 in
      let mem,start,last_addr = load_code start_addr2 mem code in
      let start' = [A.Code_ins (SymbConstant.intToV addr, ins (* not right yet *) )] @ start1 @ start in
      mem, start', last_addr

    let rec load = function
    | [] -> (A.LabelMap.empty, [])
    | (proc,code)::prog ->
	let addr = 1000 * (proc+1) in
	let mem,starts = load prog in
	let mem,start,_ = load_code addr mem code in
	(mem, (proc,start) :: starts)

end
