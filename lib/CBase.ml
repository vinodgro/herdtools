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

(** Define registers, barriers, and instructions for CPP11 *) 

open Printf
open Constant

(*Initial CPP11 file*)

let arch = Archs.C

(*************)
(* Registers *)
(*************)

type gpr_reg =
  | GPR0 | GPR1 | GPR2 | GPR3
  | GPR4 | GPR5 | GPR6 | GPR7
  | GPR8 | GPR9 

let gpr_regs =
  [
   GPR0,"r0";  GPR1,"r1";
   GPR2,"r2";  GPR3,"r3";
   GPR4,"r4";  GPR5,"r5";
   GPR6,"r6";  GPR7,"r7";
   GPR8,"r8";  GPR9,"r9";
  ]

type reg =
  | GPRreg of gpr_reg (* integer registers *)
  | PC (* program counter *)

let pp_reg r =
  try List.assoc r gpr_regs with
  | Not_found -> assert false

let pp_reg r =
  match r with
  | GPRreg(ir) -> pp_reg ir
  | PC -> "pc"

let reg_compare = Pervasives.compare

let parse_list =
  List.map (fun (r,s) -> s, GPRreg r) gpr_regs 

let parse_reg s =
  let s = String.lowercase s in
  try Some (List.assoc s parse_list)
  with Not_found -> None
    
let pc = PC

(*******************)
(* Mem order stuff *)
(*******************)

type mem_order = 
| Acq
| Rel
| Acq_Rel
| SC
| Rlx
| NA
| Con

let pp_mem_order o = 
  match o with 
  | Acq -> "memory_order_acquire"
  | Rel -> "memory_order_release"
  | Acq_Rel -> "memory_order_acq_rel"
  | SC -> "memory_order_seq_cst"
  | Rlx -> "memory_order_relaxed"
  | NA -> "non_atomic"
  | Con -> "memory_order_consume"

(****************)
(* Barriers     *)
(****************)

type barrier = unit
    
let all_kinds_of_barriers =  [ ]
  
let pp_barrier _ = assert false

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

type loc = SymbConstant.v

type store_op = SymbConstant.v


type instruction = 
| Pstore  of loc * store_op * mem_order 
| Pload   of loc * reg * mem_order
| Pcas    of loc * loc * store_op * mem_order * mem_order * bool
| Plock   of loc
| Punlock of loc
| Pfence  of mem_order
| Pexpr_const of store_op
| Pexpr_reg of reg
| Pexpr_eqeq of reg * store_op
| Passign of store_op * reg

include Pseudo.Make
    (struct
      type ins = instruction
      type reg_arg = reg
      let get_naccesses = function 
	| Pstore _ | Pload _ -> 1
	| _ -> 0 
       (* JPW: maybe locks/unlocks/RMWs should return something other
          than 0, but I'm not sure whether this function is
          used, so I'll leave them at 0 *)

      (* I don't think we have labels yet... *)
      let fold_labels k _ = function 
	| _ -> k

      let map_labels _ = function 
	| ins -> ins

     end)
    
let pp_addr = function
  | Symbolic s -> s
  | _ -> "concrete addresses not supported in C++11"

let pp_sop = function
  | Concrete i -> (sprintf "%d" i)
  | _ -> "only concrete store ops supported at this time in C++11"

let dump_instruction i = match i with
  | Pstore(loc,sop,mo) ->
    (match mo with 
    | NA -> sprintf("%s = %s") 
		   (pp_addr loc) (pp_sop sop)
    | _ -> sprintf("atomic_store_explicit(%s,%s,%s)") 
		  (pp_addr loc) (pp_sop sop) (pp_mem_order mo))
  | Pload(loc,reg,mo) ->
    (match mo with 
    | NA -> sprintf("%s = %s") 
		   (pp_reg reg) (pp_addr loc)
    | _ -> sprintf("%s = atomic_load_explicit(%s,%s)") 
		  (pp_reg reg) (pp_addr loc) (pp_mem_order mo))
  | Pcas(obj,exp,des,mo_success,mo_failure,strong) ->
    sprintf("%sCAS(%s,%s,%s,%s,%s)") 
      (if strong then "S" else "W")
      (pp_addr obj) (pp_addr exp) (pp_sop des) 
      (pp_mem_order mo_success) (pp_mem_order mo_failure)     
  | Plock(loc) ->
    sprintf("Lock(%s)") (pp_addr loc)
  | Punlock(loc) ->
    sprintf("Unlock(%s)") (pp_addr loc)
  | Pfence mo ->  sprintf("fence(%s)") (pp_mem_order mo)
  | Pexpr_const sop -> pp_sop sop
  | Pexpr_reg reg -> pp_reg reg
  | Pexpr_eqeq (reg,sop) -> sprintf("%s==%s") (pp_reg reg) (pp_sop sop)
  | Passign (sop,reg) -> sprintf("%s = %s") (pp_reg reg) (pp_sop sop)

(* We don't have symbolic registers. This should be enough *)
let fold_regs (f_reg,_f_sreg) = 
  let fold_reg reg (y_reg, y_sreg) = match reg with
    | GPRreg _ -> f_reg reg y_reg, y_sreg
    | _ -> y_reg, y_sreg in 
  
  fun (_y_reg,_y_sreg as c) ins -> match ins with
  | Pload(_,reg,_) -> fold_reg (reg) c
  | _ -> c

let map_regs f_reg _f_symb = 
  let map_reg reg = match reg with
    | GPRreg _ -> f_reg reg
    | _ -> reg in

  fun ins -> match ins with
  | Pload(loc,reg,mo) -> Pload(loc, (map_reg reg),mo)
  | _ -> ins



(* This is what ARM and X86 do *)
let norm_ins ins = ins

(*unimplemented so far*)
let get_reg_list _ins = ([], [])

let get_macro _name = Warn.fatal "C++11 get_macro has not been implemented"
let is_data _reg _ins = Warn.fatal "C++11 is_data has not been implemented"

let map_addrs _f _ins = Warn.fatal "C++11 map_addrs has not been implemented"

(*This is how PPC and ARM did it...*)
let fold_addrs _f c _ins = c

let pp_instruction _m _ins = dump_instruction _ins

let get_next _ins = Warn.fatal "C++11 get_next not implemented"

let allowed_for_symb = []

include LocationKindMap
