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

let arch = Archs.CPP11

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

type expression =
| Econstant of SymbConstant.v
| Eregister of reg
| Eassign of reg * expression
| Eeq of expression * expression
| Estore  of loc * expression * mem_order 
| Eload   of loc * mem_order
| Ecas    of loc * loc * expression * mem_order * mem_order * bool
| Elock   of loc
| Eunlock of loc
| Efence  of mem_order
| Ecomma of expression * expression
| Eparen of expression

type instruction = 
| Pif     of expression * instruction * instruction
| Pwhile  of expression * instruction
| Pblock  of instruction list
| Pexpr   of expression

include Pseudo.Make
    (struct
      type ins = instruction
      type reg_arg = reg
      let get_naccesses = function 
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

let rec dump_expression e = match e with
  | Estore(loc,e,mo) ->
    (match mo with 
    | NA -> sprintf("*%s = %s") 
		   (pp_addr loc) (dump_expression e)
    | SC -> sprintf("atomic_store(%s,%s)") 
		  (pp_addr loc) (dump_expression e)
    | _ -> sprintf("atomic_store_explicit(%s,%s,%s)") 
		  (pp_addr loc) (dump_expression e) (pp_mem_order mo))
  | Eload(loc,mo) ->
    (match mo with 
    | NA -> sprintf("*%s") 
		   (pp_addr loc)
    | SC -> sprintf("atomic_load(%s)") 
		  (pp_addr loc)
    | _ -> sprintf("atomic_load_explicit(%s,%s)") 
		  (pp_addr loc) (pp_mem_order mo))
  | Ecas(obj,exp,des,mo_success,mo_failure,strong) ->
    sprintf("%sCAS(%s,%s,%s,%s,%s)") 
      (if strong then "S" else "W")
      (pp_addr obj) (pp_addr exp) (dump_expression des) 
      (pp_mem_order mo_success) (pp_mem_order mo_failure)     
  | Elock(loc) ->
    sprintf("lock(%s)") (pp_addr loc)
  | Eunlock(loc) ->
    sprintf("unlock(%s)") (pp_addr loc)
  | Efence mo -> sprintf("fence(%s)") (pp_mem_order mo)
  | Econstant i -> pp_sop i
  | Eregister reg -> pp_reg reg
  | Eassign(reg,e) -> sprintf "%s = %s" (pp_reg reg) (dump_expression e)
  | Eeq (e1,e2) -> sprintf "%s == %s" (dump_expression e1) (dump_expression e2)
  | Ecomma (e1,e2) -> sprintf "%s, %s" (dump_expression e1) (dump_expression e2)
  | Eparen e -> sprintf "(%s)" (dump_expression e)

let rec dump_instruction i = match i with
  | Pif(e,i1,i2) -> 
    sprintf ("if(%s)%s else %s") (dump_expression e)  
      (dump_instruction i1) (dump_instruction i2)
  | Pwhile(e,i) -> 
    sprintf ("while(%s)%s") (dump_expression e) 
      (dump_instruction i)
  | Pblock insts ->
    sprintf ("{%s}") (List.fold_left (fun z i -> z ^ dump_instruction i) "" insts)
  | Pexpr e -> sprintf "%s;" (dump_expression e)
   
(* We don't have symbolic registers. This should be enough *)
let fold_regs (f_reg,_f_sreg) = 
  let fold_reg reg (y_reg, y_sreg) = match reg with
    | GPRreg _ -> f_reg reg y_reg, y_sreg
    | _ -> y_reg, y_sreg in 
  
  fun (_y_reg,_y_sreg as c) ins -> match ins with
  | _ -> c

let map_regs f_reg _f_symb = 
  let map_reg reg = match reg with
    | GPRreg _ -> f_reg reg
    | _ -> reg in

  fun ins -> match ins with
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
