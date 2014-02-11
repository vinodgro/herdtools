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
  | Acq -> "mo_acquire"
  | Rel -> "mo_release"
  | Acq_Rel -> "mo_acq_rel"
  | SC -> "mo_seq_cst"
  | Rlx -> "mo_relaxed"
  | NA -> "non_atomic"
  | Con -> "mo_consume"

(****************)
(* Barriers     *)
(****************)

type barrier =
| Fence of mem_order
    
let all_kinds_of_barriers =  [ ]
  
let pp_barrier b = 
  match b with
  | Fence o -> sprintf "fence(%s)" (pp_mem_order o)

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

type loc = SymbConstant.v

type store_op = SymbConstant.v


type instruction = 
| Pstore  of loc*store_op*mem_order 
| Pload   of loc*reg*mem_order
| Pincr   of loc*mem_order
| Prmw    of loc*store_op*store_op*mem_order*mem_order
| Plock   of loc
| Punlock of loc
| Pfence  of mem_order

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
    | NA -> sprintf("%s = %s") (pp_addr loc) (pp_sop sop)
    | _ -> sprintf("%s.store(%s,%s)") (pp_addr loc) (pp_sop sop) (pp_mem_order mo))
  | Pload(loc,reg,mo) ->
    (match mo with 
    | NA -> sprintf("%s = %s") (pp_reg reg) (pp_addr loc)
    | _ -> sprintf("%s = %s.load(%s)") (pp_reg reg) (pp_addr loc) (pp_mem_order mo))
  | Pincr(loc,mo) ->
    sprintf "%s.incr(%s)" (pp_addr loc) (pp_mem_order mo)
  | Prmw(loc,sop1,sop2,mo_success,mo_failure) ->
    sprintf("RMW(%s,%s,%s,%s,%s)") (pp_addr loc) (pp_sop sop1) (pp_sop sop2) (pp_mem_order mo_success) (pp_mem_order mo_failure)
  | Plock(loc) ->
    sprintf("Lock(%s)") (pp_addr loc)
  | Punlock(loc) ->
    sprintf("Unlock(%s)") (pp_addr loc)
  | Pfence mo ->  sprintf("fence(%s)") (pp_mem_order mo)
   
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

let pp_instruction _m _ins = Warn.fatal "C++11 pp_instruction has not been implemented"

let get_next _ins = Warn.fatal "C++11 get_next not implemented"

let allowed_for_symb = []

type atrb = mem_order
let pp_atrb o = match o with
  | Acq -> "acq"
  | Rel -> "rel"
  | Acq_Rel -> "acq_rel"
  | SC -> "sc"
  | Rlx -> "rlx"
  | NA -> "na"
  | Con -> "con"
    
