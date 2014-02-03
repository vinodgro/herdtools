open Printf
open Constant

(*Initial OpenCL file*)

let arch = Archs.OpenCL

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

let pp_mem_order o = 
  match o with 
  | Acq -> "acq"
  | Rel -> "rel"
  | Acq_Rel -> "acq_rel"
  | SC -> "sc"
  | Rlx -> "rlx"
  | NA -> "na"

(*********************)
(* Scope annotations *)
(*********************)

type mem_scope = 
  | S_workitem
  | S_subgroup
  | S_workgroup
  | S_device
  | S_all_svn_devices

let pp_mem_scope s = 
  match s with 
  | S_workitem -> "wi"
  | S_subgroup -> "sg"
  | S_workgroup -> "wg"
  | S_device -> "dev"
  | S_all_svn_devices -> "all_dev"

(****************)
(* Barriers     *)
(****************)

(* Todo: distinguish local and global fences. *)
type barrier =
| Fence of mem_order * mem_scope
    
let all_kinds_of_barriers =  [ ]
  
let pp_barrier b = 
  match b with
  | Fence (o,s) -> 
    sprintf "fence(%s,%s)" (pp_mem_order o) (pp_mem_scope s)

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

type loc = SymbConstant.v

type store_op = SymbConstant.v


type instruction = 
| Pstore of loc * store_op * mem_order * mem_scope
| Pload  of loc * reg * mem_order * mem_scope
| Pfence of mem_order * mem_scope

include Pseudo.Make
    (struct
      type ins = instruction
      type reg_arg = reg
      let get_naccesses = function 
	| Pstore _
	| Pload _ -> 1
	| _ -> 0

      (* I don't think we have labels yet... *)
      let fold_labels k _ = function 
	| _ -> k

      let map_labels _ = function 
	| ins -> ins

     end)
    
let pp_addr = function
  | Symbolic s -> s
  | _ -> "concrete addresses not supported in OpenCL"

let pp_sop = function
  | Concrete i -> (sprintf "%d" i)
  | _ -> "only concrete store ops supported at this time in OpenCL"

let dump_instruction i = match i with
  | Pstore(loc,sop,mo,scope) ->
    (match mo with 
    | NA -> sprintf("%s = %s") (pp_addr loc) (pp_sop sop)
    | _ -> sprintf("%s.store(%s,%s,%s)") (pp_addr loc) (pp_sop sop) (pp_mem_order mo) (pp_mem_scope scope))
  | Pload(loc,reg,mo,scope) ->
    (match mo with 
    | NA -> sprintf("%s = %s") (pp_reg reg) (pp_addr loc)
    | _ -> sprintf("%s = %s.load(%s,%s)") (pp_reg reg) (pp_addr loc) (pp_mem_order mo) (pp_mem_scope scope))
  | Pfence(mo,scope) ->  sprintf("fence(%s,%s)") (pp_mem_order mo) (pp_mem_scope scope)
   
(* We don't have symbolic registers. This should be enough *)
let fold_regs (f_reg,f_sreg) = 
  let fold_reg reg (y_reg, y_sreg) = match reg with
    | GPRreg _ -> f_reg reg y_reg, y_sreg
    | _ -> y_reg, y_sreg in 
  
  fun (y_reg,y_sreg as c) ins -> match ins with
  | Pload(_,reg,_,_) -> fold_reg (reg) c
  | _ -> c

let map_regs f_reg f_symb = 
  let map_reg reg = match reg with
    | GPRreg _ -> f_reg reg
    | _ -> reg in

  fun ins -> match ins with
  | Pload(loc,reg,mo,scope) -> Pload(loc, (map_reg reg),mo,scope)
  | _ -> ins



(* This is what ARM and X86 do *)
let norm_ins ins = ins

(*unimplemented so far*)
let get_reg_list ins = ([], [])

let get_macro name = Warn.fatal "OpenCL get_macro has not been implemented"
let is_data reg ins = Warn.fatal "OpenCL is_data has not been implemented"

let map_addrs _f ins = Warn.fatal "OpenCL map_addrs has not been implemented"
let fold_addrs _f c _ins = Warn.fatal "OpenCL fold_addrs has not been implemented"

let pp_instruction _m ins = Warn.fatal "OpenCL pp_instruction has not been implemented"

let get_next ins = Warn.fatal "OpenCL get_next not implemented"

let allowed_for_symb = []

type atrb = 
| Mem_order of mem_order
| Mem_scope of mem_scope
    
let pp_atrb o = match o with 
  | Mem_order m -> pp_mem_order m
  | Mem_scope m -> pp_mem_scope m
