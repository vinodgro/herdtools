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

(** Define registers, barriers, and instructions for OpenCL *) 

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
type barrier = unit
    
let all_kinds_of_barriers =  [ ]
  
let pp_barrier _b = assert false

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

type loc = SymbConstant.v

type store_op = SymbConstant.v

include MemSpaceMap

type instruction = 
| Pstore of loc * store_op * mem_order * mem_scope
| Pload  of loc * reg * mem_order * mem_scope
| Pfence of gpu_memory_space * mem_order * mem_scope

include MachPseudo.Make
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

module Loader = struct

    type nice_prog = nice_prog
    type program = program
    type start_points = start_points
    module V = V

    let rec load_code addr mem code = match code with
    | [] -> mem,[]
    | ins::code ->
	load_ins addr mem ins code

    and load_ins addr mem ins code = match ins with
    | Nop -> load_code addr mem code 
    | Instruction ins ->
	let mem,start = load_code (addr+4) mem code in
	mem,(SymbConstant.intToV addr,ins)::start
    | Label (lbl,ins) ->
	let mem,start = load_ins addr mem ins code in
	if LabelMap.mem lbl mem then
	  Warn.user_error
	    "Label %s occurs more that once" lbl ;
	LabelMap.add lbl start mem,start
    | Macro (_,_) -> assert false 

    let rec load = function
    | [] -> LabelMap.empty,[]
    | (proc,code)::prog ->
	let addr = 1000 * (proc+1) in
	let mem,starts = load prog in
	let mem,start = load_code addr mem code in
	mem,(proc,start)::starts 
end

module SymbReg = struct 
   type v = V.v
   type location = location
   type code = (int * pseudo list) list
   type ('loc,'v) t = ('loc,'v, code) MiscParser.r3
      
(******************************************************)
(* All those to substitute symbolic regs by real ones *)
(******************************************************)

  let get_reg name = match parse_reg name with
  | Some r -> r
  | None -> Warn.user_error "%s is not a register" name

  let finish_reg = get_reg

  let finish_location f_reg loc = match loc with
  | Location_global m -> maybev_to_location m
  | Location_reg (i,r) -> Location_reg (i,finish_reg r)
  | Location_sreg reg  ->
      let p,r = f_reg reg in Location_reg (p,r)

  let finish_state_atom f_reg (loc,v) =
    finish_location f_reg loc, V.maybevToV v

  let finish_state f_reg = List.map (finish_state_atom f_reg)

  let finish_locations f_reg =
    List.map (fun (loc,t) -> finish_location f_reg loc,t)

  let finish_atom f_reg a =
    let open ConstrGen in
    match a with
    | LV (loc,v) -> LV (finish_location f_reg loc, V.maybevToV v)
    | LL (l1,l2) -> LL (finish_location f_reg l1,finish_location f_reg l2)

  let finish_constr f_reg = ConstrGen.map_constr (finish_atom f_reg)

  let finish_pseudo f_reg =
    pseudo_map (map_regs (fun r -> r) f_reg)

  let finish_code f_reg = List.map (finish_pseudo f_reg)


(**********************************)	
(* All those to collect registers *)
(**********************************)	

  module ProcRegSet = 
    MySet.Make
      (struct
	type t = int * reg
	let compare (p1,r1) (p2,r2) = match compare p1 p2 with
	| 0 -> reg_compare r1 r2
	| r -> r
      end)

  module RegSet =
    MySet.Make
      (struct
	type t = reg
	let compare = reg_compare
      end)


  let collect_pseudo f =
    pseudo_fold
      (fun k ins -> fold_regs f k ins)

  let collect_prog =
    List.map
      (List.fold_left
	 (collect_pseudo  (RegSet.add,StringSet.add))
	 (RegSet.empty,StringSet.empty))
      

  let collect_location loc (regs,symbs as c) = match loc with
  | Location_reg (p,r) ->
      ProcRegSet.add (p,get_reg r) regs,symbs
  | Location_sreg reg ->
      regs,StringSet.add reg symbs
  | Location_global _ -> c

  let collect_state_atom (loc,(_:maybev)) = collect_location loc

  let collect_state = List.fold_right collect_state_atom

  let collect_atom a =
    let open ConstrGen in
    match a with
    | LV (loc,_) -> collect_location loc
    | LL (loc1,loc2) ->
        fun c -> collect_location loc1 (collect_location loc2 c)

  let collect_constr = ConstrGen.fold_constr collect_atom

  let collect_locs = List.fold_right (fun (loc,_) -> collect_location loc)

(*********************************************)
(* Here we go: collect, allocate, substitute *)
(*********************************************)
  open MiscParser

(*
  let pp_reg_set chan rs =
    RegSet.pp chan "," (fun chan r -> fprintf chan "%s" (pp_reg r)) rs
  and pp_string_set chan s =
      StringSet.pp chan "," (fun chan r -> fprintf chan "%s" r) s
*)
  let allocate_regs test =
    let initial = test.init
    and prog = test.prog
    and final = test.condition
    and locs = test.locations in
    (* Collect all registers, either real or symbolic *)
    let regs,symbs =
      collect_constr final
        (collect_locs locs
	   (collect_state initial
	      (ProcRegSet.empty,StringSet.empty)))
    in

    let in_code = collect_prog (List.map snd prog) in
    (* Control register usage, ambiguity is possible,
       for unconstrained symbolic regs *)
    let (_,bad) =
      List.fold_left
	(fun (seen,bad) (_,symbs_p) ->
	  let symbs_p = StringSet.inter symbs_p symbs in
	  let bad =
	    StringSet.union
	      bad (StringSet.inter seen symbs_p) in
	  let seen = StringSet.union symbs_p seen in
	  seen,bad)
	(StringSet.empty,StringSet.empty)
	in_code in
    if not (StringSet.is_empty bad) then begin
      let msg =
	sprintf "ambiguous symbolic register(s): {%s}"
	  (String.concat ","
	     (StringSet.elements bad)) in
      Warn.user_error "%s" msg
    end ;
    (* Perform allocation of symbolic registers to real ones *)
    let envs =
      List.map2
	(fun (p,_) (regs_p,symbs_p) ->
	  let regs_cstr =
	    ProcRegSet.fold
	      (fun (q,reg) k ->
		if p=q then RegSet.add reg k else k)
	      regs RegSet.empty in
	  let free_regs =
	    RegSet.diff (RegSet.of_list allowed_for_symb)
	      (RegSet.union regs_p regs_cstr) in
	  let env,_ =
	    StringSet.fold
	      (fun name (env,free_regs) -> match free_regs with
	      | [] ->
		  Warn.fatal
		    "not enough registers for all those symbolic registers"
	      | next::free_regs ->
		  (name,next)::env,free_regs)
	      symbs_p ([],RegSet.elements free_regs) in
	  p,env)
	prog in_code in
    (* Replace symbolic registers *)
    let prog =
      List.map2
	(fun (proc,code) (_,env) ->
	  let replace name =
	    try List.assoc name env
	    with Not_found -> assert false in
	  proc,finish_code replace code)
	prog envs in
    
    let env =
      List.fold_left
	(fun k (p,env_p) ->
	  List.fold_left
	    (fun k (symb,reg) -> (symb,(p,reg))::k)
	    k env_p)
	[] envs in
    let replace name =
      try List.assoc name env
      with Not_found ->
	Warn.user_error
	  "symbolic register %%%s does not appear in code" name in
    { test with
      init = finish_state replace initial ;
      prog = prog;
      condition = finish_constr replace final;
      locations = finish_locations replace locs;
    }
end
    
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
  | Pfence(mr,mo,scope) ->  sprintf("fence(%s,%s,%s)") (pp_gpu_memory_space mr) (pp_mem_order mo) (pp_mem_scope scope)
   
(* We don't have symbolic registers. This should be enough *)
let fold_regs (f_reg,_f_sreg) = 
  let fold_reg reg (y_reg, y_sreg) = match reg with
    | GPRreg _ -> f_reg reg y_reg, y_sreg
    | _ -> y_reg, y_sreg in 
  
  fun (_y_reg,_y_sreg as c) ins -> match ins with
  | Pload(_,reg,_,_) -> fold_reg (reg) c
  | _ -> c

let map_regs f_reg _f_symb = 
  let map_reg reg = match reg with
    | GPRreg _ -> f_reg reg
    | _ -> reg in

  fun ins -> match ins with
  | Pload(loc,reg,mo,scope) -> Pload(loc, (map_reg reg),mo,scope)
  | _ -> ins



(* This is what ARM and X86 do *)
let norm_ins ins = ins

(*unimplemented so far*)
let get_reg_list _ins = ([], [])

let get_macro _name = Warn.fatal "OpenCL get_macro has not been implemented"
let is_data _reg _ins = Warn.fatal "OpenCL is_data has not been implemented"

let map_addrs _f _ins = Warn.fatal "OpenCL map_addrs has not been implemented"

(*This is how PPC and ARM did it...*)
let fold_addrs _f c _ins = c

let pp_instruction _m _ins = Warn.fatal "OpenCL pp_instruction has not been implemented"

let get_next _ins = Warn.fatal "OpenCL get_next not implemented"

let allowed_for_symb = []

include ScopeTree

