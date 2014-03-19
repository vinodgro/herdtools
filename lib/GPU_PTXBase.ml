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

(** Define registers, barriers, and instructions for PTX *) 

open Printf

(* Who am i ? *)
let arch = Archs.GPU_PTX

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

let parse_list =
  List.map (fun (r,s) -> s, GPRreg r) gpr_regs 
    
let parse_cop s =
  let s = String.lowercase s in
  try Some (List.assoc s parse_list)
  with Not_found -> None


let pc = PC

(****************)
(* Barriers     *)
(****************)

type bar_scope = 
| CTA_bar
| GL_bar
| SYS_bar

let pp_bar_scope s = match s with
  | CTA_bar -> "cta"
  | GL_bar -> "gl"
  | SYS_bar -> "sys"

type barrier =
 | Membar of bar_scope

let all_kinds_of_barriers =  [ Membar GL_bar ;]
(* Shouldn't the list above include Membar CTA and Membar SYS too? *)

let pp_barrier b = match b with
  | Membar s -> sprintf "membar.%s" (pp_bar_scope s)
  

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

type cache_op = 
  | CA
  | CG
  | CV
  | WB
  | WT
  | NCOP


type state_space = 
  | Shared
  | Global
  | NOMP

type op_type = 
  | S32
  | B64
  | B32
  | U64
  | U32

let pp_op_type o = match o with
  | S32 -> ".s32"
  | B64 -> ".b64"
  | B32 -> ".b32"
  | U64 -> ".u64"
  | U32 -> ".u32"

let pp_state_space m = match m with
  | Shared -> ".shared"
  | Global -> ".global"
  | NOMP -> ""

let pp_cache_op c = match c with
  | CA -> ".ca"
  | CG -> ".cg"
  | CV -> ".cv"
  | WB -> ".wb"
  | WT -> ".wt"
  | NCOP -> ""

type ins_op = 
| Reg of reg
| Im of int

let pp_ins_op op = match op with
  | Reg r -> (pp_reg r)
  | Im i -> sprintf "%d" i

let mk_b64_reg r = match r with
  | GPRreg(r) -> r
  | _ -> assert false

let mk_s32_reg r = match r with
  | GPRreg(r) -> r
  | _ -> assert false

type instruction = 
| Pld of reg*reg*state_space*cache_op*op_type
| Pst of reg*reg*state_space*cache_op*op_type
| Pldvol of reg*reg*state_space*op_type
| Pstvol of reg*reg*state_space*op_type
| Padd of reg*ins_op*ins_op*op_type
| Pand of reg*ins_op*ins_op*op_type
| Pmov of reg*ins_op*op_type
| Pcvt of reg*reg*op_type*op_type
| Pmembar of bar_scope

include Pseudo.Make
    (struct
      type ins = instruction
      type reg_arg = reg
      let get_naccesses = function 
	| Pld _
	| Pst _ 
	| Pldvol _ 
	| Pstvol _ -> 1
	| _ -> 0


      (* I don't think we have labels yet... *)
      let fold_labels k _f = function 
	| _ -> k

      let map_labels _f = function 
	| ins -> ins

     end)
    

let dump_instruction i = match i with
  | Pld(r1,r2,m,cop,t) -> sprintf "ld%s%s%s %s, [%s]" (pp_state_space m)
                                                      (pp_cache_op cop)
                                                      (pp_op_type t)
                                                      (pp_reg r1) (pp_reg r2)

  | Pst(r1,r2,m,cop,t) -> sprintf "st%s%s%s [%s], %s" (pp_state_space m)
                                                      (pp_cache_op cop)
                                                      (pp_op_type t)
                                                      (pp_reg r1) (pp_reg r2)

  | Pldvol(r1,r2,m,t) -> sprintf "ld.volatile%s%s %s, [%s]" (pp_state_space m)
                                                            (pp_op_type t)
                                                            (pp_reg r1) (pp_reg r2)

  | Pstvol(r1,r2,m,t) -> sprintf "st.volatile%s%s [%s], %s" (pp_state_space m)
                                                            (pp_op_type t)
                                                            (pp_reg r1) (pp_reg r2)


  | Pmov(r1,op,t) -> sprintf "mov%s %s, %s" (pp_op_type t)
                                            (pp_reg r1)
                                            (pp_ins_op op)

  | Padd(r1,op1,op2,t) -> sprintf "add%s %s, %s %s" (pp_op_type t) 
                                                     (pp_reg r1) 
                                                     (pp_ins_op op1) 
                                                     (pp_ins_op op2)

  | Pand(r1,op1,op2,t) -> sprintf "and%s %s, %s %s" (pp_op_type t) 
                                                     (pp_reg r1) 
                                                     (pp_ins_op op1) 
                                                     (pp_ins_op op2)

  | Pcvt(r1,r2,t1,t2) -> sprintf "cvt%s%s %s, %s"    (pp_op_type t1)
                                                       (pp_op_type t2)
                                                       (pp_reg r1) 
                                                       (pp_reg r2)

  | Pmembar s -> sprintf "membar.%s" (pp_bar_scope s)

(* Required by archBase.mli   *)

(* We don't have symbolic registers. This should be enough *)
let fold_regs (f_reg,_f_sreg) = 
  let fold_reg reg (y_reg,y_sreg) = match reg with
    | GPRreg _ -> f_reg reg y_reg,y_sreg
    | _ -> y_reg, y_sreg in
  

  fun (_y_reg,_y_sreg as c) ins -> match ins with
    (*two registers*)
  | Pld(r1,r2,_,_,_) | Pst(r1,r2,_,_,_) | Pcvt(r1,r2,_,_) | Pldvol(r1,r2,_,_) | Pstvol(r1,r2,_,_)
      -> fold_reg (r2 )(fold_reg (r1) c)

    (*one register or 2 registers for mov*)
  | Pmov (r1,op,_) ->
    begin match op with
    | Reg r2  -> fold_reg (r1 )(fold_reg (r2) c)
    | _          -> fold_reg (r1) c
    end

  | Padd(r1, op1, op2, _) ->
    begin match op1,op2 with
    | Reg r2, Reg r3 -> fold_reg (r1 )(fold_reg (r2) (fold_reg (r3) c))
    | _, Reg r2      -> fold_reg (r1 )(fold_reg (r2) c)
    | Reg r2, _      -> fold_reg (r1 )(fold_reg (r2) c)
    | _, _           -> fold_reg (r1) c
    end

  | Pand(r1, op1, op2, _) ->
    begin match op1,op2 with
    | Reg r2, Reg r3 -> fold_reg (r1 )(fold_reg (r2) (fold_reg (r3) c))
    | _, Reg r2      -> fold_reg (r1 )(fold_reg (r2) c)
    | Reg r2, _      -> fold_reg (r1 )(fold_reg (r2) c)
    | _, _           -> fold_reg (r1) c
    end

  (*zero registers*)
  | Pmembar _  -> c
    

let map_regs f_reg _f_symb = 
  let map_reg reg = match reg with
    | GPRreg _ -> f_reg reg
    | _ -> reg in

  let map2 ins r1 r2 = ins (map_reg r1,map_reg r2) in
  let map3 ins r1 r2 r3 =ins (map_reg r1,map_reg r2,map_reg r3) in

  fun ins -> match ins with
  (*Two registers*)
  | Pld(r1,r2,m,cop,t) ->
    map2 (fun (r1,r2) -> Pld(r1,r2,m,cop,t)) r1 r2
  | Pst(r1,r2,m,cop,t) -> 
    map2 (fun (r1,r2) -> Pst(r1,r2,m,cop,t)) r1 r2

  | Pldvol(r1,r2,m,t) ->
    map2 (fun (r1,r2) -> Pldvol(r1,r2,m,t)) r1 r2
  | Pstvol(r1,r2,m,t) -> 
    map2 (fun (r1,r2) -> Pstvol(r1,r2,m,t)) r1 r2

  | Pcvt(r1,r2,t1,t2) ->
    map2 (fun (r1,r2) -> Pcvt(r1,r2,t1,t2)) r1 r2

  (*mov instruction takes 1 or 2 registers*)
  | Pmov(r1,op,t) ->
    begin match op with
    | Reg r2  -> map2 (fun (r1,r2) -> Pmov(r1,Reg r2,t)) r1 r2
    | _       -> Pmov(map_reg r1,op,t)
    end

  | Padd (r1,op1,op2,t) ->
    begin match op1,op2 with
    | Reg r2, Reg r3 -> map3 (fun (r1,r2,r3) -> Padd(r1, Reg r2, Reg r3,t)) r1 r2 r3
    | Reg r2, _ ->
      map2 (fun (r1,r2) -> Padd(r1, Reg r2, op2,t)) r1 r2
    | _, Reg r2 ->
      map2 (fun (r1,r2) -> Padd(r1, op1, Reg r2,t)) r1 r2
    | _,_ -> Padd(map_reg r1, op1, op2, t)
    end    

  | Pand (r1,op1,op2,t) ->
    begin match op1,op2 with
    | Reg r2, Reg r3 -> map3 (fun (r1,r2,r3) -> Pand(r1, Reg r2, Reg r3,t)) r1 r2 r3
    | Reg r2, _ ->
      map2 (fun (r1,r2) -> Pand(r1, Reg r2, op2,t)) r1 r2
    | _, Reg r2 ->
      map2 (fun (r1,r2) -> Pand(r1, op1, Reg r2,t)) r1 r2
    | _,_ -> Pand(map_reg r1, op1, op2, t)
    end    

      (*Zero registers*)
  | Pmembar _  -> ins
  

(* GPU operation to change memory into shared or global locations *)
let to_shared s1 s2 = sprintf "cvta.to.shared %s, %s;" s1 s2
let to_global _s1 _s2 = ""

let gpu_macros = ""

(* This is what ARM and X86 do *)
let norm_ins ins = ins

(*unimplemented so far*)
let _get_reg_list _ins = ([], [])

let get_macro _name = Warn.fatal "GPU_PTX get_macro has not been implemented"
let is_data _reg _ins = Warn.fatal "GPU_PTX is_data has not been implemented"

let map_addrs _f _ins = Warn.fatal "GPU_PTX map_addrs has not been implemented"

"ARM and PPC do the same..."
let fold_addrs _f c _ins = c

let pp_instruction _m _ins = Warn.fatal "GPU_PTX dump_instruction has not been implemented"

let get_next _ins = Warn.fatal "GPU_PTX get_next not implemented"

let allowed_for_symb = []

let set_shared _i = Warn.fatal "GPU_PTX set_shared has not been implemented"
let set_global _i = Warn.fatal "GPU_PTX set_global has not been implmeneted"

let get_reg_list _i = Warn.fatal "Litmus GPU_PTX does not implement get_reg_list"

include ScopeTree
include MemSpaceMap
