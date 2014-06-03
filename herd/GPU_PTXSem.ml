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

(** Semantics of PTX instructions *)

module Make (C:Sem.Config)(V:Value.S)  
    = 
  struct

    module GPU_PTX = GPU_PTXArch.Make(C.PC)(V)
    module Act = MachAction.Make(GPU_PTX)
    include SemExtra.Make(C)(GPU_PTX)(Act)

(* barrier pretty print *)
    let bar_cta = {barrier=GPU_PTX.Membar GPU_PTX.CTA_bar; pp="membar.cta";}
    let bar_gl = {barrier=GPU_PTX.Membar GPU_PTX.GL_bar; pp="membar.gl";}
    let bar_sys = {barrier=GPU_PTX.Membar GPU_PTX.SYS_bar; pp="membar.sys";}

    let barriers = [bar_cta; bar_gl; bar_sys]

    let isync = None
        
(****************************)	  
(* Build semantics function *)
(****************************)	  

    let (>>=) = M.(>>=)
    let (>>*=) = M.(>>*=)
    let (>>|) = M.(>>|)
    let (>>::) = M.(>>::)
    let (>>!) = M.(>>!)

    let mk_read ato loc v = Act.Access (Dir.R, loc, v, ato)

    let read_reg r ii = 
      M.read_loc (mk_read false) (A.Location_reg (ii.A.proc,r)) ii
    let read_mem a ii = 
      M.read_loc (mk_read false) (A.Location_global a) ii

    let write_reg r v ii = 
      M.mk_singleton_es (Act.Access (Dir.W, (A.Location_reg (ii.A.proc,r)), v, false)) ii
    let write_mem a v ii = 
      M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v, false)) ii

    let create_barrier b ii = 
      M.mk_singleton_es (Act.Barrier b) ii

    let read_ins_op op ii = 
      match op with 
      | GPU_PTX.Reg reg -> read_reg reg ii
      | GPU_PTX.Im i -> (M.unitT (V.intToV i))
      

    let constant_to_int v = match v with
      | Constant.Concrete vv -> vv
      | _ -> Warn.fatal "Couldn't convert constant to int"

    let build_semantics _st ii = match ii.A.inst with
      | GPU_PTX.Pld(reg1,reg2,_,_cop,_) ->
	read_reg reg2 ii >>= 
	    (fun addr -> read_mem addr ii) >>=
	    (fun v -> write_reg reg1 v ii) >>!
	  B.Next

      | GPU_PTX.Pst(reg1,reg2,_,_cop,_) -> 
	read_reg reg1 ii >>| read_reg reg2 ii >>=
	    (fun (addr,v) -> write_mem addr v ii) >>!
	      B.Next 

      | GPU_PTX.Pldvol(reg1,reg2,_,_) ->
	read_reg reg2 ii >>= 
	    (fun addr -> read_mem addr ii) >>=
	    (fun v -> write_reg reg1 v ii) >>!
	  B.Next

      | GPU_PTX.Pstvol(reg1,reg2,_,_) ->
	read_reg reg1 ii >>| read_reg reg2 ii >>=
	    (fun (addr,v) -> write_mem addr v ii) >>!
	      B.Next 

      | GPU_PTX.Pmov(reg1,op,_) ->
	read_ins_op op ii >>= 
	    (fun v -> write_reg reg1 v ii) >>!
	      B.Next 

      | GPU_PTX.Padd(reg,op1,op2,_) ->
	read_ins_op op1 ii >>| read_ins_op op2 ii >>= 
	    (fun (v1,v2) -> M.op Op.Add v1 v2) >>=
	    (fun v -> write_reg reg v ii) >>!
	      B.Next

      | GPU_PTX.Pand(reg,op1,op2,_) ->
	read_ins_op op1 ii >>| read_ins_op op2 ii >>= 
	    (fun (v1,v2) -> M.op Op.And v1 v2) >>=
	    (fun v -> write_reg reg v ii) >>!
	      B.Next


      | GPU_PTX.Pmembar(scope) ->
	create_barrier (GPU_PTX.Membar scope) ii >>! B.Next

      (*cvt is just an instruction for converting data types*)
      | GPU_PTX.Pcvt (reg1,reg2,_,_) -> 
	read_reg reg2 ii >>= 
	  (fun v -> write_reg reg1 v ii) >>! B.Next

  end
