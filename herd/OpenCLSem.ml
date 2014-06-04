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

(** Semantics of OpenCL instructions *)

module Make (C:Sem.Config)(V:Value.S)  
    = 
  struct

    module OpenCL = OpenCLArch.Make(C.PC)(V)
    module Act = OpenCLAction.Make(OpenCL)
    include SemExtra.Make(C)(OpenCL)(Act)
    let barriers = []
    let isync = None      


(****************************)	  
(* Build semantics function *)
(****************************)	  

    let (>>=) = M.(>>=)
    let (>>*=) = M.(>>*=)
    let (>>|) = M.(>>|)
    let (>>::) = M.(>>::)
    let (>>!) = M.(>>!)

    let read_loc s mo = M.read_loc (fun loc v -> Act.Access (Dir.R, loc, v, mo, s))
    let read_reg r ii = read_loc OpenCL.S_workitem OpenCL.NA (A.Location_reg (ii.A.proc,r)) ii
    let read_mem s mo a = read_loc s mo (A.Location_global a)

    let write_loc s mo loc v ii = M.mk_singleton_es (Act.Access (Dir.W, loc, v, mo, s)) ii
    let write_reg r v ii = write_loc OpenCL.S_workitem OpenCL.NA (A.Location_reg (ii.A.proc,r)) v ii
    let write_mem s mo a  = write_loc s mo (A.Location_global a) 	     
		 
    let constant_to_int v = match v with
      | Constant.Concrete vv -> vv
      | _ -> Warn.fatal "Couldn't convert constant to int"

    let build_semantics ii = 
      M.addT (A.next_po_index ii.A.program_order_index)
        begin match ii.A.inst with
      | OpenCL.Pload(loc,reg,mo,scope) ->
	M.unitT (OpenCL.maybev_to_location loc) >>=
	(fun loc -> read_loc scope mo loc ii) >>= 
	  (fun v -> write_reg reg v ii) >>! 
	  B.Next

      | OpenCL.Pstore(l,v,mo,scope) ->
	(M.unitT (OpenCL.maybev_to_location l)) >>|
	    (M.unitT (V.intToV (constant_to_int v))) >>=
	(fun (loc, vv) -> write_loc scope mo loc vv ii) >>! B.Next

      | OpenCL.Pfence(mr, mo, scope) ->
	M.mk_singleton_es (Act.Fence(mr, mo, scope)) ii >>! B.Next
        end
  end
