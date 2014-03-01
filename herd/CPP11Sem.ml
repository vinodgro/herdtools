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

module Make (C:Sem.Config)(V:Value.S)  
    = 
  struct

    module CPP11 = CPP11Arch.Make(C.PC)(V)
    module Act = CPP11Action.Make(CPP11)
    include SemExtra.Make(C)(CPP11)(Act)

(* barrier pretty print *)
    let bar_acq = {barrier=CPP11.Fence CPP11.Acq; pp="fence(mo_acquire)";}
    let bar_rel = {barrier=CPP11.Fence CPP11.Rel; pp="fence(mo_release)";}
    let bar_sc = {barrier=CPP11.Fence CPP11.SC; pp="fence(mo_sc)";}
    let bar_acq_rel = {barrier=CPP11.Fence CPP11.Acq_Rel; pp="fence(mo_acq_rel)";}

    let barriers = [bar_acq; bar_rel; bar_sc; bar_acq_rel]

    let isync = None
        


(****************************)	  
(* Build semantics function *)
(****************************)	  

    let (>>=) = M.(>>=)
    let (>>*=) = M.(>>*=)
    let (>>|) = M.(>>|)
    let (>>::) = M.(>>::)
    let (>>!) = M.(>>!)
		       
    let read_loc mo = M.read_loc (fun loc v -> Act.mk_Access_CPP11 (Dir.R, loc, v, mo))
    let read_reg r ii = read_loc CPP11.NA (A.Location_reg (ii.A.proc,r)) ii
    let read_mem mo a = read_loc mo (A.Location_global a)

    let write_loc mo loc v ii = M.mk_action (Act.mk_Access_CPP11 (Dir.W, loc, v, mo)) ii
    let write_reg r v ii = write_loc CPP11.NA (A.Location_reg (ii.A.proc,r)) v ii
    let write_mem mo a  = write_loc mo (A.Location_global a) 	     
		 
    let read_addr a ii = read_mem CPP11.NA (* ? *) a ii

    let constant_to_int v = match v with
      | Constant.Concrete vv -> vv
      | _ -> Warn.fatal "Couldn't convert constant to int"

    let build_semantics _st i ii = match i with
      | CPP11.Pload(loc,reg,mo) ->
	M.unitT (CPP11.maybev_to_location loc) >>=
	(fun loc -> read_loc mo loc ii) >>= 
	  (fun v -> write_reg reg v ii) >>! 
	  B.Next

      | CPP11.Pstore(l,v,mo) ->
	(M.unitT (CPP11.maybev_to_location l)) >>|
	    (M.unitT (V.intToV (constant_to_int v))) >>=
	(fun (loc, vv) -> write_loc mo loc vv ii) >>! B.Next
      (*
    | PPC.Pstwcx(rS,rA,rB) ->
	((read_reg rS ii >>| read_reg PPC.RES ii)
	   >>| (* Enforce right associativity of >>| *)
	   (read_reg_or_zero rA ii >>| read_reg rB ii)) >>=
	fun ((vS,_),(aA,aB)) ->
	  M.add aA aB  >>=
	  fun a ->
            M.altT
              ((write_reg PPC.RES V.zero ii >>| flags_res false ii) >>! B.Next)
              (write_reg PPC.RES V.zero ii >>| (write_addr_conditional a vS ii >>|
              flags_res true ii) >>! B.Next)
       *)

      | CPP11.Plock l ->
	(M.unitT (CPP11.maybev_to_location l)) >>=
	  fun loc -> 
          M.altT
	    (M.mk_action (Act.mk_Lock (loc, true)) ii >>! B.Next)
	    (M.mk_action (Act.mk_Lock (loc, false)) ii >>! B.Next)

      | CPP11.Punlock l ->
	(M.unitT (CPP11.maybev_to_location l)) >>=
	  fun loc -> 
	  M.mk_action (Act.mk_Unlock loc) ii >>! B.Next

      | CPP11.Pcas(l,v1,v2,mo_success, _ (*mo_failure *)) ->
	 (M.unitT (CPP11.maybev_to_location l)) >>|
	   (M.unitT (V.intToV (constant_to_int v1))) >>|
           (M.unitT (V.intToV (constant_to_int v2))) >>=
	   (* Todo: give this the proper CAS semantics *)
	   fun ((loc, v1), v2) -> 
	   M.mk_action (Act.mk_RMW (loc,v1,v2,mo_success)) ii >>! B.Next     
								    
      | CPP11.Pfence(mo) ->
	M.create_barrier (CPP11.Fence mo) ii >>! B.Next

  end
