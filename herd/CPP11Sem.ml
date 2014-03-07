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
		       
    let read_loc mo = M.read_loc (fun loc v -> Act.Access (Dir.R, loc, v, mo))
    let read_reg r ii = read_loc CPP11.NA (A.Location_reg (ii.A.proc,r)) ii
    let read_mem mo a = read_loc mo (A.Location_global a)

    let write_loc mo loc v ii = M.mk_singleton_es (Act.Access (Dir.W, loc, v, mo)) ii
    let write_reg r v ii = write_loc CPP11.NA (A.Location_reg (ii.A.proc,r)) v ii
    let write_mem mo a  = write_loc mo (A.Location_global a) 	     
		 
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

      | CPP11.Plock l ->
	(M.unitT (CPP11.maybev_to_location l)) >>=
	  fun loc -> 
          M.altT
            (* successful attempt to obtain mutex *)
	    (M.mk_singleton_es (Act.Lock (loc, true)) ii >>! B.Next)
            (* unsuccessful attempt to obtain mutex *)
	    (M.mk_singleton_es (Act.Lock (loc, false)) ii >>! B.Next)

      | CPP11.Punlock l ->
	(M.unitT (CPP11.maybev_to_location l)) >>=
	  fun loc -> 
	  M.mk_singleton_es (Act.Unlock loc) ii >>! B.Next

      | CPP11.Pcas(l,v1,v2,mo_success, _ (*mo_failure *)) ->
	 (M.unitT (CPP11.maybev_to_location l)) >>|
	   (M.unitT (V.intToV (constant_to_int v1))) >>|
           (M.unitT (V.intToV (constant_to_int v2))) >>=
	   (* Todo: give this the proper CAS semantics *)
	   fun ((loc, v1), v2) -> 
	   M.mk_singleton_es (Act.RMW (loc,v1,v2,mo_success)) ii >>! B.Next     
								    
      | CPP11.Pfence(mo) ->
	M.mk_singleton_es (Act.Fence mo) ii >>! B.Next

  end
