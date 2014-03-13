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
	(fun (loc, vv) -> write_loc mo loc vv ii) >>! 
        B.Next

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

(*
Are we creating po-edges between the events as they
are added to the event structure? (Hopefully yes.) 
Same question for data/address dependencies. (The
read of loc_exp should have a control dependency
to all the other events that follow. (This control
dependency isn't actually used though.) In the failure case
there should be a data dependency from the atomic read of
the object, to the non-atomic write of expected.) 

For the strong case, we need to rule out the fail
case when the values do match up.
*)

(*
atomic_compare_exchange_weak_explicit(
  A* object, 
  C* expected, 
  C desired, 
  memory_order success, 
  memory_order failure)

	M.unitT (CPP11.maybev_to_location expected) >>=
	(fun loc_exp -> read_loc na loc_exp ii) >>= 
        (fun v_exp ->
          M.altT
            ((M.unitT (CPP11.maybev_to_location object)) >>=
            (fun loc_obj -> read_loc failure loc_obj ii) >>=
            (fun v_obj -> write_loc loc_exp v_obj) >>!
            B.Next)
            ((M.unitT (CPP11.maybev_to_location object)) >>|
            (M.unitT (V.intToV (constant_to_int desired))) >>= 
            (fun (loc_obj,v_des) -> 
            rmw_loc success loc_obj v_exp v_des ii) >>!
            B.Next))

	M.unitT (CPP11.maybev_to_location expected) >>=
	(fun loc_exp -> read_loc na loc_exp ii) >>= 
        (fun v_exp ->
          if v_exp != v_obj then
            ((M.unitT (CPP11.maybev_to_location object)) >>=
            (fun loc_obj -> read_loc failure loc_obj ii) >>=
            (fun v_obj -> write_loc loc_exp v_obj) >>!
            B.Next)
          else
            ((M.unitT (CPP11.maybev_to_location object)) >>|
            (M.unitT (V.intToV (constant_to_int desired))) >>= 
            (fun (loc_obj,v_des) -> 
            rmw_loc success loc_obj v_exp v_des ii) >>!
            B.Next))



A successful CAS results in a non-atomic load of expected and a RMW of
A. The value stored at the location addressed by the expected argument
has to match the desired value (what terrible names!). The non-atomic
load should be po and data-before the RMW.

A failed CAS results in three memory accesses: a non-atomic load of
expected, an atomic load of A, and a non-atomic write of expected.
This time, the three accesses should be ordered by po and data in
sequence: NA-load, atomic-load, NA-store.
*)

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
