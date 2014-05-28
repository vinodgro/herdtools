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

(** Semantics of CPP11 instructions *)

module Make (C:Sem.Config)(V:Value.S)  
    = 
  struct

    module CA = CArch.Make(C.PC)(V)
    module Act = CAction.Make(CA)
    include SemExtra.Make(C)(CA)(Act)
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
    let read_reg r ii = read_loc CA.NA (A.Location_reg (ii.A.proc,r)) ii
    let read_mem mo a = read_loc mo (A.Location_global a)

    let write_loc mo loc v ii = M.mk_singleton_es (Act.Access (Dir.W, loc, v, mo)) ii
    let write_reg r v ii = write_loc CA.NA (A.Location_reg (ii.A.proc,r)) v ii
    let write_mem mo a  = write_loc mo (A.Location_global a) 	     
		 
    let constant_to_int v = match v with
      | Constant.Concrete vv -> vv
      | _ -> Warn.fatal "Couldn't convert constant to int"

    let build_semantics _st i ii = match i with
      | CA.Pload(loc,reg,mo) ->
	M.unitT (CA.maybev_to_location loc) >>= fun loc -> 
        read_loc mo loc ii >>= fun v -> 
        write_reg reg v ii >>! 
	(Some v, B.Next)

      | CA.Passign(sop,reg) ->
	let v = V.cstToV sop in
        write_reg reg v ii >>! 
	(Some v, B.Next)

      | CA.Pstore(l,v,mo) ->
	M.unitT (CA.maybev_to_location l) >>|
	M.unitT (V.intToV (constant_to_int v)) >>= fun (loc, vv) -> 
        write_loc mo loc vv ii >>! 
        (Some vv, B.Next)

      | CA.Pexpr_const(sop) ->
        let v = V.cstToV sop in
        M.unitT (Some v, B.Next)

      | CA.Pexpr_reg(reg) ->
        read_reg reg ii >>= fun v ->
        M.unitT (Some v, B.Next)

      | CA.Pexpr_eqeq(reg,sop) ->
        read_reg reg ii >>= fun v1 ->
        let v2 = V.cstToV sop in
        M.op Op.Eq v1 v2 >>= fun res ->
        M.unitT (Some res, B.Next)

      | CA.Plock l ->
	M.unitT (CA.maybev_to_location l) >>= fun loc -> 
        (M.altT
          (* successful attempt to obtain mutex *)
	  (M.mk_singleton_es (Act.Lock (loc, true)) ii)
          (* unsuccessful attempt to obtain mutex *)
          (M.mk_singleton_es (Act.Lock (loc, false)) ii)) >>! 
        (None, B.Next)

      | CA.Punlock l ->
	M.unitT (CA.maybev_to_location l) >>= fun loc -> 
        M.mk_singleton_es (Act.Unlock loc) ii >>! 
        (None, B.Next)

      | CA.Pcas(obj,exp,des,success,failure,strong) ->
        (* Obtain location of "expected" value *)
        M.unitT (CA.maybev_to_location exp) >>= fun loc_exp ->
        (* Obtain location of object *) 
        M.unitT (CA.maybev_to_location obj) >>= fun loc_obj -> 
        (* Non-atomically read the value at "expected" location *)
        read_loc CA.NA loc_exp ii >>*= fun v_exp -> 
        (* Non-deterministic choice *)
        (M.altT
          (* Read memory at location "object", using memory order "failure" *)
          (read_loc failure loc_obj ii >>*= fun v_obj ->
          (* For "strong" cas: fail only when v_obj != v_exp *)
          (if strong then M.addNeqConstraintT v_obj v_exp else (fun x -> x)) (
            (* Non-atomically write that value into the "expected" location *)
            write_loc CA.NA loc_exp v_obj ii))
          (* Obtain "desired" value *)
          (M.unitT (V.intToV (constant_to_int des)) >>= fun v_des -> 
           (* Do RMW action on "object", to change its value from "expected"
              to "desired", using memory order "success" *)
           M.mk_singleton_es (Act.RMW (loc_obj,v_exp,v_des,success)) ii)) >>!
        (None, B.Next)
						    
      | CA.Pfence(mo) ->
	M.mk_singleton_es (Act.Fence mo) ii >>! 
        (None, B.Next)

  end
