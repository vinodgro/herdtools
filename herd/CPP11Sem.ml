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
    let (>>>) = M.(>>>)
		       
    let read_loc mo = M.read_loc (fun loc v -> Act.Access (Dir.R, loc, v, mo))
    let read_reg r ii = read_loc CPP11.NA (A.Location_reg (ii.A.proc,r)) ii
    let read_mem mo a = read_loc mo (A.Location_global a)

    let write_loc mo loc v ii = M.mk_singleton_es (Act.Access (Dir.W, loc, v, mo)) ii
    let write_reg r v ii = write_loc CPP11.NA (A.Location_reg (ii.A.proc,r)) v ii
    let write_mem mo a  = write_loc mo (A.Location_global a) 	     
		 
    let constant_to_int v = match v with
      | Constant.Concrete vv -> vv
      | _ -> Warn.fatal "Couldn't convert constant to int"

    let rec build_semantics_expr e ii : V.v M.t = match e with
      | CPP11.Econstant v -> 	
        M.unitT (V.intToV (constant_to_int v))

      | CPP11.Eregister reg ->
        read_reg reg ii >>= fun v -> 
        M.unitT v

      | CPP11.Eassign (reg,e) ->
        build_semantics_expr e ii >>= fun v ->
        write_reg reg v ii >>! 
        v

      | CPP11.Eeq (e1, e2) ->
        build_semantics_expr e1 ii >>= fun v1 ->
        build_semantics_expr e2 ii >>= fun v2 ->
        (* TODO: remove sequencing between the above *)
        M.op Op.Eq v1 v2

      | CPP11.Estore(l,e,mo) ->
	(M.unitT (CPP11.maybev_to_location l)) >>|
	build_semantics_expr e ii >>= fun (loc, v) -> 
        write_loc mo loc v ii >>! 
        v

      | CPP11.Eload(loc,mo) ->
	M.unitT (CPP11.maybev_to_location loc) >>= fun loc -> 
        read_loc mo loc ii

      | CPP11.Elock l ->
	M.unitT (CPP11.maybev_to_location l) >>= fun loc -> 
        M.altT
          (* successful attempt to obtain mutex *)
	  (M.mk_singleton_es (Act.Lock (loc, true)) ii >>! 
           V.intToV 0)
          (* unsuccessful attempt to obtain mutex *)
	  (M.mk_singleton_es (Act.Lock (loc, false)) ii >>! 
           V.intToV 1)

      | CPP11.Eunlock l ->
	M.unitT (CPP11.maybev_to_location l)>>= fun loc -> 
	M.mk_singleton_es (Act.Unlock loc) ii >>! 
        V.intToV 0

      | CPP11.Ecas(obj,exp,des,success,failure,strong) ->
        (* Obtain location of "expected" value *)
        M.unitT (CPP11.maybev_to_location exp) >>= fun loc_exp ->
        (* Obtain location of object *) 
        M.unitT (CPP11.maybev_to_location obj) >>= fun loc_obj -> 
        (* Non-atomically read the value at "expected" location *)
        read_loc CPP11.NA loc_exp ii >>*= fun v_exp -> 
        (* Non-deterministic choice *)
        M.altT
          (* Read memory at location "object", using memory order "failure" *)
          (read_loc failure loc_obj ii >>*= fun v_obj ->
           (* For "strong" cas: fail only when v_obj != v_exp *)
           (if strong then M.addNeqConstraintT v_obj v_exp else (fun x -> x)) (
           (* Non-atomically write that value into the "expected" location *)
           write_loc CPP11.NA loc_exp v_obj ii) >>!
           V.intToV 0)
          (* Obtain "desired" value *)
          (build_semantics_expr des ii >>= fun v_des -> 
           (* Do RMW action on "object", to change its value from "expected"
              to "desired", using memory order "success" *)
           M.mk_singleton_es (Act.RMW (loc_obj,v_exp,v_des,success)) ii >>!
           V.intToV 1)
						    
      | CPP11.Efence(mo) ->
	M.mk_singleton_es (Act.Fence mo) ii >>! V.intToV 0

        
    let rec build_semantics ii : branch M.t = match ii.A.inst with
      | CPP11.Pblock insts -> 
        build_semantics_list insts ii 
      
      | CPP11.Pexpr e ->
        build_semantics_expr e ii >>!
        B.Next
      
      | CPP11.Pif (e,i1,i2) ->
        let evts = build_semantics_expr e ii in
        let prog_order = ii.A.program_order_index in
        let ii' = {ii with A.program_order_index = A.next_po_index prog_order;} in
        (* TODO: Advance program_order_index by the right amount for each instruction *)
        evts >>> fun ret ->
        M.choiceT ret
          (build_semantics {ii' with A.inst = i1})
          (build_semantics {ii' with A.inst = i2}) >>! B.Next
      
      | CPP11.Pwhile (e,i1) ->
        let evts = build_semantics_expr e ii in
        let prog_order = ii.A.program_order_index in
	let ii' = { ii with A.program_order_index = A.next_po_index prog_order;} in
        (* TODO: Advance program_order_index by the right amount for each instruction *)
        evts >>> fun ret -> 
        M.choiceT ret
            (build_semantics {ii' with A.inst = i1} >>> fun _ ->
             build_semantics ii' >>! 
            B.Next)
            (M.unitT B.Next)
        
    and build_semantics_list insts ii = match insts with
      | [] -> M.unitT B.Next
      | inst :: insts ->
	let ii = {ii with A.inst=inst; } in
	let evts = build_semantics ii in
        let prog_order = ii.A.program_order_index in
        let ii' = {ii with A.program_order_index = A.next_po_index prog_order;} in
	evts  >>> fun _branch -> build_semantics_list insts ii'
     
  end
