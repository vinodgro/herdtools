open Constant

module Make (C:Sem.Config)(V:Value.S)  
    = 
  struct

    module OpenCL = OpenCLArch.Make(C.PC)(V)
    include SemExtra.Make(C)(OpenCL) 
(* barrier pretty print *)

(* Todo: add other scopes besides S_device *)

    let bar_acq     = {barrier=OpenCL.Fence (OpenCL.Acq, OpenCL.S_device); 
                       pp="fence(mo_acquire,S_device)";}
    let bar_rel     = {barrier=OpenCL.Fence (OpenCL.Rel, OpenCL.S_device); 
                       pp="fence(mo_release,S_device)";}
    let bar_sc      = {barrier=OpenCL.Fence (OpenCL.SC, OpenCL.S_device); 
                       pp="fence(mo_sc,S_device)";}
    let bar_acq_rel = {barrier=OpenCL.Fence (OpenCL.Acq_Rel, OpenCL.S_device);
                       pp="fence(mo_acq_rel,S_device)";}

    let barriers = [bar_acq; bar_rel; bar_sc; bar_acq_rel]
    let atrb = ["acq"; "rel"; "acq_rel"; "sc"; "rlx"; "na"; "wi"; "sg"; "dev"; "all_dev"]
    let isync = None
        


(****************************)	  
(* Build semantics function *)
(****************************)	  

    let (>>=) = M.(>>=)
    let (>>*=) = M.(>>*=)
    let (>>|) = M.(>>|)
    let (>>::) = M.(>>::)
    let (>>!) = M.(>>!)

    let read_addr a ii = M.read_mem a ii

    let constant_to_int v = match v with
      | Constant.Concrete vv -> vv
      | _ -> Warn.fatal "Couldn't convert constant to int"

    let build_semantics _st i ii = match i with
      | OpenCL.Pload(loc,reg,mo,scope) ->
        (* Todo: do something with the scope parameter *)
	M.unitT (OpenCL.maybev_to_location loc) >>=
	(fun loc -> M.read_loc_atrb loc ii [Mem_order mo; Mem_scope scope]) >>= 
	  (fun v -> M.write_reg reg v ii) >>! 
	  B.Next

      | OpenCL.Pstore(l,v,mo,scope) ->
        (* Todo: do something with the scope parameter *)
	(M.unitT (OpenCL.maybev_to_location l)) >>|
	    (M.unitT (V.intToV (constant_to_int v))) >>=
	(fun (loc, vv) -> M.write_loc_atrb loc vv ii [Mem_order mo; Mem_scope scope]) >>! B.Next


      | OpenCL.Pfence(mo,scope) ->
        (* Todo: should it really be "SC" below? Should it not be "mo"? *)
	M.create_barrier (OpenCL.Fence (mo, scope)) ii >>! B.Next
  end
