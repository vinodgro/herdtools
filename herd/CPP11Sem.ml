
module Make (C:Sem.Config)(V:Value.S)  
    = 
  struct

    module CPP11 = CPP11Arch.Make(C.PC)(V)
    include SemExtra.Make(C)(CPP11) 
(* barrier pretty print *)

    let bar_acq = {barrier=CPP11.Fence CPP11.Acq; pp="fence(mo_acquire)";}
    let bar_rel = {barrier=CPP11.Fence CPP11.Rel; pp="fence(mo_release)";}
    let bar_sc = {barrier=CPP11.Fence CPP11.SC; pp="fence(mo_sc)";}
    let bar_acq_rel = {barrier=CPP11.Fence CPP11.Acq_Rel; pp="fence(mo_acq_rel)";}

    let barriers = [bar_acq; bar_rel; bar_sc; bar_acq_rel]
    (*Should match pp atrb in arch base file*)
    let atrb = ["rel"; "acq" ; "acq_rel"; "sc"; "na"; "rlx"; "con"]
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
      | CPP11.Pload(loc,reg,mo) ->
	M.unitT (CPP11.maybev_to_location loc) >>=
	(fun loc -> M.read_loc_atrb loc ii [mo]) >>= 
	  (fun v -> M.write_reg reg v ii) >>! 
	  B.Next

      | CPP11.Pstore(l,v,mo) ->
	(M.unitT (CPP11.maybev_to_location l)) >>|
	    (M.unitT (V.intToV (constant_to_int v))) >>=
	(fun (loc, vv) -> M.write_loc_atrb loc vv ii [mo]) >>! B.Next


      | CPP11.Pfence(mo) ->
        (* Todo: should it really be "SC" below? Should it not be "mo"? *)
	M.create_barrier_atrb (CPP11.Fence mo) ii [mo] >>! B.Next

  end
