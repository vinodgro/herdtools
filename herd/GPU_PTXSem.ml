open Printf

module Make (C:Sem.Config)(V:Value.S)  
    = 
  struct

    module GPU_PTX = GPU_PTXArch.Make(C.PC)(V)
    include SemExtra.Make(C)(GPU_PTX) 

(* barrier pretty print *)
    let bar_cta = {barrier=GPU_PTX.Fence GPU_PTX.CTA; pp="membar.cta";}
    let bar_gl = {barrier=GPU_PTX.Fence GPU_PTX.GL; pp="membar.gl";}
    let bar_sys = {barrier=GPU_PTX.Fence GPU_PTX.SYS; pp="membar.sys";}

    let barriers = [bar_cta; bar_gl; bar_sys]
    let atrb = []
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
    let read_ins_op op ii = 
      match op with 
      | GPU_PTX.Reg reg -> M.read_reg reg ii
      | GPU_PTX.Im i -> (M.unitT (V.intToV i))
      

    let constant_to_int v = match v with
      | Constant.Concrete vv -> vv
      | _ -> Warn.fatal "Couldn't convert constant to int"

    let build_semantics _st i ii = match i with
      | GPU_PTX.Pld(reg1,reg2,_,_cop,_) ->
	M.read_reg reg2 ii >>= 
	    (fun addr -> M.read_mem addr ii) >>=
	    (fun v -> M.write_reg reg1 v ii) >>!
	  B.Next

      | GPU_PTX.Pst(reg1,reg2,_,_cop,_) -> 
	M.read_reg reg1 ii >>| M.read_reg reg2 ii >>=
	    (fun (addr,v) -> M.write_mem addr v ii) >>!
	      B.Next 

      | GPU_PTX.Pldvol(reg1,reg2,_,_) ->
	M.read_reg reg2 ii >>= 
	    (fun addr -> M.read_mem addr ii) >>=
	    (fun v -> M.write_reg reg1 v ii) >>!
	  B.Next

      | GPU_PTX.Pstvol(reg1,reg2,_,_) ->
	M.read_reg reg1 ii >>| M.read_reg reg2 ii >>=
	    (fun (addr,v) -> M.write_mem addr v ii) >>!
	      B.Next 

      | GPU_PTX.Pmov(reg1,op,_) ->
	read_ins_op op ii >>= 
	    (fun v -> M.write_reg reg1 v ii) >>!
	      B.Next 

      | GPU_PTX.Padd(reg,op1,op2,_) ->
	read_ins_op op1 ii >>| read_ins_op op2 ii >>= 
	    (fun (v1,v2) -> M.op Op.Add v1 v2) >>=
	    (fun v -> M.write_reg reg v ii) >>!
	      B.Next

      | GPU_PTX.Pand(reg,op1,op2,_) ->
	read_ins_op op1 ii >>| read_ins_op op2 ii >>= 
	    (fun (v1,v2) -> M.op Op.And v1 v2) >>=
	    (fun v -> M.write_reg reg v ii) >>!
	      B.Next


      | GPU_PTX.Pmembar(scope) ->
	M.create_barrier (GPU_PTX.Fence scope) ii >>! B.Next

      (*cvt is just an intsruction for converting data types*)
      | GPU_PTX.Pcvt (reg1,reg2,_,_) -> 
	M.read_reg reg2 ii >>= 
	  (fun v -> M.write_reg reg1 v ii) >>! B.Next

      | _ -> 
	let warn_st = sprintf "GPU_PTX does not currently have semenatics for %s" (GPU_PTX.dump_instruction i) in
	Warn.fatal "%s" warn_st
  end
