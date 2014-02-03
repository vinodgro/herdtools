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
    module ARM = ARMArch.Make(C.PC)(V)
    include SemExtra.Make(C)(ARM)

(* Barrier pretty print *)
    let  dmb =
      ARMBase.fold_barrier_option
        (fun o k ->
          { barrier = ARMBase.DMB o;
            pp = String.lowercase
              (ARMBase.pp_barrier_option "dmb" o);}::k)
        []

    let  dsb =
      ARMBase.fold_barrier_option
        (fun o k ->
          { barrier = ARMBase.DSB o;
            pp = String.lowercase
              (ARMBase.pp_barrier_option "dsb" o);}::k)
        dmb
        
    let barriers = dsb
    let atrb = [] (* this line not used ...? *)
    let isync = Some { barrier = ARMBase.ISB;pp = "isb";}
 
(* Semantics proper *)
    let (>>=) = M.(>>=)
    let (>>*=) = M.(>>*=)
    let (>>|) = M.(>>|)
    let (>>!) = M.(>>!)
    let (>>::) = M.(>>::)

    let flip_flag v = M.op Op.Xor v V.one	
    let is_zero v = M.op Op.Eq v V.zero
    let is_not_zero v = M.op Op.Ne v V.zero
    let check_flag = function
      |ARM.AL -> assert false
      |ARM.NE -> flip_flag
      |ARM.EQ -> M.unitT

    let checkZ op c ii = match c with
    | ARM.AL -> op ii >>! B.Next
    | ARM.NE ->
        ((M.read_reg  ARM.Z ii)
	   >>=
	 (fun veq -> 
	   flip_flag veq >>=
	   fun veqneg ->
	     M.choiceT veqneg
	       (op ii)
	       (M.unitT ())))
	  >>! B.Next      
    | ARM.EQ ->
        ((M.read_reg  ARM.Z ii)
	   >>=
	 (fun veq -> 
	   M.choiceT veq
	     (op ii)
	     (M.unitT ())))
	  >>! B.Next      

    let checkCZ op c ii = match c with
    | ARM.AL -> op ii >>! B.Next
    | ARM.NE ->
        ((M.read_reg  ARM.Z ii)
	   >>=
	 (fun veq -> 
	   flip_flag veq >>=
	   fun veqneg ->
             M.commit ii >>*=
             fun () ->
	       M.choiceT veqneg
	         (op ii)
	         (M.unitT ())))
	  >>! B.Next      
    | ARM.EQ ->
        ((M.read_reg  ARM.Z ii)
	   >>=
	 (fun veq ->
           M.commit ii >>*=
	   fun () -> M.choiceT veq
	     (op ii)
	     (M.unitT ())))
	  >>! B.Next   

    let write_flags set v1 v2 ii = match set with
    | ARM.SetFlags -> M.write_flag ARM.Z Op.Eq v1 v2 ii
    | ARM.DontSetFlags -> M.unitT ()

    let build_semantics : int list -> ARM.instruction -> ARM.inst_instance_id -> B.t M.t
	= fun _procs i ii ->
	  match i with
	  | ARM.I_ADD (set,rd,rs,v) ->
	      ((M.read_reg rs ii) 
		 >>=
	       (fun vs ->
		 M.add vs (V.intToV v))
		 >>=
	       (fun vres ->
		 (M.write_reg rd vres ii) 
		   >>| 
		   write_flags set vres (V.intToV 0) ii))
		>>! B.Next
	  | ARM.I_SUB (set,rd,rs,v) ->
	      ((M.read_reg rs ii) 
		 >>=
	       (fun vs ->
		 M.op Op.Sub vs (V.intToV v))
		 >>=
	       (fun vres ->
		 (M.write_reg rd vres ii) 
		   >>| 
		   write_flags set vres (V.intToV 0) ii))
		>>! B.Next
	  | ARM.I_ADD3 (set,rd,rn,rm) ->
	      (((M.read_reg  rn ii) >>| (M.read_reg rm ii)) 
		 >>=
	       (fun (vn,vm) ->
		 M.op Op.Add vn vm
		 >>=
	       (fun vd ->
                 M.write_reg rd vd ii
                   >>|
                 write_flags set vd (V.intToV 0) ii)))
		>>! B.Next
	  | ARM.I_SUB3 (set,rd,rn,rm) ->
	      (((M.read_reg  rn ii) >>| (M.read_reg rm ii)) 
		 >>=
	       (fun (vn,vm) ->
		 M.op Op.Sub vn vm
		 >>=
	       (fun vd ->
                 M.write_reg rd vd ii
                   >>|
                 write_flags set vd (V.intToV 0) ii)))
		>>! B.Next
	  | ARM.I_AND (set,rd,rs,v) ->
	      ((M.read_reg  rs ii) 
		 >>=
	       (fun vs ->
		 M.op Op.And vs (V.intToV v))
		 >>=
	       (fun vres ->
		 M.write_reg  rd vres ii
                   >>|
                 write_flags set vres (V.intToV 0) ii))
		>>! B.Next
          | ARM.I_B lbl -> B.branchT lbl
	  | ARM.I_BEQ (lbl) ->
	      M.read_reg ARM.Z ii >>=
	      fun v -> M.commit ii >>= fun () -> B.bccT v lbl
	  | ARM.I_BNE (lbl) ->
	      M.read_reg ARM.Z ii >>=
	      fun v -> flip_flag v >>= fun vneg -> M.commit ii >>=
                fun () -> B.bccT vneg lbl 
          | ARM.I_CB (n,r,lbl) ->
              let cond = if n then is_not_zero else is_zero in
              M.read_reg r ii >>= cond >>=
                fun v -> M.commit ii >>= fun () -> B.bccT v lbl
	  | ARM.I_CMPI (r,v) ->
	      ((M.read_reg  r ii) 
		 >>= 
	       (fun vr ->
		 write_flags ARM.SetFlags vr (V.intToV v) ii))
		>>! B.Next
	  | ARM.I_CMP (r1,r2) ->
	      (((M.read_reg  r1 ii)  >>| (M.read_reg  r2 ii))
		 >>= 
	       (fun (v1,v2) ->
		 write_flags ARM.SetFlags v1 v2 ii))
		>>! B.Next
	  |  ARM.I_LDR (rt,rn,c) ->
              let ldr ii =
	        (M.read_reg  rn ii)
		   >>= 
	         (fun vn ->
		   (M.read_mem vn ii) >>=
		   (fun v -> M.write_reg  rt v ii)) in
              checkCZ ldr c ii
          |  ARM.I_LDREX (rt,rn) ->
              let ldr ii =
	        (M.read_reg  rn ii)
		   >>= 
	         (fun vn ->
		   (M.read_mem_atomic vn ii) >>=
		   (fun v -> M.write_reg  rt v ii)) in
              ldr ii >>! B.Next
	  |  ARM.I_LDR3 (rt,rn,rm,c) ->
              let ldr3 ii =
                ((M.read_reg  rn ii) >>| (M.read_reg  rm ii))
		   >>= 
	         (fun (vn,vm) ->
		   (M.add vn vm) >>=
		   (fun vaddr -> 
		     (M.read_mem vaddr ii) >>=
		     (fun v -> M.write_reg  rt v ii))) in
              checkZ ldr3 c ii
	  |  ARM.I_STR (rt,rn,c) ->
              let str ii =
	        ((M.read_reg  rn ii) >>| (M.read_reg  rt ii))
		  >>= 
	        (fun (vn,vt) ->
		  let a = vn in
		  (M.write_mem a vt ii)) in
              checkCZ str c ii
	  |  ARM.I_STR3 (rt,rn,rm,c) ->
              let str3 ii = 
	        (((M.read_reg  rm ii) >>|
                ((M.read_reg  rn ii) >>|
                (M.read_reg  rt ii)))
		   >>= 
	         (fun (vm,(vn,vt)) ->
		   (M.add vn vm) >>=
		   (fun a ->
		     (M.write_mem a vt ii)))) in
              checkZ str3 c ii
          | ARM.I_STREX (r1,r2,r3,c) ->
              let strex ii =
                (M.read_reg r2 ii >>| M.read_reg r3 ii) >>=
                fun (v,a) -> 
                  M.altT
                    (M.write_reg r1 V.one ii >>! ())
                    ((M.write_reg r1 V.zero ii >>|
                    M.write_mem_atomic a v ii) >>! ()) in
              checkZ strex c ii
	  | ARM.I_MOV (rd, rs, c) -> 
              let mov ii =
	        M.read_reg  rs ii >>=
                fun v -> M.write_reg  rd v ii in
              checkCZ mov c ii
	  | ARM.I_MOVI (rt, i, c) -> 
              let movi ii =  M.write_reg  rt (V.intToV i) ii in
              checkCZ movi c ii
	  | ARM.I_XOR (set,r3,r1,r2) ->
	      (((M.read_reg  r1 ii) >>| (M.read_reg r2 ii)) 
		 >>=
	       (fun (v1,v2) ->
		 M.op Op.Xor v1 v2
		 >>=
	       (fun v3 ->
		 M.write_reg  r3 v3 ii
                   >>|
                write_flags set v3 (V.intToV 0) ii)))
		>>! B.Next
	  | ARM.I_DMB o ->
	      (M.create_barrier (ARM.DMB o) ii)
		>>! B.Next
	  | ARM.I_DSB o ->
	      (M.create_barrier (ARM.DSB o) ii)
		>>! B.Next
	  | ARM.I_ISB ->
	      (M.create_barrier ARM.ISB ii)
		>>! B.Next
          | ARM.I_SADD16 _ ->
              Warn.user_error "SADD16 not implemented"
          | ARM.I_SEL _ ->
              Warn.user_error "SEL not implemented"
  end
