(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf
open Code

module Make(V:Constant.S)(Cfg:CompileCommon.Config) : XXXCompile.S =
  struct

    module ARM = ARMArch.Make(V)
    include CompileCommon.Make(Cfg)(ARM)


(******)
    let ppo _f k = k

    open ARM
    open C


(* Utilities *)
    let next_reg x = ARM.alloc_reg x

    let next_init st p init loc =
      let rec find_rec = function
        | (Reg (p0,r0),loc0)::_ when loc0 = loc && p = p0 ->
            r0,init,st
        | _::rem -> find_rec rem
        | [] ->
            let r = Symbolic_reg (sprintf "%s%i" loc p) in
            r,(Reg (p,r),loc)::init,st in
      find_rec init

    let pseudo = List.map (fun i -> Instruction i)


(* RMW utilities *)
    let tempo1 = ARM.Symbolic_reg "T1"

    let loop_rmw r1 r2 addr =
      let lab = Label.next_label "Loop" in
      Label (lab,Nop)::
      pseudo
        [I_LDREX (r1,addr) ;
         I_STREX (tempo1,r2,addr,AL);
         I_CMPI (tempo1,0);
         I_BNE (lab);
       ]


    let unroll_rmw p r1 r2 addr u =
      if u <= 0 then
        pseudo
          [I_LDREX (r1,addr);
           I_STREX (tempo1,r2,addr,AL);]
      else if u = 1 then
        pseudo
          [I_LDREX (r1,addr);
           I_STREX (tempo1,r2,addr,AL);
           I_CMPI (tempo1,0);
           I_BNE (Label.fail p);]
      else
        let out = Label.next_label "Go" in
        let rec do_rec = function
          | 1 ->
              [I_LDREX (r1,addr);
               I_STREX (tempo1,r2,addr,AL);
               I_CMPI (tempo1,0);
               I_BNE (Label.fail p);]
          | u ->
              I_LDREX (r1,addr)::
              I_STREX (tempo1,r2,addr,AL)::
              I_CMPI (tempo1,0)::
              I_BEQ (out)::
              do_rec (u-1) in
        pseudo (do_rec u)@[Label (out,Nop)]
        



(*********)
(* loads *)
(*********)

    let emit_load st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,pseudo [I_LDR (rA,rB,AL)],st

    let emit_load_not_zero st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::pseudo [I_LDR (rA,rB,AL); I_CMPI (rA,0); I_BEQ (lab)],
      st

    let emit_load_one st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let lab = Label.next_label "L" in
      rA,init,
      Label (lab,Nop)::pseudo [I_LDR (rA,rB,AL); I_CMPI (rA,1); I_BNE (lab)],
      st

    let emit_load_not st p init x cmp =
      let rA,st = next_reg st in
      let rC,st = next_reg st in
      let rB,init,st = next_init st p init x in
      let lab = Label.next_label "L" in
      let out = Label.next_label "L" in
      rA,init,
      Instruction (I_MOVI (rC,200,AL))::
      (* 200 X about 5 ins looks for a typical memory delay *)
      Label (lab,Nop)::
      pseudo
        [
         I_LDR (rA,rB,AL); cmp rA ;
         I_BNE out; I_ADD (DontSetFlags,rC,rC,-1) ;
         I_CMPI (rC,0) ; I_BNE lab ;
       ]@
      [Label (out,Nop)],
      st
        
    let emit_load_not_eq st p init x rP =
      emit_load_not st p init x (fun r -> I_CMP (r,rP))

    let emit_load_not_value st p init x v =
      emit_load_not st p init x (fun r -> I_CMPI (r,v))
        
    let emit_load_idx st p init x idx =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,pseudo [I_LDR3 (rA,idx,rB,AL)],st

(**********)
(* Stores *)
(**********)

    let emit_store_reg st p init x rA =
      let rB,init,st = next_init st p init x in
      init,[Instruction (I_STR (rA,rB,AL))],st

    let emit_store_idx_reg st p init x idx rA =
      let rB,init,st = next_init st p init x in
      init,[Instruction (I_STR3 (rA,idx,rB,AL))],st

    let emit_store st p init x v =
      let rA,st = next_reg st in
      let init,cs,st = emit_store_reg st p init x rA in
      init,Instruction (I_MOVI (rA,v,AL))::cs,st

    let emit_store_idx st p init x idx v =
      let rA,st = next_reg st in
      let init,cs,st = emit_store_idx_reg st p init x idx rA in
      init,Instruction (I_MOVI (rA,v,AL))::cs,st

(* No FNO yet *)
    and emit_fno2 _st _p _init _x = assert false
    and emit_open_fno _st _p _init _x = assert false
    and emit_close_fno _st _p _init _lab _r _x = assert false

(* Load exclusive *)

    let tempo3 = ARM.Symbolic_reg "T3"

(* FNO *)

    let emit_ldrex st p init x =
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,pseudo [I_LDREX (rA,rB)],st

    let emit_ldrex_idx st p init x idx =      
      let rA,st = next_reg st in
      let rB,init,st = next_init st p init x in
      rA,init,
      pseudo [I_ADD3 (DontSetFlags,tempo1,idx,rB); I_LDREX (rA,tempo1)],st

    let unroll_fno u st p init rB =
      let rA,st = next_reg st in
      let cs = unroll_rmw p rA rA rB u in
      rA,init,cs,st

    let loop_fno st init rB =
      let rA,st = next_reg st in
      let cs = loop_rmw rA rA rB in
      rA,init,cs,st

    let do_emit_fno st p init rB =
      match Cfg.unrollatomic with
      | None -> loop_fno st init rB
      | Some u -> unroll_fno u  st p init rB

    let emit_fno st p init x =
      let rB,init,st = next_init st p init x in
      do_emit_fno st p init rB

    let emit_fno_idx st p init x idx =
      let rB,init,st = next_init st p init x in
      let r,init,cs,st = do_emit_fno st p init tempo1 in
      r,init,
      Instruction (I_ADD3 (DontSetFlags,tempo1,idx,rB))::cs,
      st
        
(* STA *)

    let unroll_sta u p rA rB = unroll_rmw p tempo1 rA rB u

    let loop_sta rA rB = loop_rmw tempo1 rA rB

    let do_emit_sta_reg p rA rB = match Cfg.unrollatomic with
    | None -> loop_sta rA rB
    | Some u -> unroll_sta u p rA rB

    let do_emit_sta st p init rB v =
      let cs = do_emit_sta_reg p tempo3 rB in
      init,
      Instruction (I_MOVI (tempo3,v,AL))::cs,
      st

    let emit_sta  st p init x v =
      let rB,init,st = next_init st p init x in
      do_emit_sta st p init rB v

    let emit_sta_idx st p init x idx v =
      let rB,init,st = next_init st p init x in
      let init,cs,st =  do_emit_sta st p init tempo1 v in
      init,
      Instruction (I_ADD3 (DontSetFlags,tempo1,idx,rB))::cs,
      st

    let emit_sta_reg st p init x rA =
      let rB,init,st = next_init st p init x in
      let cs = do_emit_sta_reg p rA rB in
      init,cs,st

(*************)
(* Acccesses *)
(*************)

    let emit_access  st p init e = match e.dir,e.atom with
    | R,Plain ->
        let r,init,cs,st = emit_load st p init e.loc in
        Some r,init,cs,st
    | R,Reserve ->
        let r,init,cs,st = emit_ldrex st p init e.loc  in
        Some r,init,cs,st
    | R,Atomic ->
        let r,init,cs,st = emit_fno st p init e.loc  in
        Some r,init,cs,st
    | W,Plain ->
        let init,cs,st = emit_store st p init e.loc e.v in
        None,init,cs,st
    | W,Reserve -> Warn.fatal "No store with reservation"
    | W,Atomic ->
        let init,cs,st = emit_sta st p init e.loc e.v in
        None,init,cs,st
        

    let emit_access_dep_addr st p init e  r1 =
      let r2,st = next_reg st in
      let c =  I_XOR (DontSetFlags,r2,r1,r1) in
      match e.dir,e.atom with
      | R,Plain ->
          let r,init,cs,st = emit_load_idx st p init e.loc r2 in
          Some r,init, Instruction c::cs,st
      | R,Reserve ->
          let r,init,cs,st = emit_ldrex_idx st p init e.loc r2 in
          Some r,init, Instruction c::cs,st
      | R,Atomic ->
          let r,init,cs,st = emit_fno_idx st p init e.loc r2 in
          Some r,init, Instruction c::cs,st
      | W,Plain ->
          let init,cs,st = emit_store_idx st p init e.loc r2 e.v in
          None,init,Instruction c::cs,st
      | W,Reserve -> Warn.fatal "No store with reservation"
      | W,Atomic ->
          let init,cs,st = emit_sta_idx st p init e.loc r2 e.v in
          None,init,Instruction c::cs,st

    let emit_access_dep_data st p init e  r1 =
      match e.dir with
      | R -> Warn.fatal "data dependency to load"
      | W ->
          let r2,st = next_reg st in
          let cs2 =
            [Instruction (I_XOR (DontSetFlags,r2,r1,r1)) ;
             Instruction (I_ADD (DontSetFlags,r2,r2,e.v)) ; ] in
          begin match e.atom with
          | Plain ->
              let init,cs,st = emit_store_reg st p init e.loc r2 in
              None,init,cs2@cs,st
          | Atomic ->
              let init,cs,st = emit_sta_reg st p init e.loc r2 in
              None,init,cs2@cs,st              
          | Reserve ->
               Warn.fatal "No store with reservation"
          end

    let insert_isb isb cs1 cs2 =
      if isb then cs1@[Instruction I_ISB]@cs2
      else cs1@cs2

    let emit_access_ctrl isb st p init e r1 =      
      let lab = Label.next_label "LC" in
      let c =
        [Instruction (I_CMP (r1,r1));
         Instruction (I_BNE lab);
         Label (lab,Nop);] in
      match e.dir with
      | R ->
          let r,init,cs,st = emit_load st p init e.loc in
          Some r,init,insert_isb isb c cs,st
      | W ->
          let init,cs,st = emit_store st p init e.loc e.v in
          None,init,insert_isb isb c cs,st

    let emit_access_dep st p init e dp r1 = match dp with
    | ADDR -> emit_access_dep_addr st p init e r1
    | DATA -> emit_access_dep_data st p init e r1
    | CTRL -> emit_access_ctrl false st p init e r1
    | CTRLISYNC -> emit_access_ctrl true st p init e r1

    let emit_fence f =
      Instruction
        (match f with
        | DMB o -> I_DMB o
        | DSB o -> I_DSB o
        | ISB -> I_ISB)

    let stronger_fence = DMB SY

    let does_fail p cs =
      let lab = Label.fail p in
      List.exists
        (fun i -> match i with
        | Instruction (I_B lab0|I_BNE lab0|I_BEQ lab0) ->
            (lab0:string) = lab
        | _ -> false)
        cs

    let postlude st p init cs =
    if does_fail p cs then
      let init,okcs,st = emit_store st p init Code.ok 0 in
      init,
      cs@
      Instruction (I_B (Label.exit p))::
      Label (Label.fail p,Nop)::
      okcs@
      [Label (Label.exit p,Nop)],
      st
    else init,cs,st

  end