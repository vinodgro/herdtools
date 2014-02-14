(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf
(* Memory order *)
  type mem_order =
    | Acq
    | Rel
    | Acq_Rel
    | SC
    | Rlx
    | Con

  let pp_mem_order o = match o with
  | Acq -> "memory_order_acquire"
  | Rel -> "memory_order_release"
  | Acq_Rel -> "memory_order_acq_rel"
  | SC -> "memory_order_seq_cst"
  | Rlx -> "memory_order_relaxed"
  | Con -> "memory_order_consume"

  let pp_mem_order_short = function
    | Acq -> "Acq"
    | Rel -> "Rel"
    | Acq_Rel -> "AcqRel"
    | SC -> "SC"
    | Rlx -> "Rlx"
    | Con -> "Con"

  let sig_of_mem_order = function
    | Acq -> 'D'
    | Rel -> 'E'
    | Acq_Rel -> 'F'
    | SC -> 'G'
    | Rlx -> 'H'
    | Con -> 'I'

(* Atoms *)
  open Code
  type atom = mem_order
  let default_atom = SC
  let applies_atom a d = match a,d with
  | (Acq|Acq_Rel|Con),Dir W -> false
  | (Rel|Acq_Rel),Dir R -> false
  | _,_ -> true

  let compare_atom = Pervasives.compare
  let sig_of_atom = sig_of_mem_order
  let pp_atom = function
    | Acq -> "Acq"
    | Rel -> "Rel"
    | Acq_Rel -> "AR"
    | SC -> "A"
    | Rlx -> "Rlx"
    | Con -> "Con"

  let fold_atom f k =
    let k = f Acq k in
    let k = f Rel k in
    let k = f Acq_Rel k in
    let k = f SC k in
    let k = f Rlx k in
    let k = f Con k in
    k
      
(* Fences, to be completed *)
  type fence = mem_order

  let compare_fence = Pervasives.compare

  let strong = SC

  let pp_fence f = sprintf "Fence%s" (pp_mem_order_short f)


  let sig_of_fence f = sig_of_mem_order f

  let do_fold_fence f k =
    let k = f Acq k in
    let k = f Rel k in
    let k = f Acq_Rel k in
    let k = f SC k in
    let k = f Rlx k in
    let k = f Con k in
    k

  let fold_cumul_fences = do_fold_fence
  let fold_all_fences =  do_fold_fence
  let fold_some_fences =  do_fold_fence

  let orders _f _d1 _d2 = true

(* Basic C arch *)
  type arch_reg = { id:int }

  let dump_reg r = sprintf "r%i" r.id

  type location =
    | Loc of Code.loc
    | Reg of Code.proc * arch_reg

  let dump_loc = function
    | Loc loc -> loc
    | Reg (p,r) -> sprintf "%i:%s" p (dump_reg r)

  let pp_location = dump_loc

  let location_compare = Pervasives.compare

  let of_reg p r = Reg (p,r)
  let of_loc loc = Loc loc

  type tbase = TypBase.t

  let dump_tbase t =
    let open TypBase in
    match t with
    | Int -> "int"
    | Short -> "short"
    | Char -> "char"

  type typ = Plain of tbase | Atomic of tbase

  let dump_typ = function
    | Plain t -> dump_tbase t
    | Atomic t -> sprintf "atomic_%s" (dump_tbase t)

  type exp =
    | Load of location
    | AtomicLoad of mem_order * location
    | Deref of exp
    | Const of Code.v

  let addrs_of_location = function
    | Reg _ -> StringSet.empty
    | Loc loc -> StringSet.singleton loc

  let rec addrs_of_exp = function
    | Const _ -> StringSet.empty
    | AtomicLoad (_,loc)|Load loc -> addrs_of_location loc
    | Deref e -> addrs_of_exp e

  type cond = Eq | Ne

  type ins =
    | Seq of ins * ins
    | Decl of typ * arch_reg * exp option
    | Store of Code.loc * exp
    | SetReg of arch_reg * exp
    | AtomicStore of mem_order * Code.loc * exp
    | Fence of fence
    | Loop of ins
    | BreakCond of cond * arch_reg * exp
    | Decr of arch_reg
    | Nop

  let rec addrs_of = function
    | Fence _ | Decr _ | Nop | Decl (_,_,None) -> StringSet.empty
    | Seq (i1,i2) -> StringSet.union (addrs_of i1) (addrs_of i2)
    | Decl (_,_,Some e)
    | SetReg (_,e)|BreakCond (_,_,e) -> addrs_of_exp e
    | Store (loc,e)|AtomicStore (_,loc,e) -> StringSet.add loc (addrs_of_exp e)
    | Loop i -> addrs_of i

  let seq i1 i2 = match i1,i2 with
  | (Nop,i)|(i,Nop) -> i
  | _,_ -> Seq (i1,i2)

  let seqs is = List.fold_right seq is Nop

  let rec is_nop = function
    | Nop|Decl (_,_,None) -> true
    | Seq (i1,i2) -> is_nop i1 && is_nop i2
    | _ -> false

(* No dependencies *)
  type dp

  let pp_dp _ = assert false
  let sig_of_dp _ = assert false
  let fold_dpr  _f k = k
  let fold_dpw _f k = k

  let ddr_default = None
  let ddw_default = None
  let ctrlr_default = None
  let ctrlw_default = None

  let is_ctrlr _ = assert false
  let fst_dp _ = assert false
  let sequence_dp _ _ = assert false

