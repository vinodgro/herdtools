(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type Config = sig
  val m : AST.pp_t
  include Model.Config
end

module Make
    (O:Config)
    (S:Sem.Semantics)
    (B:AllBarrier.S with type a = S.barrier)
    =
  struct
    module I = Interpreter.Make(O)(S)(B)
    module E = S.E
    module U = MemUtils.Make(S)
    module JU = JadeUtils.Make(O)(S)(B)

    let (pp,(withco,_,prog)) = O.m

    let check_event_structure test conc kont res =
      let prb = JU.make_procrels conc in
      let pr = prb.JU.pr in
      let vb_pp = lazy (JU.vb_pp_procrels prb) in
      let evts = E.EventSet.filter E.is_mem conc.S.str.E.events in
      let id =
        E.EventRel.of_list
          (List.rev_map
             (fun e -> e,e)
             (E.EventSet.elements evts)) in
(* Initial env *)
      let m =
        List.fold_left
          (fun m (k,v) -> StringMap.add k (I.Rel v) m)
          StringMap.empty
          ["id",id;
	   "unv", E.EventRel.cartesian evts evts;
           "atom",conc.S.atomic_load_store;
           "po",S.restrict E.is_mem E.is_mem conc.S.po;
           "pos", conc.S.pos;
           "po-loc", conc.S.pos;
           "addr", pr.S.addr;
           "data", pr.S.data;
           "ctrl", pr.S.ctrl;
           "ctrlisync", pr.S.ctrlisync;
           "ctrlisb", pr.S.ctrlisync;
           "rf", pr.S.rf;
           "rfe", U.ext pr.S.rf;
           "rfi", U.internal pr.S.rf;
(* Power fences *)
           "lwsync", prb.JU.lwsync;
           "eieio", prb.JU.eieio;
           "sync", prb.JU.sync;
           "isync", prb.JU.isync;
(* ARM fences *)
           "dmb",prb.JU.dmb;
           "dsb",prb.JU.dsb;
           "dmbst",prb.JU.dmbst;
           "dmb.st",prb.JU.dmbst;
           "dsbst",prb.JU.dsbst;
           "dsb.st",prb.JU.dsbst;
           "isb",prb.JU.isb;
(* X86 fences *)
           "mfence",prb.JU.mfence;
           "sfence",prb.JU.sfence;
           "lfence",prb.JU.lfence;
         ] in
      let m =
        List.fold_left
          (fun m (k,v) -> StringMap.add k (I.Set v) m)
          m
          [
           "_", evts;
           "R", E.EventSet.filter E.is_mem_load evts;
           "W", E.EventSet.filter E.is_mem_store evts;
           "M", E.EventSet.filter E.is_mem evts;
	   "B", E.EventSet.filter E.is_barrier evts;
           "P", E.EventSet.filter (fun e -> not (E.is_atomic e)) evts;
           "A", E.EventSet.filter E.is_atomic evts;
	   "I", E.EventSet.filter E.is_mem_store_init evts;
         ] in
      let m = 
	List.fold_left
	  (fun m (k,v) -> StringMap.add k (I.Set (E.EventSet.filter (fun e -> v e.E.action) evts)) m)
	  m
	  E.Act.arch_sets in
      if withco then
        let process_co co0 res =
          let co = S.tr co0 in
          let fr = U.make_fr conc co in
          let vb_pp =
            lazy begin
              if S.O.PC.showfr then
              ("fr",fr)::("co",co0)::Lazy.force vb_pp
              else
                ("co",co0)::Lazy.force vb_pp
            end in

          let m =
            List.fold_left
              (fun m (k,v) -> StringMap.add k (I.Rel v) m)
              m
              [
               "fr", fr; "fre", U.ext fr; "fri", U.internal fr;
               "co", co; "coe", U.ext co; "coi", U.internal co;
             ] in
          I.run_interpret test conc m id vb_pp kont res in
        U.apply_process_co test  conc process_co res
      else
        let co0 = conc.S.pco in
        let m =
           List.fold_left
              (fun m (k,v) -> StringMap.add k (I.Rel v) m)
              m
              [
               "co0", co0;
             ] in
        I.run_interpret test conc m id vb_pp kont res
  end
