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

(** Check an event structure against a machine model *)

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

    let (pp,(opts,_,prog)) = O.m
    let withco = opts.ModelOption.co

    let run_interpret failed_requires_clause test conc m id vb_pp kont res =
      match I.interpret failed_requires_clause test conc m id vb_pp with
      | Some st ->
          if not O.strictskip || StringSet.equal st.I.skipped O.skipchecks then
            let vb_pp = lazy (I.show_to_vbpp st) in
            kont conc conc.S.fs vb_pp None res
          else res
      | None -> res

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
      let unv = E.EventRel.cartesian evts evts in
(* Initial env *)
      let m =
        List.fold_left
          (fun m (k,v) -> StringMap.add k (lazy (I.Rel v)) m)
          StringMap.empty
          (["id",id;
	   "unv", unv;
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
(* PTX fences *)
	   "membar.cta", prb.JU.membar_cta;
	   "membar.gl", prb.JU.membar_gl;
	   "membar.sys", prb.JU.membar_sys;
          ] @ 
          (match test.Test.scope_tree with
           | None -> []
           | Some scope_tree ->
        List.fold_left (fun z (k,v) ->
            ("ext-" ^ k, U.ext_scope v unv scope_tree) :: 
            ("int-" ^ k, U.int_scope v unv scope_tree) :: 
            z ) [] [ 
          "wi", AST.Work_Item; 
          "thread", AST.Work_Item;
          "sg", AST.Sub_Group; "warp", AST.Sub_Group;
          "wg", AST.Work_Group; 
          "block", AST.Work_Group; 
          "cta", AST.Work_Group;
	  "kernel", AST.Kernel;
	  "dev", AST.Device; 
	])) in
      let m =
        List.fold_left
          (fun m (k,v) -> StringMap.add k (lazy (I.Set (E.EventSet.filter v evts))) m)
          m
          [
           "_", (fun _ -> true);
           "R", E.is_mem_load;
           "W", E.is_mem_store;
           "M", E.is_mem;
	   "B", E.is_barrier;
           "P", (fun e -> not (E.is_atomic e));
           "A", E.is_atomic;
	   "I", E.is_mem_store_init;
         ] in
      let m = 
	List.fold_left
	  (fun m (k,v) -> StringMap.add k (lazy (I.Set (E.EventSet.filter (fun e -> v e.E.action) evts))) m)
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
              (fun m (k,v) -> StringMap.add k (lazy (I.Rel v)) m)
              m
              [
               "fr", fr; "fre", U.ext fr; "fri", U.internal fr;
               "co", co; "coe", U.ext co; "coi", U.internal co;
             ] in
          run_interpret (fun () -> ()) test conc m id vb_pp kont res in
        U.apply_process_co test  conc process_co res
      else
        let co0 = conc.S.pco in
        let m =
           List.fold_left
              (fun m (k,v) -> StringMap.add k (lazy (I.Rel v)) m)
              m
              [
               "co0", co0;
             ] in
        run_interpret (fun () -> ()) test conc m id vb_pp kont res
  end
