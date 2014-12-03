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
      let prb = lazy (JU.make_procrels conc) in
      let pr = lazy (Lazy.force prb).JU.pr in
      let vb_pp = lazy (JU.vb_pp_procrels (Lazy.force prb)) in
      let evts =
        E.EventSet.filter
          (fun e -> E.is_mem e || E.is_barrier e)
          conc.S.str.E.events in
      let id =
        lazy begin
          E.EventRel.of_list
            (List.rev_map
               (fun e -> e,e)
               (E.EventSet.elements evts))
        end in
      let unv = lazy begin E.EventRel.cartesian evts evts  end in
      let ks = { I.id; unv; evts;} in
(* Initial env *)
      let m =
        I.add_rels
          I.env_empty
          (["id",id;
	   "unv", unv;
            "loc", lazy begin
              E.EventRel.restrict_rel E.same_location (Lazy.force unv)
            end;
           "atom",lazy conc.S.atomic_load_store;
           "po", lazy conc.S.po;
           "pos", lazy conc.S.pos;
           "po-loc", lazy conc.S.pos;
           "addr", lazy (Lazy.force pr).S.addr;
           "data", lazy (Lazy.force pr).S.data;
           "ctrl", lazy (Lazy.force pr).S.ctrl;
           "ctrlisync", lazy (Lazy.force pr).S.ctrlisync;
           "ctrlisb", lazy (Lazy.force pr).S.ctrlisync;
           "rf", lazy (Lazy.force pr).S.rf;
           "rfe", lazy (U.ext (Lazy.force pr).S.rf);
           "rfi", lazy (U.internal (Lazy.force pr).S.rf);
(* Power fences *)
           "lwsync", (Lazy.force prb).JU.lwsync;
           "eieio", (Lazy.force prb).JU.eieio;
           "sync",  (Lazy.force prb).JU.sync;
           "isync",  (Lazy.force prb).JU.isync;
(* ARM fences *)
           "dmb", (Lazy.force prb).JU.dmb;
           "dsb", (Lazy.force prb).JU.dsb;
           "dmbst", (Lazy.force prb).JU.dmbst;
           "dmb.st", (Lazy.force prb).JU.dmbst;
           "dsbst", (Lazy.force prb).JU.dsbst;
           "dsb.st",(Lazy.force prb).JU.dsbst;
           "isb",(Lazy.force prb).JU.isb;
(* X86 fences *)
           "mfence",(Lazy.force prb).JU.mfence;
           "sfence",(Lazy.force prb).JU.sfence;
           "lfence",(Lazy.force prb).JU.lfence;
(* PTX fences *)
	   "membar.cta", (Lazy.force prb).JU.membar_cta;
	   "membar.gl", (Lazy.force prb).JU.membar_gl;
	   "membar.sys", (Lazy.force prb).JU.membar_sys;
          ] @
          (match test.Test.scope_tree with
           | None -> []
           | Some scope_tree ->
        List.fold_left (fun z (k,v) ->
            ("ext-" ^ k, lazy (U.ext_scope v (Lazy.force unv) scope_tree)) ::
            ((* "int-" ^ *) k,
             lazy (U.int_scope v (Lazy.force unv) scope_tree)) ::
            z ) [] [
          "wi", AST.Work_Item; "thread", AST.Work_Item;
          "sg", AST.Sub_Group; "warp", AST.Sub_Group;
          "wg", AST.Work_Group; "block", AST.Work_Group; "cta", AST.Work_Group;
          "kernel", AST.Kernel;
	  "dev", AST.Device; "gl", AST.Device;
	])) in
      let m =
        I.add_sets m
          (List.map
             (fun (k,p) -> k,lazy (E.EventSet.filter p evts))
          [
           "_", (fun _ -> true);
           "R", E.is_mem_load;
           "W", E.is_mem_store;
           "M", E.is_mem;
	   "B", E.is_barrier;
           "P", (fun e -> not (E.is_atomic e));
           "A", E.is_atomic;
	   "I", E.is_mem_store_init;
         ]) in
      let m =
        I.add_sets m
          (List.map
             (fun (k,a) ->
               k,lazy (E.EventSet.filter (fun e -> a e.E.action) evts))
	  E.Act.arch_sets) in
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
            I.add_rels m
              [
               "fr", lazy fr;
               "fre", lazy (U.ext fr); "fri", lazy (U.internal fr);
               "co", lazy co;
               "coe", lazy (U.ext co); "coi", lazy (U.internal co);
	     ] in
          run_interpret (fun () -> ()) test conc m ks vb_pp kont res in
        U.apply_process_co test  conc process_co res
      else
        let m = I.add_rels m ["co0",lazy  conc.S.pco] in
        run_interpret (fun () -> ()) test conc m ks vb_pp kont res
  end
