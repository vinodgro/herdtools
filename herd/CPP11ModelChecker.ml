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

(** Check an event structure against a CPP11 model *)

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

    let failed_requires_clauses = ref 0

    let run_interpret failed_requires_clause test conc m id vb_pp kont res =
      match I.interpret failed_requires_clause test conc m id vb_pp with
      | Some st ->
          if not O.strictskip || StringSet.equal st.I.skipped O.skipchecks then
            let vb_pp = lazy (I.show_to_vbpp st) in
            kont conc conc.S.fs vb_pp (Some (!failed_requires_clauses)) res
          else res
      | None -> res

    let check_event_structure test conc kont res =
      let prb = JU.make_procrels conc in
      let pr = prb.JU.pr in
      let vb_pp = lazy (JU.vb_pp_procrels prb) in
      let evts = conc.S.str.E.events in
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
          ["id",id;
	   "unv", unv;
           "atom",conc.S.atomic_load_store;
           "asw", S.restrict E.is_mem_store_init
	     (fun x -> not (E.is_mem_store_init x)) 
	     unv ;
           "po",S.restrict E.is_mem E.is_mem conc.S.po;
           "pos", conc.S.pos;
           "po-loc", conc.S.pos;
	   "loc", E.EventRel.restrict_rel E.same_location unv;
           "addr", pr.S.addr;
           "data", pr.S.data;
           "ctrl", pr.S.ctrl;
           "ctrlisync", pr.S.ctrlisync;
           "ctrlisb", pr.S.ctrlisync;
           "rf", pr.S.rf;
           "rfe", U.ext pr.S.rf;
           "rfi", U.internal pr.S.rf;
         ] in
      let m =
        List.fold_left
          (fun m (k,v) -> StringMap.add k (lazy (I.Set (E.EventSet.filter v evts))) m)
          m
          [
           "_", (fun _ -> true);
           "R", E.is_mem_load;
           "W", E.is_mem_store;
           "M", E.is_mem;
           "P", (fun e -> not (E.is_atomic e));
           "A", E.is_atomic;
	   "I", E.is_mem_store_init;
         ] in
      let m = 
	List.fold_left
	  (fun m (k,v) -> StringMap.add k (lazy (I.Set (E.EventSet.filter (fun e -> v e.E.action) evts))) m)
	  m
	  E.Act.arch_sets in
      let failed_requires_clause () =
	let () = incr failed_requires_clauses in ()
      in
      if withco then
        let process_co co0 res =
	  let process_lo lo0 res =
            let co = S.tr co0 in
	    let lo = lo0 in
            let fr = U.make_fr conc co in
            let vb_pp =
              lazy begin
		(if S.O.PC.showfr then [("fr",fr)] else []) @
                (if StringSet.mem "co" S.O.PC.unshow 
		  then [] else [("co",co0)]) @
		(if StringSet.mem "lo" S.O.PC.unshow 
		  then [] else [("lo",lo0)]) @
		Lazy.force vb_pp
	      end in

            let m =
              List.fold_left
		(fun m (k,v) -> StringMap.add k (lazy (I.Rel v)) m)
		m
		[
		  "fr", fr; "fre", U.ext fr; "fri", U.internal fr;
		  "co", co; "coe", U.ext co; "coi", U.internal co;
		  "lo", lo; "loe", U.ext lo; "loi", U.internal lo;
		] in
            run_interpret failed_requires_clause test conc m id vb_pp kont res in
	  U.apply_process_lo test conc process_lo res in
        U.apply_process_co test conc process_co res
      else
        let co0 = conc.S.pco in
        let m =
           List.fold_left
              (fun m (k,v) -> StringMap.add k (lazy (I.Rel v)) m)
              m
              [
               "co0", co0;
             ] in
        run_interpret failed_requires_clause test conc m id vb_pp kont res
  end
