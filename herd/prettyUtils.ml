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


module Make(S : SemExtra.S) = struct
  module E = S.E


  module IntMap =
    Map.Make
      (struct
        type t = int
        let compare = Misc.int_compare
      end)

  let int_map_find k m =
    try IntMap.find k m
    with Not_found -> []
        
  let progorder_as_list es  =
    let by_po =
      E.EventSet.fold
        (fun e k ->
          let poi = E.progorder_of e in
          let es_poi = int_map_find  poi k in
          IntMap.add poi (e::es_poi) k)
        es IntMap.empty in
    let as_list =
      IntMap.fold
        (fun _ es k -> es::k)
        by_po [] in
    List.rev_map E.EventSet.of_list as_list

  let make_by_proc_and_poi es =
    let by_proc = E.proj_events es in
    List.map progorder_as_list by_proc

  let observed test es =
    let locs = S.outcome_locations test in
    let xss = make_by_proc_and_poi es in
    let xss = Misc.mapi (fun i x -> i,x) xss in
    let _,obs =
      List.fold_right
        (fun (i,ess) (locs,obs) ->
          let rec find_rec = function
            | [] -> locs,obs
            | es::rem ->
                let locs,obs as r = find_rec rem in
                let wr = (* significant written registers *)
                  E.EventSet.fold
                    (fun e k ->
                      if E.is_reg_store e i then
                        let rloc =  Misc.as_some (E.location_of e) in
                        if S.LocSet.mem rloc locs then
                          S.LocSet.add rloc k
                        else k
                      else k)
                    es S.LocSet.empty in
                if S.LocSet.is_empty wr then r
                else
                  let locs = S.LocSet.diff locs wr
                  and obs =
                    let read = E.EventSet.filter E.is_mem_load es in
                    E.EventSet.union obs read in
                  locs,obs in
          find_rec ess)
        xss (locs,E.EventSet.empty) in
    obs

(* All registers that read memory *)

  let all_regs_that_read es =
    let xss = make_by_proc_and_poi es in
    let xss = Misc.mapi (fun i x -> i,x) xss in
    let locs =
      List.fold_right
        (fun (i,ess) locs ->
          let rec find_rec = function
            | [] -> locs
            | es::rem ->
                let locs = find_rec rem in
                let wr = (* significant written registers *)
                  if E.EventSet.exists E.is_mem_load es then
                    E.EventSet.fold
                      (fun e k ->
                        if E.is_reg_store e i then
                          let rloc =  Misc.as_some (E.location_of e) in
                          S.LocSet.add rloc k
                        else k)
                      es S.LocSet.empty
                  else S.LocSet.empty in
                S.LocSet.union locs wr in
          find_rec ess)
        xss (S.LocSet.empty) in
    locs

end
