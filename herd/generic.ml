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

open Printf

(*
exception Requires_clause_failure of string (* not sure where to put this! *)
*)

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

(****************************)
(* Convenient abbreviations *)
(****************************)

    module S = S
    module A = S.A
    module E = S.E
    module U = MemUtils.Make(S)
    module MU = ModelUtils.Make(O)(S)
    module JU = JadeUtils.Make(O)(S)(B)
    module W = Warn.Make(O)
(*  Model interpret *)
    let (pp,(withco,_,prog)) = O.m

    type v =
      | Set of S.event_set 
      | Rel of S.event_rel
      | Clo of closure

    and env = v StringMap.t
    and closure =
        { clo_args : AST.var list ;
          clo_env : env ;
          clo_body : AST.exp; }

    let find_env env k =
      try StringMap.find k env
      with
      | Not_found -> Warn.user_error "unbound var: %s" k

    let is_rel = function
      | Rel _ -> true
      | _ -> false

    let is_set = function
      | Set _ -> true
      | _ -> false

    let as_rel = function
      | Rel r -> r
      | _ ->  Warn.user_error "relation expected"

    let as_set = function
      | Set s -> s
      | _ -> Warn.user_error "set expected"

    let as_clo = function
      | Clo c -> c
      | _ -> Warn.user_error "closure expected"


    let rec stabilised vs ws = match vs,ws with
    | [],[] -> true
    | v::vs,w::ws ->
        E.EventRel.subset w v && stabilised vs ws
    | _,_ -> assert false

    open AST

(* State of interpreter *)
    type st =
        { env : env ;
          show : S.event_rel StringMap.t Lazy.t ;
	  seen_requires_clause : bool ;
	  failed_requires_clauses : int ;
          skipped : StringSet.t ; }

  let select_event = match S.O.PC.showevents with
  | AllEvents -> (fun _ -> true)
  | MemEvents -> E.is_mem
  | NonRegEvents -> (fun e -> E.is_mem e || E.is_barrier e || E.is_commit e
  )

  let select_rel =
    E.EventRel.filter (fun (e1,e2) -> select_event e1 && select_event e2)

    let rt_loc = if O.verbose <= 1 then 
	(fun rel -> 
	  let filtered_rel = select_rel rel in
	  S.rt filtered_rel)
      else (fun x -> x)

    let show_to_vbpp st =
      StringMap.fold (fun tag v k -> (tag,v)::k)   (Lazy.force st.show) []

    let empty_rel = Rel E.EventRel.empty   
    let empty_set = Set E.EventSet.empty       
    let interpret test conc m id vb_pp =

      let is_dir = function
	| Unv_Set -> (fun _ -> true)
	| Bar_Set -> E.is_barrier
        | WriteRead -> E.is_mem
        | Write -> E.is_mem_store
        | Read -> E.is_mem_load
        | Atomic -> E.is_atomic
	| Filter s -> E.sat_filter s
        | Plain -> fun e -> not (E.is_atomic e) in

      let rec eval env = function
        | Empty_rel -> empty_rel
        | Empty_set -> empty_set
        | Scope_op (sc,External) ->  
	    Rel (U.ext_scope sc conc.S.unv test.Test.scope_tree)
        | Scope_op (sc,Internal) -> 
	    Rel (U.int_scope sc conc.S.unv test.Test.scope_tree)
        | Var k -> find_env env k
        | Cartesian (e1,e2) ->
          let v1 = eval_set env e1 in
          let v2 = eval_set env e2 in
          Rel (E.EventRel.cartesian v1 v2)
        | Fun (xs,body) ->
            Clo {clo_args=xs; clo_env=env; clo_body=body; }
        | Op1 (op,e) ->
            begin
              let v = eval env e in
              match op with
              | Plus -> Rel (S.tr (as_rel v))
              | Star -> Rel (S.union (S.tr (as_rel v)) id)
              | Opt -> Rel (S.union (as_rel v) id)
	      | Comp -> begin
                match v with
                  | Set s -> Set (E.EventSet.diff conc.S.str.E.events s)
                  | Rel r -> Rel (S.comp r conc.S.unv)
                end
	      | Inverse -> Rel (S.inverse (as_rel v))
              | Select (s1,s2) ->
                  let f1 = is_dir s1 and f2 = is_dir s2 in
                  Rel (S.restrict f1 f2 (as_rel v))
            end
        | Op (op,es) ->
            begin
              let vs = List.map (eval env) es in
              if List.for_all is_rel vs then begin
                match op with
                  | Union -> Rel (S.unions (List.map as_rel vs))
                  | Seq -> Rel (S.seqs (List.map as_rel vs))
                  | Inter ->
                    begin match vs with
                      | [] -> assert false
                      | v::vs ->
                        List.fold_left (fun v z -> 
                          Rel (E.EventRel.inter (as_rel v) (as_rel z))) v vs
                    end

		  (*TS: Not an efficient way to implement Diff, but it shouldn't be used that often*)
		  | Diff -> 
                    begin match vs with
                      | [] -> assert false
                      | v::vs ->			
                        List.fold_left (fun v z -> 
			  let comp = Rel (S.comp (as_rel z) conc.S.unv)
			  in 
                          Rel (E.EventRel.inter (as_rel v) (as_rel comp))) v vs
                    end

              (* Todo: I think Diff is missing here *)
              end else if List.for_all is_set vs then begin
                match op with
                  | Union -> Set (E.EventSet.unions (List.map as_set vs))
                  | Seq -> assert false
                  | Inter ->
                    begin match vs with
                      | [] -> assert false
                      | v::vs ->
                        List.fold_left (fun v z -> 
                          Set (E.EventSet.inter (as_set v) (as_set z))) v vs
                    end
              (* Todo: I think Diff is missing here *)
              end else 
		  begin
		    printf "Unable to operate on values of different types (set and relation)";
		    assert false
		  end

            end 
        | App (f,es) ->
            let f = eval_clo env f in
            let vs = List.map (eval env) es in
            let bds =
              try
                List.combine f.clo_args vs
              with _ -> Warn.user_error "argument_mismatch" in
            let env =
              List.fold_right
                (fun (x,v) env -> StringMap.add x v env)
                bds f.clo_env in
            eval env f.clo_body
        | Bind (bds,e) ->
            let env = eval_bds env bds in
            eval env e
        | BindRec (bds,e) ->
            let env = env_rec (fun pp -> pp) bds env in
            eval env e

      and eval_rel env e = as_rel (eval env e)
      and eval_set env e = as_set (eval env e) 
      and eval_clo env e = as_clo (eval env e)

(* For let *)
      and eval_bds env bds = match bds with
      | [] -> env
      | (k,e)::bds ->
          let v = eval env e in
          StringMap.add k v (eval_bds env bds)

(* For let rec *)
      and env_rec pp bds =
        let rec fix  k env vs =          
          if O.debug && O.verbose > 1 then begin
            let vb_pp =
              List.map2
                (fun (x,_) v -> x,rt_loc v)
                bds vs in
            let vb_pp = pp vb_pp in
            MU.pp_failure test conc
              (sprintf "Fix %i" k)
              vb_pp
          end ;
          let env,ws = fix_step env bds in
          if stabilised vs ws then env
          else fix (k+1) env ws in
        fun env ->
          fix 0
            (List.fold_left
               (fun env (k,_) -> StringMap.add k empty_rel env)
               env bds)
            (List.map (fun _ -> E.EventRel.empty) bds)

      and fix_step env bds = match bds with
      | [] -> env,[]
      | (k,e)::bds ->
          let v = eval env e in
          let env = StringMap.add k v env in
          let env,vs = fix_step env bds in
          env,(as_rel v::vs) in

(* Showing bound variables, (-doshow option) *)

      let doshow bds st =
        let to_show =
          StringSet.inter S.O.PC.doshow (StringSet.of_list (List.map fst bds)) in
        if StringSet.is_empty to_show then st
        else
          let show = lazy begin
            (* JPW: This might be wrong now that x could represent a set. *)
            StringSet.fold
              (fun x show  ->
                StringMap.add x (rt_loc (eval_rel st.env (Var x))) show)
              to_show
              (Lazy.force st.show)
          end in
          { st with show;} in
        
(* Execute one instruction *)

      let rec exec st i c =  match i with
      | Show xs ->
          let show = lazy begin              
            List.fold_left
              (fun show x ->
                StringMap.add x (rt_loc (eval_rel st.env (Var x))) show)
              (Lazy.force st.show) xs
          end in
          run { st with show;} c
      | UnShow xs ->
          let show = lazy begin
            List.fold_left
              (fun show x -> StringMap.remove x show)
              (Lazy.force st.show) xs
          end in
          run { st with show;} c
      | ShowAs (e,id) ->
          let show = lazy begin
            StringMap.add id
              (rt_loc (eval_rel st.env e)) (Lazy.force st.show)
          end in
          run { st with show; } c
      | Test (pos,t,e,name,test_type) ->
	 if st.seen_requires_clause && test_type = Provides then 
	   begin
	     let pp = String.sub pp pos.pos pos.len in
	     Warn.user_error 
	       "A provides-clause must not come after a requires-clause. Culprit: '%s'." pp
	   end;
	 let st = {st with seen_requires_clause = 
			     (test_type = Requires) || st.seen_requires_clause;} in
          let skip_this_check =
            match name with
            | Some name -> StringSet.mem name O.skipchecks
            | None -> false in
          if
            O.strictskip || not skip_this_check
          then
            let v = eval_rel st.env e in
            let pred = match t with
            | Acyclic -> E.EventRel.is_acyclic
            | Irreflexive -> E.EventRel.is_irreflexive
            | TestEmpty -> E.EventRel.is_empty in
            let ok = pred v in
            let ok = MU.check_through ok in
            if ok then run st c
            else if skip_this_check then begin
              assert O.strictskip ;
              run 
                { st with
                  skipped = StringSet.add (Misc.as_some name) st.skipped;}
                c
            end 
            else begin
              if (O.debug && O.verbose > 0) then begin
                let pp = String.sub pp pos.pos pos.len in
                MU.pp_failure test conc
                  (sprintf "%s: Failure of '%s'" test.Test.name.Name.name pp)
                  (show_to_vbpp st)
              end ;
	      match test_type with
	      | Provides -> None
	      | Requires ->
		 let st = {st with failed_requires_clauses =
		   st.failed_requires_clauses + 1;} in
		 run st c		     
            end
          else begin
            W.warn "Skipping check %s" (Misc.as_some name) ;
            run st c
          end
      | Let bds -> 
          let env = eval_bds st.env bds in
          let st = { st with env; } in
          let st = doshow bds st in
          run st c
      | Rec bds ->
          let env =
            env_rec
              (fun pp -> pp@show_to_vbpp st)
              bds st.env in
          let st = { st with env; } in
          let st = doshow bds st in
          run st c

      and run st = function
        | [] ->  Some st 
        | i::c -> exec st i c in

      let show =
        lazy begin
          List.fold_left
            (fun show (tag,v) -> StringMap.add tag v show)
            StringMap.empty (Lazy.force vb_pp)
        end in
      run {env=m; show=show; 
	   seen_requires_clause=false; 
	   failed_requires_clauses=0;
	   skipped=StringSet.empty;} 
	  prog
        
    let check_event_structure test atrb conc kont res =
      let prb = JU.make_procrels conc in
      let pr = prb.JU.pr in
      let vb_pp = lazy (JU.vb_pp_procrels prb) in
      let evts = (* E.EventSet.filter E.is_mem *) conc.S.str.E.events in
      let id =
        E.EventRel.of_list
          (List.rev_map
             (fun e -> e,e)
             (E.EventSet.elements evts)) in

      let arch_vars = List.map (fun x -> 
        (x, E.EventSet.filter (E.sat_filter_single x) conc.S.str.E.events)
      ) atrb in

(* Initial env *)
      let m =
        List.fold_left
          (fun m (k,v) -> StringMap.add k (Rel v) m)
          StringMap.empty
          ["id",id;
           "atom",conc.S.atomic_load_store;
	   (*TS: lifting the restriction from po, for C++11 model*)
           (*"po",S.restrict E.is_mem E.is_mem conc.S.po;*)
           "po", conc.S.po;
           "asw", S.restrict E.is_mem_store_init
	     (fun x -> not (E.is_mem_store_init x)) 
	     conc.S.unv ;
           "unv", conc.S.unv;
           "pos", conc.S.pos;
           "po-loc", conc.S.pos; (* is this right? *)
           "loc", S.restrict_rel E.same_location conc.S.unv;
           "addr", pr.S.addr;
           "data", pr.S.data;
           "ctrl", pr.S.ctrl;
           "ctrlisync", pr.S.ctrlisync;
           "ctrlisb", pr.S.ctrlisync;
           "rf", pr.S.rf;
           "rfe", U.ext pr.S.rf;
           "rfi", U.internal pr.S.rf;
           "ext", U.ext conc.S.unv;
           "int", U.internal conc.S.unv;
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
         ] in

      let m =
        List.fold_left
          (fun m (k,v) -> StringMap.add k (Set v) m)
          m
          (arch_vars @ [
           "_", evts;
           "R", E.EventSet.filter E.is_mem_load evts;
           "W", E.EventSet.filter E.is_mem_store evts;
           "M", E.EventSet.filter E.is_mem evts;
	   "B", E.EventSet.filter E.is_barrier evts;
           "P", E.EventSet.filter (fun e -> not (E.is_atomic e)) evts;
           "A", E.EventSet.filter E.is_atomic evts;
	   "I", E.EventSet.filter E.is_mem_store_init evts;
(* C++ RMW and lock/unlock actions *)
           "rmw", E.EventSet.filter E.is_rmw evts;
           "ls", E.EventSet.filter E.is_successful_lock evts;
           "lk", E.EventSet.filter E.is_lock evts;
           "ul", E.EventSet.filter E.is_unlock evts;
         ]) in

      let process_co_and_lo co0 res =
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
            (fun m (k,v) -> StringMap.add k (Rel v) m)
            m
            [
             "fr", fr; "fre", U.ext fr; "fri", U.internal fr;
             "co", co; "coe", U.ext co; "coi", U.internal co;
             "lo", E.EventRel.empty;
           ] in

        match interpret test conc m id vb_pp with
        | Some st ->
            if not O.strictskip || StringSet.equal st.skipped O.skipchecks then
              let vb_pp = lazy (show_to_vbpp st) in
              kont conc (conc.S.fs, st.failed_requires_clauses) vb_pp res
            else res
        | None -> res in
      U.apply_process_co_and_lo test  conc process_co_and_lo res
  end
