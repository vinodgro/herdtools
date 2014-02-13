(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* 'C' compiler *)

(* Compared with hardware archs, XXXCompile and top are merged... *)

open Printf
open Code

module type Config = sig
  include Top.Config
  include Cycle.Config
  val typ : TypBase.t
end

module Make(O:Config) : Builder.S
    = struct
      module A = struct
        include CArch
        let deftype = O.typ
      end
      module E = Edge.Make(A)
      module R = Relax.Make(A)(E)
      module C = Cycle.Make(O)(E)

      module AR = struct
        module A = A
        module E = E
        module R = R
        module C = C
      end

      module U = TopUtils.Make(O)(AR)
      module F = Final.Make(O)(AR)

(******************************************)
(* Compile cycle, ie generate test proper *)
(******************************************)

(* Typing *)
      let type_event env e =
        let loc = e.C.loc in
        let ty = match e.C.atom with
        | Some _ -> A.Atomic A.deftype
        | None  -> A.Plain A.deftype in
        try
          let ty0 = StringMap.find loc env in
          if ty0 <> ty then
            Warn.fatal "Type mismatch on location %s" loc ;
          env
        with
        | Not_found -> StringMap.add loc ty env

      let type_cycle n =
        let rec do_rec env m =
          let env = type_event env m.C.evt in
          if m.C.next == n then env
          else do_rec env m.C.next in
        do_rec StringMap.empty n

(* Basics *)
      let st0 = 0

      let alloc_reg _p st = { A.id=st;},st+1

      let compile_store e = 
        let v = e.C.v in
        let loc = e.C.loc in
        match e.C.atom with
        | None ->
            A.Store (loc,A.Const v)
        | Some a ->
            A.AtomicStore(a,loc,A.Const v)

      let load_from mo loc = match mo with
      | None -> A.Deref (A.Load loc)
      | Some mo -> A.AtomicLoad (mo,loc)

      let compile_load st p mo loc =
        let r,st = alloc_reg p st in
        let i = match mo with
        | None ->
            A.Decl (A.Plain A.deftype,r,Some (load_from None loc))
        | Some _ ->
            A.Decl (A.Atomic A.deftype,r,Some (load_from mo loc)) in
        r,i,st


      let compile_load_not_zero st p mo x =
        let r,st = alloc_reg p st in
        let decls =
          A.Decl
            (begin match mo with
            | None -> A.Plain A.deftype
            | Some _ -> A.Atomic A.deftype
            end,r,None)
        and body =
          A.Seq
            (A.SetReg (r,load_from mo x),
             A.BreakCond (A.Ne,r,A.Const 0)) in
        r,A.Seq (decls,A.Loop body),st

      let compile_load_one st p mo x =
        let r,st = alloc_reg p st in
        let decls =
          A.Decl
            (begin match mo with
            | None -> A.Plain A.deftype
            | Some _ -> A.Atomic A.deftype
            end,r,None)
        and body =
          A.Seq
            (A.SetReg (r,load_from mo x),
             A.BreakCond (A.Eq,r,A.Const 1)) in
        r,A.Seq (decls,A.Loop body),st

          
      let do_compile_load_not st p mo x e =
        let idx,st = alloc_reg p st in
        let r,st = alloc_reg p st in
        let decls =
          A.Seq
            (A.Decl (A.Plain TypBase.Int,idx,Some (A.Const 200)),
             A.Decl
               (begin match mo with
               | None -> A.Plain A.deftype
               | Some _ -> A.Atomic A.deftype
               end,r,None))
        and body =
          A.seqs
            [A.SetReg (r,load_from mo x) ;
             A.BreakCond (A.Ne,r,e) ;
             A.Decr idx ;
             A.BreakCond (A.Eq,idx,A.Const 0);] in
        r,A.Seq (decls,A.Loop body),st
          
      let compile_load_not_value st p mo x v =
        do_compile_load_not st p mo x (A.Const v)

      let compile_load_not_eq st p mo x r =
        do_compile_load_not st p mo x (A.Load (A.Reg (p,r)))

      let compile_access st p n =
        let e = n.C.evt in
        match e.C.dir with
        | R ->
            let loc = A.Loc e.C.loc in
            let mo = e.C.atom in
            let r,i,st =
              (if U.do_poll n then compile_load_one else compile_load)
                st p mo loc in
            Some r,i,st
        | W ->
            let i = compile_store e in
            None,i,st



(*************)
(* Observers *)
(*************)

      let rec straight_observer st p mo x = function
        | [] -> A.Nop,[]
        | v::vs ->
            let r,c,st = compile_load st p mo x in
            let cs,fs = straight_observer st p  mo x vs in
            A.seq c cs,F.add_final_v p r v fs

      let rec fenced_observer st p mo x = function
        | [] -> A.Nop,[]
        | v::vs ->
            let r,c,st = compile_load st p mo x in
            let cs,fs = fenced_observer st p  mo x vs in
            (match cs with
            | A.Nop -> c
            | _ -> A.seqs [c;A.Fence A.strong;cs]),
            F.add_final_v p r v fs

      let loop_observer st p mo x = function
        | []|[_] -> A.Nop,[]    
        | v::vs ->
            let r,c,st = compile_load_not_zero st p mo x in
            let rec do_loop st prev_r = function
              | [] ->  assert false
              | [v] ->
                  let r,c,_st = compile_load_not_eq st p mo x prev_r in
                  c,F.add_final_v p r v []
              | v::vs ->
                  let r,c,st = compile_load_not_eq st p mo x prev_r in
                  let cs,fs = do_loop st r vs in
                  A.seq c cs,F.add_final_v p r v fs in
            let cs,fs = do_loop st r vs in
            A.seq c cs,F.add_final_v p r v fs

      let rec split_last = function
        | [] -> assert false
        | [v] -> [],v 
        | v::vs ->
            let vs,w = split_last vs in
            v::vs,w

      let rec do_opt_coherence k obs = function
        | [] -> [k]
        | (v,vobs)::co ->
            let i = Ints.inter obs vobs in
            if Ints.is_empty i then begin
              k::
              do_opt_coherence (Ints.singleton v) vobs co
            end else
              do_opt_coherence (Ints.add v k) vobs co

      let opt_coherence = function
        | [] -> assert false
        | (v,obs)::co ->
            do_opt_coherence (Ints.singleton v) obs co

      let min_set =
        if O.coherence_decreasing then Ints.max_elt
        else Ints.min_elt

      let max_set =
        if O.coherence_decreasing then Ints.min_elt
        else Ints.max_elt

      let min_max xs =
        let ps = List.map (fun x -> min_set x, max_set x) xs in
        match ps with
        | []|[_] -> []
        | (_,x)::rem ->
            let rec remove_last = function
              | [] -> assert false
              | [x,_] -> [x]
              | (x,y)::rem ->
                  if x=y then x::remove_last rem
                  else x::y::remove_last rem in
            List.map Ints.singleton (x::remove_last rem)

              

      exception NoObserver

      let build_observer st p mo x vs =
        let vs,f =
          if O.optcoherence && O.obs_type <> Config.Loop then
            let vs = opt_coherence vs in
            if O.verbose > 1 then begin
              eprintf "OPT:" ;
              List.iter
                (fun vs ->
                  eprintf " {%s}" (Ints.pp_str "," (sprintf "%i") vs))        
                vs ;
              eprintf "\n%!"
            end ;
            match vs with
            | []|[_] -> raise NoObserver
            | _ ->
                if
                  List.for_all
                    (fun x ->
                      match Ints.as_singleton x with
                      | Some _ -> true | None -> false)
                    vs then
                  let ws,w = split_last vs in
                  (match ws with [_] -> [] | _ -> ws),[A.Loc x, w]
                else
                  min_max vs,[]
          else
            let vs = List.map (fun (v,_obs) -> Ints.singleton v) vs in      
            vs,[] in
        let cs,fs =
          let open Config in
          let x = A.Loc x in
          match O.obs_type with
          | Straight ->  straight_observer st p  mo x vs
          | Config.Fenced -> fenced_observer st p  mo x vs
          | Loop -> loop_observer st p mo x vs in
        cs,fs@f

      let rec build_observers p mo x arg =
        let open Config in
        match arg,O.do_observers with
        | [],_ -> [],[]
        | []::vss,_
        | [_]::vss,(Avoid|Accept) ->
            build_observers p mo x vss
        | vs::vss,_ ->
            try 
              let c,f = build_observer st0 p mo x vs in
              let is_nop = A.is_nop c in
              begin match is_nop,O.do_observers with
              | false,Avoid  -> Warn.fatal "Observer"
              | _,_ -> ()
              end ;
              if is_nop then
                  let cs,fs = build_observers p mo x vss in
                  cs,f@fs
              else
                let cs,fs = build_observers (p+1) mo x vss in
                c::cs,f@fs
            with NoObserver -> build_observers p mo x vss
                
      let rec check_rec env p =
        let add_look_loc loc v k =
          if O.optcond then k else (A.Loc loc,Ints.singleton v)::k in
        let open Config in
        function
          | [] -> [],[]
          | (x,vs)::xvs ->
              let mo =
                try match StringMap.find x env with
                | A.Plain _ -> None
                | A.Atomic _ -> Some A.Rlx                  
                with
                | Not_found -> assert false in
              let c,f = match O.cond with
              | Observe ->
                  let vs = List.flatten vs in
                  begin match vs with
                  | [] -> [],[]
                  | _::_ ->
                      let v,_ = Misc.last vs in
                      [],[A.Loc x,Ints.singleton v]
                  end
              | Unicond -> assert false
              | Cycle -> begin
                  match vs with
                  | []|[[_]] -> [],[]
                  | [[_;(v,_)]] ->
                      begin match O.do_observers with
                      | Local -> [],add_look_loc x v []
                      | Avoid|Accept -> [],[A.Loc x,Ints.singleton v]
                      | Enforce ->  
                          let c,f = build_observers p mo x vs in
                          c,add_look_loc x v f
                      end
                  | _ ->
                      let v =
                        let v,_ = Misc.last (List.flatten vs) in
                        v in
                      begin match O.do_observers with
                      | Local -> [],add_look_loc x v []
                      | _ ->
                          let c,f = build_observers p mo x vs in
                          c,add_look_loc x v f
                      end
              end in
              let cs,fs =
                check_rec env (p+List.length c) xvs in
              c@cs,f@fs

      let check_writes p cos = check_rec p cos

(* Local check of coherence *)

      let do_add_load st p f mo x v =
        let r,c,st = compile_load st p mo x in
        c,F.add_final_v p r (Ints.singleton v) f,st

      let do_add_loop st p f mo x v w =
        let r,c,st = compile_load_not_value st p mo x v in
        c,F.add_final_v p r (Ints.singleton w) f,st


      let do_observe_local st p code f mo x prev_v v =
        let open Config in
        match O.obs_type with
        | Straight ->
            let c,f,st = do_add_load st p f mo x v in
            A.Seq (code,c),f,st
        | Config.Fenced ->
            let c,f,st = do_add_load st p f mo x v in
            A.Seq (code,A.Seq (A.Fence A.strong,c)),f,st
        | Loop ->
            let c,f,st = do_add_loop st p f mo x prev_v v in
            A.Seq (code,c),f,st

      let add_co_local_check lsts ns st p code f =
        let lst = Misc.last ns in
        if U.check_here lst then
          let elst = lst.C.evt in
          let x = elst.C.loc and v = lst.C.next.C.evt.C.v
          and prev_v = elst.C.v in
          let all_lst =
            try StringMap.find x lsts
            with Not_found -> C.evt_null in
          if C.OrderedEvent.compare all_lst lst.C.next.C.evt = 0
          then
            code,(A.Loc x,Ints.singleton v)::f,st
          else
            let mo = elst.C.atom in
            do_observe_local st p code f mo (A.Loc x) prev_v v
        else
          code,f,st

(* Detours *)

      let do_observe_local_before st p code f mo x prev_v v =
        if O.optcoherence && v = 0 then
          code,[],st
        else
          let x = A.Loc x in
          let open Config in
          match O.obs_type with
          | Straight|Config.Fenced ->
              let c,f,st = do_add_load st p f mo x v in
              A.Seq (code,c),f,st
          | Loop ->
              let c,f,st = do_add_loop st p f mo x prev_v v in
              A.Seq (code,c),f,st
                

      let build_detour lsts st p n =
        let open Config in
        let c0,f,st = match O.do_observers with
        | Local ->
            let e = n.C.evt in
            let mo = e.C.atom in
            begin match n.C.edge.E.edge with
            | E.DetourWs (Dir W) ->          
                do_observe_local_before st p A.Nop [] mo n.C.evt.C.loc
                  n.C.prev.C.prev.C.evt.C.v n.C.prev.C.evt.C.v
            | E.DetourWs (Dir R) ->          
                do_observe_local_before st p A.Nop [] mo n.C.evt.C.loc
                  n.C.prev.C.prev.C.evt.C.v n.C.prev.C.evt.C.v
            | _ -> A.Nop,[],st 
            end
        | _ -> A.Nop,[],st in

        let _,c,st = compile_access st p n in
        let c = A.seq c0 c in
        match O.do_observers with
        | Local ->
            let c,f,_st = add_co_local_check lsts [n] st p c f in
            c,f
        | _ -> c,f

      let rec build_detours lsts p ns = match ns with
      | [] -> [],[]
      | n::ns ->
          let c,f = build_detour lsts st0 p n in
          let cs,fs = build_detours lsts (p+1) ns in
          c::cs,f@fs

(* zyva *)
      let rec compile_stores ns k = match ns with
      | [] -> k
      | n::ns ->
          let sto = n.C.store in
          if sto == C.nil then
            compile_stores ns k
          else
            let i = compile_store sto.C.evt in
            let k = compile_stores ns k in
            A.Seq (i,k)

      let rec compile_proc loc_writes st p ns = match ns with
      | [] -> A.Nop,(C.EventMap.empty,[]),st
      | n::ns ->
          let o,i,st = compile_access st p n in
          let is,fs,st = compile_proc loc_writes st p ns in
          A.Seq
            (i,
             match n.C.edge.E.edge with
             | E.Fenced (fe,_,_,_) -> A.Seq (A.Fence fe,is)
             | _ -> is),
          (if StringSet.mem n.C.evt.C.loc loc_writes && not (U.do_poll n) then
            F.add_final p o n fs
          else fs),
          st

      let add_args env prog =
        List.map
          (fun i ->
            let locs = A.addrs_of i in
            let args =
              StringSet.fold
                (fun x k ->
                  let t =
                    try StringMap.find x env
                    with Not_found -> assert false in
                  (t,x)::k)
                locs [] in
            args,i)
          prog
              
        
      let compile_cycle ok n =
        let env = type_cycle n in
        let open Config in
        let splitted =  C.split_procs n in
        (* Split before, as  proc numbers added by side effet.. *)
        let cos0 = C.coherence n in    
        let lsts = U.last_map cos0 in
        let cos = U.compute_cos cos0 in
        if O.verbose > 1 then U.pp_coherence cos0 ;
        let loc_writes = U.comp_loc_writes n in

        let rec do_rec p = function
          | [] -> [],(C.EventMap.empty,[]),[]
          | n::ns ->
              let c,(m,f),st = compile_proc loc_writes st0 p n in
              let c = compile_stores n c in
              let c,f,_st =
                match O.cond with
                | Unicond -> c,f,st                
                | Cycle|Observe ->
                    match O.do_observers with
                    | Local -> add_co_local_check lsts n st p c f
                    | Avoid|Accept|Enforce -> c,f,st in
              let ds = C.get_detours_from_list n in
              let cds,fds = build_detours lsts (p+1) ds in
              let cs,(ms,fs),ios = do_rec (p+1+List.length cds) ns in
              let io = U.io_of_thread n in
              let iod = List.map U.io_of_detour ds in
              c::(cds@cs),(C.union_map m ms,f@fds@fs),(io::iod)@ios in

        let obsc,f =
          match O.cond with
          | Unicond -> [],[]
          | Cycle|Observe -> check_writes env 0 cos in
        match splitted,O.cond with
        | [],_ -> Warn.fatal "No proc"
(*    | [_],Cycle -> Warn.fatal "One proc" *)
        | _,_ ->
            let c,(m,f),ios =
              if
                let len =  List.length splitted in
                O.nprocs <= 0 ||
                (if O.eprocs then len = O.nprocs else len <= O.nprocs)
              then
                let ess = List.map (List.map (fun n -> n.C.edge)) splitted in
                if ok ess then
                  let cs,(m,fs),ios = do_rec (List.length obsc) splitted in
                  obsc@cs,(m,f@fs),ios
                else Warn.fatal "Last minute check"
              else  Warn.fatal "Too many procs" in
            let f =
              match O.cond with
              | Unicond ->
                  let evts = 
                    List.map
                      (List.map (fun n -> n.C.evt))
                      splitted in
                  F.run evts m
              | Cycle -> F.check f
              | Observe -> F.observe f in
            (add_args env c,f),
            (U.compile_prefetch_ios (List.length obsc) ios,
             U.compile_coms splitted),
            env


(********)
(* Dump *)
(********)

      type args = (A.typ * Code.loc) list

      let dump_args args =
        let pp =
          List.map
            (fun (t,loc) -> match t with
            | A.Plain _ ->
                sprintf "volatile %s* %s" (A.dump_typ t) loc
            | A.Atomic _ ->
                sprintf "%s* %s" (A.dump_typ t) loc)
            args in
        String.concat "," pp


      let dump_mem_order = A.pp_mem_order

      let dump_loc_exp = function
        | A.Loc loc -> loc
        | A.Reg (_,r) -> A.dump_reg r

      let rec dump_exp e =
        let open A in
        match e with
        | Load loc -> dump_loc_exp loc
        | AtomicLoad (SC,loc) ->
            sprintf "atomic_load(%s)" (dump_loc_exp loc)
        | AtomicLoad (mo,loc) ->
            sprintf "atomic_load_explicit(%s,%s)"
              (dump_loc_exp loc) (dump_mem_order mo)
        | Deref (Load _ as e) -> sprintf "*%s" (dump_exp e)
        | Deref e -> sprintf "*(%s)" (dump_exp e)
        | Const v -> sprintf "%i" v


      let fx chan indent fmt =
        output_string chan indent ;
        kfprintf
          (fun chan -> output_string chan "\n")
          chan fmt
      let indent1 = "  "

      let rec dump_ins chan i ins =
        let open A in
        match ins with
        | Seq (i1,i2) ->
            dump_ins chan i i1 ;
            dump_ins chan i i2
        | Decl (t,r,None) ->
            fx chan i "%s %s;" (A.dump_typ t) (A.dump_reg r)
        | Decl (t,r,Some e) ->
            fx chan i "%s %s = %s;"
              (A.dump_typ t) (A.dump_reg r) (dump_exp e)
        | Store (loc,e) ->
            fx chan i "*%s = %s;" loc (dump_exp e)
        | AtomicStore (SC,loc,e) ->
            fx chan i "atomic_store(%s,%s);"
              loc (dump_exp e)
        | SetReg (r,e) ->
            fx chan i "%s = %s;" (A.dump_reg r) (dump_exp e)
        | AtomicStore (mo,loc,e) ->
            fx chan i "atomic_store_explicit(%s,%s,%s);"
              loc (dump_exp e) (dump_mem_order mo)
        | Fence mo ->
            fx chan i "atomic_thread_fence(%s);" (dump_mem_order mo)
        | Loop body ->
            fx chan i "for (;;) {" ;
            dump_ins chan (i ^ indent1) body ;
            fx chan i "}"
        | A.BreakCond (c,r,e) ->
            fx chan i "if (%s %s %s) break;"
              (A.dump_reg r)
              (match c with A.Ne -> "!=" | A.Eq -> "==")
              (dump_exp e)
        | A.Decr r ->
            fx chan i "%s--;" (A.dump_reg r)
        | Nop -> ()


      let dump_proc_code chan p (a,i) =
        fprintf chan "P%i (%s) {\n" p (dump_args a) ;
        dump_ins chan indent1 i ; 
        fprintf chan "}\n" ;
        ()

      type prog = (args * A.ins) list

      let dump_code chan prog =
        Misc.iteri
          (fun p code ->
            dump_proc_code chan p code ;
            output_string chan "\n")
          prog

(* Empty init *)
      let dump_init chan =  fprintf chan "\n{}\n\n"

(* Test *)
      type edge = E.edge
      type node = C.node
      let ppo _f k = k

      type check = edge list list -> bool

      type test =
          {
           name : string ;
           com : string ;
           info : Code.info ;
           edges : edge list ;
           prog : prog ;
           final : F.final ;
           types : A.typ StringMap.t;
         }

      let extract_edges t = t.edges

      let dump_test_channel chan t =
        fprintf chan "C %s\n" t.name ;
        if t.com <>  "" then fprintf chan "\"%s\"\n" t.com ;
        List.iter
          (fun (k,v) -> fprintf chan "%s=%s\n" k v)
          t.info ;
        Hint.dump O.hout t.name t.info ;
        (* Empty init *)
        dump_init chan ;
        dump_code chan t.prog ;
        F.dump_final chan t.final ;
        ()

      let dump_test ({ name = name; _ } as t) =
        let fname = name ^ ".litmus" in
        Misc.output_protect
          (fun chan -> dump_test_channel chan t)
          fname

      let test_of_cycle name ?com ?(info=[]) ?(check=(fun _ -> true)) es c =
        let com = match com with None -> E.pp_edges es | Some com -> com in
        let (prog,final),(prf,coms),env = compile_cycle check c in
        let coms = String.concat " " coms in
        let info = info@["Prefetch",prf ; "Com",coms; "Orig",com; ] in
        { name=name ; info=info; com=com ;  edges = es ;
          prog=prog ; final=final ; types=env;}
          
      let make_test name ?com ?info ?check es =
        try
          if O.verbose > 1 then eprintf "**Test %s**\n" name ;
          if O.verbose > 2 then eprintf "**Cycle %s**\n" (E.pp_edges es) ;
          let es,c = C.make es in
          test_of_cycle name ?com ?info ?check es c
        with
        | Misc.Fatal msg ->
            Warn.fatal "Test %s [%s] failed:\n%s" name (E.pp_edges es) msg
              


    end
