(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Code
open Printf

module type Config = sig
  val verbose : int
  val hout : Hint.out
  val cond : Config.cond
  val coherence_decreasing : bool
  val neg : bool
  val nprocs : int
  val eprocs : bool
  val do_observers : Config.do_observers
  val obs_type : Config.obs_type
  val optcond : bool
  val overload : int option
  val fno : Config.fno
  val poll : bool
  val optcoherence : bool
end

module Make (O:Config) (C:XXXCompile.S) : sig

  module A : Arch.S

  type test
  type edge = C.E.edge

(* Returns resolved edges of test *)
  val extract_edges : test -> edge list

(* Build up test, test structure includes
   name & comment given as first two arguments,
   third argument is the last minute check *)
  type info = (string * string) list

  type check = edge list list -> bool
  val make_test :
      string -> ?com:string -> ?info:info -> ?check:check -> edge list -> test
(* Build test from cycle *)
  val test_of_cycle :
      string -> ?com:string -> ?info:info -> ?check:check -> edge list ->
       C.C.node -> test  
(* Dump the given test *)
  val dump_test : test -> unit
  val dump_test_channel : out_channel -> test -> unit


(* Combination of make_test/dump_test *)
(*
  val compile :
      string -> ?com:string -> ?info:MiscParser.info ->
        ?check:check -> edge list -> unit
*)

end
= struct

(* Config *)

  module A = C.A
  module Run = Run.Make(O)(C)

  open C.E
  type edge = C.E.edge

  type info = (string * string) list
  type final =
    | Exists of A.final
    | Forall of (A.location * Code.v) list list
    | Locations of A.location list

  type test =
      {
       name : string ;
       com : string ;
       info : info ;
       edges : edge list ;
       init : A.init ;
       prog : A.pseudo list list ;
       final : final ;
     }

  let extract_edges {edges=es; _} = es

(********)
(* Util *)
(********)

module StringSet = MySet.Make(String)
module StringMap = MyMap.Make(String)

(***************)
(* Compilation *)
(***************)

  let show_in_cond =
    if O.optcond then
      let valid_edge e = match e.C.E.edge with
      | Rf _ | RfStar _| Fr _ | Ws _ | Hat|Detour _|DetourWs _ -> true
      | Po _ | Fenced _ | Dp _|Rmw -> false
      | Store| Leave | Back -> assert false in
      (fun n -> valid_edge n.C.C.prev.C.C.edge || valid_edge n.C.C.edge)
    else
      (fun _ -> true)

  let add_final_v p r v finals = (A.Reg (p,r),v)::finals

  let add_final p o n finals = match o with
  | Some r ->
      let m,fs = finals in
      if show_in_cond n then
        C.C.EventMap.add n.C.C.evt (A.Reg (p,r)) m,
        add_final_v p r (Ints.singleton n.C.C.evt.C.C.v) fs
      else finals
  | None -> finals

  let rec emit_overload st p init ov loc =
    if ov <= 0 then init,[],st
    else
      let loc_ov = sprintf "%s%i" loc ov in
      let _,init,i,st = C.emit_load st p init loc_ov in
      let init,is,st = emit_overload st p init (ov-1) loc in
      init,i@is,st

  let insert_overload n = match n.C.C.edge.C.E.edge with
  | Po (_,Dir R,Dir (W|R)) -> true
  | _ -> false

  type prev_load =
    | No       (* Non-existent or irrelevant *)
    | Yes of C.E.dp * C.A.arch_reg


(* Encodes load of first non-initial vakue in chain,
   can poll on value in place of checking it *)
  let do_poll n = match O.poll,n.C.C.prev.C.C.edge.C.E.edge,n.C.C.evt.C.C.v with
  | true,Rf Ext,1 -> true
  | _,_,_ -> false

  let emit_access ro_prev st p init n =
    let init,ip,st = match O.overload with
    | Some ov  when insert_overload n ->
        emit_overload st p init ov n.C.C.evt.C.C.loc
    | _ -> init,[],st in
    let o,init,i,st = match ro_prev with
    | No ->
        let open Config in
        begin match
          O.fno,n.C.C.evt.C.C.dir,n.C.C.prev.C.C.prev.C.C.edge.C.E.edge,
          n.C.C.prev.C.C.edge.C.E.edge,
          n.C.C.edge.C.E.edge
        with
        | (FnoPrev|FnoBoth|FnoOnly),R,_,Rf _, C.E.Fenced _->
            let r,init,i,st = C.emit_fno st p init n.C.C.evt.C.C.loc in
            Some r,init,i,st
        | (FnoBoth|FnoOnly),R,Rf _, C.E.Fenced _,_->
            let r,init,i,st = C.emit_fno st p init n.C.C.evt.C.C.loc in
            Some r,init,i,st
        | _ ->
            if do_poll n then
              let r,init,i,st =
                C.emit_load_one st p init n.C.C.evt.C.C.loc in
              Some r,init,i,st
            else
              C.emit_access st p init n.C.C.evt
        end
    | Yes (dp,r1) -> C.emit_access_dep st p init n.C.C.evt dp r1 in    
    o,init,ip@i,st

let edge_to_prev_load o e = match o with
| None -> No
| Some r ->
    begin match e.C.E.edge with
    | Dp (dp,_,_) -> Yes (dp,r)
    | _ -> No
    end

let get_fence n =
  let open Config in 
  match O.fno,n.C.C.edge.C.E.edge with
  | FnoOnly,C.E.Fenced (fe,_,_,_) ->
      begin match n.C.C.evt.C.C.dir,n.C.C.next.C.C.evt.C.C.dir with
      | R,R -> None
      | _,_ -> Some fe
      end
  | _, C.E.Fenced (fe,_,_,_) ->  Some fe
  | _,_ -> None
        
let rec compile_stores st p i ns k = match ns with
| [] -> i,k,st
| n::ns ->
    let sto = n.C.C.store in
    if sto == C.C.nil then
      compile_stores st p i ns k
    else
      let _,i,c,st = C.emit_access st p i sto.C.C.evt in
      let i,k,st = compile_stores st p i ns k in
      i,(c@k),st

let rec compile_proc loc_writes st p ro_prev init ns = match ns with
| [] -> init,[],(C.C.EventMap.empty,[]),st
| n::ns ->
    let open Config in
    begin match
      O.fno,
      n.C.C.evt.C.C.dir,n.C.C.prev.C.C.edge.C.E.edge,
      n.C.C.next.C.C.evt.C.C.dir,ns with
    | FnoAll,R,_,_,_ ->
        begin match  ro_prev with
        | No -> ()
        | Yes _ -> Warn.fatal "Dependency to fno"
        end ;
        let o,init,i,st = C.emit_fno st p init n.C.C.evt.C.C.loc in
        let init,is,finals,st =
          compile_proc loc_writes
            st p (edge_to_prev_load (Some o) n.C.C.edge)
            init ns in
        init,
        i@(match get_fence n with Some fe -> C.emit_fence fe::is  | _ -> is),
        (if StringSet.mem n.C.C.evt.C.C.loc loc_writes then
          add_final p (Some o) n finals
        else finals),
        st
    | (FnoRf,R,Rf _,d,m::ns)
    | (_,R,RfStar _,d,m::ns) ->
        let o1,init,i1,lab,st = C.emit_open_fno st p init n.C.C.evt.C.C.loc in
        let o2,init,i2,st =
          emit_access (edge_to_prev_load (Some o1) n.C.C.edge) st p init m in
        let init,i3,st = C.emit_close_fno st p init lab o1 n.C.C.evt.C.C.loc in
        let init,is,finals,st =
          compile_proc loc_writes
            st p (edge_to_prev_load o2 m.C.C.edge)
            init ns in
        init,
        i1@
        (match  get_fence n  with Some fe -> [C.emit_fence fe] | _ -> [])@
        (match d with R -> i2@i3@is | W -> i3@i2@is),
        add_final p (Some o1) n (add_final p o2 m finals),
        st
    | _ ->
        let o,init,i,st = emit_access ro_prev st p init n in
        let init,is,finals,st =
          compile_proc loc_writes
            st p (edge_to_prev_load o n.C.C.edge)
            init ns in
        init,
        i@(match get_fence n with Some fe -> C.emit_fence fe::is  | _ -> is),
        (if
          StringSet.mem n.C.C.evt.C.C.loc loc_writes && not (do_poll n)
        then
          add_final p o n finals
        else finals),
        st
    end


(*************)
(* Observers *)
(*************)

let last_observation st p i x v =
  let r,i,c,_st = C.emit_load st p i x in
  i,c,add_final_v p r v []

let rec straight_observer st p i x = function
  | [] -> i,[],[]
  | [v] -> last_observation st p i x v
  | v::vs ->
      let r,i,c,st = C.emit_load st p i x in
      let i,cs,fs = straight_observer st p i x vs in
      i,c@cs,add_final_v p r v fs

let rec fenced_observer st p i x = function
  | [] -> i,[],[]
  | [v] -> last_observation st p i x v
  | v::vs ->
      let r,i,c,st = C.emit_load st p i x in
      let f = [C.emit_fence C.stronger_fence] in
      let i,cs,fs = fenced_observer st p i x vs in
      i,c@f@cs,add_final_v p r v fs


let loop_observer st p i x = function
  | []|[_] -> i,[],[]        
  | v::vs ->
      let r,i,c,st = C.emit_load_not_zero st p i x in
      let rec do_loop st i prev_r = function
        | [] ->  assert false
        | [v] ->
            let r,i,c,_st = C.emit_load_not_eq st p i x prev_r in
            i,c,add_final_v p r v []
        | v::vs ->
            let r,i,c,st = C.emit_load_not_eq st p i x prev_r in
            let i,cs,fs = do_loop st i r vs in
            i,c@cs,add_final_v p r v fs in
      let i,cs,fs = do_loop st i r vs in
      i,c@cs,add_final_v p r v fs

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

  let build_observer st p i x vs =
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
    let i,cs,fs =
      let open Config in
      match O.obs_type with
      | Straight ->  straight_observer st p i x vs
      | Config.Fenced -> fenced_observer st p i x vs
      | Loop -> loop_observer st p i x vs in
    i,cs,fs@f

  let rec build_observers p i x arg =
    let open Config in
    match arg,O.do_observers with
    | [],_ -> i,[],[]
    | []::vss,_
    | [_]::vss,(Avoid|Accept) ->
        build_observers p i x vss
    | vs::vss,_ ->
        let i0 = i in
        try 
          let i,c,f = build_observer A.st0 p i x vs in
          begin match c,O.do_observers with
          | _::_,Avoid  -> Warn.fatal "Observer"
          | _,_ -> ()
          end ;
          match c with
          | [] -> 
              let i,cs,fs = build_observers p i0 x vss in
              i,cs,f@fs
          | _ ->
              let i,cs,fs = build_observers (p+1) i x vss in
              i,c::cs,f@fs
        with NoObserver -> build_observers p i x vss
            
  let rec check_rec p i =
    let add_look_loc loc v k =
      if O.optcond then k else (A.Loc loc,Ints.singleton v)::k in
    let open Config in
    function
      | [] -> i,[],[]
      | (x,vs)::xvs ->          
          let i,c,f = match O.cond with
          | Observe ->
              let vs = List.flatten vs in
              begin match vs with
              | [] -> i,[],[]
              | _::_ ->
                  let v,_ = Misc.last vs in
                  i,[],[A.Loc x,Ints.singleton v]
              end
          | Unicond -> assert false
          | Cycle -> begin
              match vs with
              | []|[[_]] -> i,[],[]
              | [[_;(v,_)]] ->
                  begin match O.do_observers with
                  | Local -> i,[],add_look_loc x v []
                  | Avoid|Accept -> i,[],[A.Loc x,Ints.singleton v]
              | Enforce ->  
                  let i,c,f = build_observers p i x vs in
                  i,c,add_look_loc x v f
                  end
              | _ ->
              let v =
                let v,_ = Misc.last (List.flatten vs) in
                v in
              begin match O.do_observers with
              | Local -> i,[],add_look_loc x v []
              | _ ->
                  let i,c,f = build_observers p i x vs in
                  i,c,add_look_loc x v f
              end
          end in
          let i,cs,fs =
            check_rec (p+List.length c) i xvs in
          i,c@cs,f@fs

  let check_writes p i cos = check_rec p i cos

  let comp_loc_writes n0 =
    let rec do_rec n =
      let k =
        if n.C.C.next == n0 then StringSet.empty
        else do_rec n.C.C.next in
      let k =
        if n.C.C.store != C.C.nil then
          StringSet.add n.C.C.store.C.C.evt.C.C.loc k
        else k in
      let k = 
        match n.C.C.evt.C.C.dir with
        | W -> StringSet.add n.C.C.evt.C.C.loc k
        | R -> k in
      if is_detour n.C.C.edge  then StringSet.add n.C.C.evt.C.C.loc k
      else k in
    do_rec n0

(******************)
(* Prefetch hints *)
(******************)


(* In thread/Out thread *)
type pt = { ploc:Code.loc ; pdir:Code.dir; }

let io_of_node n = {ploc=n.C.C.evt.C.C.loc; pdir=n.C.C.evt.C.C.dir;}

let io_of_thread n = match n with
| []|[_] -> None
| n0::rem ->
    Some (io_of_node n0,io_of_node (Misc.last rem))

let io_of_detour _n = None

  let compile_prefetch_ios =
    let rec do_rec p = function
      | [] -> []
      | None::rem -> do_rec (p+1) rem
      | Some (i,o)::rem ->
          let k = do_rec (p+1) rem in
          if i.ploc = o.ploc then k
          else
            sprintf "%i:%s=F" p i.ploc::
            sprintf "%i:%s=%s" p o.ploc
              (match o.pdir with W -> "W" | R -> "T")::k in
    fun fst ios -> String.concat "," (do_rec fst ios)

(******************)
(* Affinity hints *)
(******************)

(*  Most of placement computation is now by litmus *)

  let last_edge ns =
    let n = Misc.last ns in n.C.C.edge.C.E.edge

  let compile_coms nss =
    List.map
      (fun ns ->
        match last_edge ns with
        | Fr _ -> "Fr"
        | Rf _|RfStar _ -> "Rf"
        | Ws _ -> "Ws"
        | _ -> assert false)
      nss


(* Local check of coherence *)

  let do_add_load st p i f x v =
    let r,i,c,st = C.emit_load st p i x in
    i,c,add_final_v p r (Ints.singleton v) f,st

  let do_add_loop st p i f x v w =
    let r,i,c,st = C.emit_load_not_value st p i x v in
    i,c,add_final_v p r (Ints.singleton w) f,st


  let is_load_init e = e.C.C.evt.C.C.dir = R && e.C.C.evt.C.C.v = 0

  let do_observe_local st p i code f x prev_v v =
    let open Config in
    match O.obs_type with
    | Straight ->
        let i,c,f,st = do_add_load st p i f x v in
        i,code@c,f,st
    | Config.Fenced ->
        let i,c,f,st = do_add_load st p i f x v in
        let c = C.emit_fence C.stronger_fence::c in
        i,code@c,f,st
    | Loop ->
        let i,c,f,st = do_add_loop st p i f x prev_v v in
        i,code@c,f,st

  let add_co_local_check lsts ns st p i code f =
    let lst = Misc.last ns in
    match lst.C.C.edge.C.E.edge with
    | Ws _|Fr _
    | DetourWs _ when not (is_load_init lst) ->
        let x = lst.C.C.evt.C.C.loc and v = lst.C.C.next.C.C.evt.C.C.v
        and prev_v = lst.C.C.evt.C.C.v in
        let all_lst =
          try StringMap.find x lsts
          with Not_found -> C.C.evt_null in
        if C.C.OrderedEvent.compare all_lst lst.C.C.next.C.C.evt = 0
        then
          i,code,(A.Loc x,Ints.singleton v)::f,st
        else
          do_observe_local st p i code f x prev_v v
    | _ -> i,code,f,st

(**********)
(* Detour *)
(**********)

  let do_observe_local_before st p i code f x prev_v v =
    if O.optcoherence && v = 0 then
      i,code,[],st
    else
      let open Config in
      match O.obs_type with
      | Straight|Config.Fenced ->
          let i,c,f,st = do_add_load st p i f x v in
          i,code@c,f,st
      | Loop ->
          let i,c,f,st = do_add_loop st p i f x prev_v v in
          i,code@c,f,st
            

  let build_detour lsts st p i n =
    let open Config in
    let i,c0,f,st = match O.do_observers with
    | Local -> begin match n.C.C.edge.C.E.edge with
      | DetourWs (Dir W) ->          
          do_observe_local_before st p i [] [] n.C.C.evt.C.C.loc
            n.C.C.prev.C.C.prev.C.C.evt.C.C.v n.C.C.prev.C.C.evt.C.C.v
      | DetourWs (Dir R) ->          
          do_observe_local_before st p i [] [] n.C.C.evt.C.C.loc
            n.C.C.prev.C.C.prev.C.C.evt.C.C.v n.C.C.prev.C.C.evt.C.C.v
      | _ -> i,[],[],st 
    end
    | _ -> i,[],[],st in

    let _,i,c,st = C.emit_access st p i n.C.C.evt in
    let c = c0@c in
    match O.do_observers with
    | Local ->
        let i,c,f,_st = add_co_local_check lsts [n] st p i c f in
        i,c,f
    | _ -> i,c,f

let rec build_detours lsts p i ns = match ns with
| [] -> i,[],[]
| n::ns ->
    let i,c,f = build_detour lsts A.st0 p i n in
    let i,cs,fs = build_detours lsts (p+1) i ns in
    i,c::cs,f@fs

(****************************)
(* Last in coherence orders *)
(****************************)


let rec find_last = function
  | [] -> assert false
  | [xs] -> Misc.last xs
  | _::xss -> find_last xss


let last_map cos =
  let lsts =
    List.map
      (fun (loc,xss) ->
        let r,_ = find_last xss in
        loc,r)
      cos in
  List.fold_left
    (fun m (loc,lst) -> StringMap.add loc lst.C.C.evt m)
    StringMap.empty lsts

(******************************************)
(* Compile cycle, ie generate test proper *)
(******************************************)

  let compile_cycle ok n =
    let open Config in
    Label.reset () ;
    let splitted =  C.C.split_procs n in
 (* Split before, as  proc numbers added by side effet.. *)
    let cos0 = C.C.coherence n in    
    let lsts = match O.do_observers with
    | Config.Local when O.optcoherence -> last_map cos0
    | _ -> StringMap.empty in       
    let cos =
      List.map
        (fun (loc,ns) ->
          loc,
          List.map
            (List.map (fun (n,obs) -> n.C.C.evt.C.C.v,obs))
            ns)
        cos0 in
            
(*    let co = flatten_co cos in *)
    if O.verbose > 1 then begin
      eprintf "COHERENCE: " ;
      Misc.pp_list stderr ""
        (fun chan (x,vs) ->
          fprintf chan "<%s:%a>" x
            (fun chan ->
              Misc.pp_list chan "|"
                (fun chan ->
                  Misc.pp_list chan ","
                    (fun chan (n,obs) ->
                      fprintf chan "%i{%s}" n.C.C.evt.C.C.v
                        (Ints.pp_str "," (sprintf "%i") obs)
                    )))
            vs)
        cos0 ;
      eprintf "\n%!"
    end ;
    let loc_writes = comp_loc_writes n in
    let rec do_rec p i = function
      | [] -> List.rev i,[],(C.C.EventMap.empty,[]),[]
      | n::ns ->
          let i,c,(m,f),st = compile_proc loc_writes A.st0 p No i n in
          let i,c,st = compile_stores st p i n c in
          let i,c,f,st =
            match O.cond with
            | Unicond -> i,c,f,st                
            | Cycle|Observe ->
                match O.do_observers with
                | Local -> add_co_local_check lsts n st p i c f
                | Avoid|Accept|Enforce -> i,c,f,st in
          let i,c,_ = C.postlude st p i c in
          let ds = C.C.get_detours_from_list n in
          let i,cds,fds = build_detours lsts (p+1) i ds in
          let i,cs,(ms,fs),ios = do_rec (p+1+List.length cds) i ns in
          let io = io_of_thread n in
          let iod = List.map io_of_detour ds in
          i,c::(cds@cs),(C.C.union_map m ms,f@fds@fs),(io::iod)@ios in
    let i,obsc,f =
      match O.cond with
      | Unicond -> [],[],[]
      | Cycle|Observe -> check_writes 0  [] cos in
    match splitted,O.cond with
    | [],_ -> Warn.fatal "No proc"
(*    | [_],Cycle -> Warn.fatal "One proc" *)
    | _,_ ->
        let i,c,(m,f),ios =
          if
            let len =  List.length splitted in
            O.nprocs <= 0 ||
            (if O.eprocs then len = O.nprocs else len <= O.nprocs)
          then
            let ess = List.map (List.map (fun n -> n.C.C.edge)) splitted in
            if ok ess then
              let i,cs,(m,fs),ios = do_rec (List.length obsc) i splitted in
              if
                List.exists
                  (fun (_,loc) -> (loc:string) = Code.ok)
                  i
              then
                (C.A.Loc Code.ok,"1")::i,obsc@cs,
                (m,(C.A.Loc Code.ok,Ints.singleton 1)::f@fs),ios
              else
                i,obsc@cs,(m,f@fs),ios
            else Warn.fatal "Last minute check"
          else  Warn.fatal "Too many procs" in
        let r =
          match O.cond with
          | Unicond ->
              let evts =
                List.map
                  (List.map (fun n -> n.C.C.evt))
                  splitted in
              let f = Run.run evts m in
              i,c,Forall f
          | Cycle -> i,c,Exists f
          | Observe ->i,c,Locations (List.map fst f) in
        r,(compile_prefetch_ios (List.length obsc) ios,compile_coms splitted)


(********)
(* Dump *)
(********)

let get_proc = function
  | A.Loc _ -> -1
  | A.Reg (p,_) -> p

let dump_init chan inits =
  fprintf chan "{" ;
  let rec p_rec q = function
    | [] -> fprintf chan "\n}\n"
    | (left,loc)::rem ->
        let p = get_proc left in
        if p <> q then fprintf chan "\n" else fprintf chan " " ;
        fprintf chan "%s=%s;" (A.pp_location left) loc ;
        p_rec p rem in
  p_rec (-1) inits


let rec dump_pseudo = function
  | [] -> []
  | A.Instruction ins::rem -> A.dump_instruction ins::dump_pseudo rem
  | A.Label (lbl,ins)::rem ->
      sprintf "%s:" lbl::dump_pseudo (ins::rem)
  | A.Nop::rem -> dump_pseudo rem
  | A.Macro (m,args)::rem ->
      sprintf "%s(%s)"
        m
        (String.concat ","
           (List.map A.pp_reg args))::
      dump_pseudo rem

let fmt_cols =
  let rec fmt_col p k = function
    | [] -> k
    | cs::prog ->
        (sprintf "P%i" p::dump_pseudo cs)::
        fmt_col (p+1) k prog in
  fmt_col 0 []

  let dump_code chan code =
    let pp = fmt_cols code in
    Misc.pp_prog chan pp

  let dump_state fs =
    String.concat " /\\ " 
      (List.map
         (fun (r,vs) ->
           match Ints.as_singleton vs with
           | Some v ->
               sprintf "%s=%i" (A.pp_location r) v
           | None ->
               let pp =
                 Ints.pp_str " \\/ "
                   (fun v -> sprintf "%s=%i" (A.pp_location r) v)
                   vs in
               sprintf "(%s)" pp)             
         fs)

  let dump_final chan tst = match tst.final with
  | Exists fs ->
      fprintf chan "%sexists\n" (if !Config.neg then "~" else "") ;
      fprintf chan "(%s)\n" (dump_state fs)
  | Forall ffs ->
      fprintf chan "forall\n" ;
      fprintf chan "%s\n" (Run.dump_cond ffs)
  | Locations locs ->
      fprintf chan "locations [%s]\n"
        (String.concat ""
           (List.map (fun loc -> sprintf "%s;" (A.pp_location loc)) locs))

  let dump_test_channel chan t =
    fprintf chan "%s %s\n" (Archs.pp A.arch) t.name ;
    if t.com <>  "" then fprintf chan "\"%s\"\n" t.com ;
    List.iter
      (fun (k,v) -> fprintf chan "%s=%s\n" k v)
      t.info ;
    Hint.dump O.hout t.name t.info ;
    dump_init chan t.init ;
    dump_code chan t.prog ;
    dump_final chan t ;
    ()

let dump_test ({ name = name; _ } as t) =
  let fname = name ^ ".litmus" in
  Misc.output_protect
    (fun chan -> dump_test_channel chan t)
    fname


let test_of_cycle name ?com ?(info=[]) ?(check=(fun _ -> true)) es c =
  let com = match com with None -> pp_edges es | Some com -> com in
  let(init,prog,final),(prf,coms) = compile_cycle check c in
  let coms = String.concat " " coms in
  let info = info@["Prefetch",prf ; "Com",coms; "Orig",com; ] in
  { name=name ; info=info; com=com ;  edges = es ;
    init=init ; prog=prog ; final=final ; }
    
let make_test name ?com ?info ?check es =
  try
    if O.verbose > 1 then eprintf "**Test %s**\n" name ;
    if O.verbose > 2 then eprintf "**Cycle %s**\n" (pp_edges es) ;
    let es,c = C.C.make es in
    test_of_cycle name ?com ?info ?check es c
  with
  | Misc.Fatal msg ->
      Warn.fatal "Test %s [%s] failed:\n%s" name (pp_edges es) msg
  

 type check = edge list list -> bool


end
