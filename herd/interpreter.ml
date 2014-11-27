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

(** Interpreter for a user-specified model *)

open Printf

module type S = sig

  module S : Sem.Semantics

(* Values *)
  type ks =
      { id : S.event_rel Lazy.t; unv : S.event_rel Lazy.t;
        evts : S.event_set; }

  module V : sig type env end


(* Helpers, initialisation *)
  val env_empty : V.env
  val add_rels : V.env -> (string * S.event_rel Lazy.t) list -> V.env
  val add_sets : V.env -> (string * S.event_set Lazy.t) list -> V.env

(* State of interpreter *)

  type st = {
    env : V.env ;
    show : S.event_rel StringMap.t Lazy.t ;
    seen_requires_clause : bool ;
    skipped : StringSet.t ;
  }

  val show_to_vbpp :
    st -> (StringMap.key * S.event_rel) list

  val interpret :
    (unit -> unit) -> (* function called when a requires clause fails *)
    S.test ->
    S.concrete ->
    V.env ->
    ks ->
    (StringMap.key * S.event_rel) list Lazy.t ->
    st option
end


module type Config = sig
  val m : AST.pp_t
  include Model.Config
end

module Make
    (O:Config)
    (S:Sem.Semantics)
    (B:AllBarrier.S with type a = S.barrier)
    :
    (S with module S = S)
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
    module W = Warn.Make(O)
(*  Model interpret *)
    let (txt,(_,_,prog)) = O.m

(*
  let debug_proc chan p = fprintf chan "%i" p
  let debug_event chan e = fprintf chan "%s" (E.pp_eiid e)
  let debug_set chan s =
  output_char chan '{' ;
  E.EventSet.pp chan "," debug_event s ;
  output_char chan '}'

  let debug_events = debug_set

  let debug_rel chan r =
  E.EventRel.pp chan ","
  (fun chan (e1,e2) -> fprintf chan "%a -> %a"
  debug_event e1 debug_event e2)
  r
 *)

    type ks =
      { id : S.event_rel Lazy.t; unv : S.event_rel Lazy.t;
        evts : S.event_set; }

(* Internal typing *)
    type typ = TEvents | TRel | TTag of string |TClo | TProc | TSet of typ

    let type_equal t1 t2 = t1 = t2

    exception CompError of string


    let rec pp_typ = function
      | TEvents -> "event set"
      | TRel -> "rel"
      | TTag ty -> ty
      | TClo -> "closure"
      | TProc -> "procedure"
      | TSet elt -> sprintf "%s set" (pp_typ elt)



(*
    module V = Ivalue.Make(S)
*)

    module rec V : sig
      type v =
        | Empty | Unv
        | Rel of S.event_rel
        | Set of S.event_set
        | Clo of closure
        | Proc of procedure
        | Tag of string * string     (* type  X name *)
        | ValSet of typ * ValSet.t   (* elt type X set *)
      and env =
          { vals  : v Lazy.t StringMap.t;
            enums : string list StringMap.t;
            tags  : string StringMap.t; }
      and closure =
          { clo_args : AST.var list ;
            mutable clo_env : env ;
            clo_body : AST.exp; }
      and procedure = {
          proc_args : AST.var list;
          proc_env : env;
          proc_body : AST.ins list; }

      val type_val : v -> typ
    end = struct

      type v =
        | Empty | Unv
        | Rel of S.event_rel
        | Set of S.event_set
        | Clo of closure
        | Proc of procedure
        | Tag of string * string     (* type  X name *)
        | ValSet of typ * ValSet.t   (* elt type X set *)

      and env =
          { vals  : v Lazy.t StringMap.t;
            enums : string list StringMap.t;
            tags  : string StringMap.t; }

      and closure =
          { clo_args : AST.var list ;
            mutable clo_env : env ;
            clo_body : AST.exp; }

      and procedure = {
          proc_args : AST.var list;
          proc_env : env;
          proc_body : AST.ins list; }

    let type_val = function
      | Empty|Unv -> assert false (* Discarded before *)
      | Rel _ -> TRel
      | Set _ -> TEvents
      | Clo _ -> TClo
      | Proc _ -> TProc
      | Tag (t,_) -> TTag t
      | ValSet (t,_) -> TSet t


    end
    and ValOrder : Set.OrderedType = struct
      (* Note: cannot use Full in sets.. *)
      type t = V.v
      open V

      let error fmt = ksprintf (fun msg -> raise (CompError msg)) fmt

      let rec compare v1 v2 = match v1,v2 with
      | V.Empty,V.Empty -> 0

      | V.Empty,ValSet (t,_) -> compare (ValSet (t,ValSet.empty)) v2
      | ValSet (t,_),V.Empty -> compare v1 (ValSet (t,ValSet.empty))

      | Tag (t1,s1), Tag (t2,s2) when t1=t2 ->
          String.compare s1 s2
      | ValSet (t1,s1),ValSet (t2,s2) when type_equal t1 t2 ->
          ValSet.compare s1 s2
      | Rel r1,Rel r2 -> E.EventRel.compare r1 r2
      | Set s1,Set s2 -> E.EventSet.compare s1 s2
      | (Unv,_)|(_,Unv) -> error "Universe in compare"
      | _,_ ->
          let t1 = V.type_val v1
          and t2 = V.type_val v2 in
          if type_equal t1 t2 then
            error "Sets of %s are illegal" (pp_typ t1)
          else
            error
              "Heterogeneous set elements: types %s and %s "
              (pp_typ t1) (pp_typ t2)

    end and ValSet : (MySet.S with type elt = V.v) = MySet.Make(ValOrder)


    let error loc fmt =
      ksprintf
        (fun msg ->
          eprintf "%a: %s\n" TxtLoc.pp loc msg ;
          raise Misc.Exit) (* Silent failure *)
        fmt

    let set_op loc t op s1 s2 =
      try V.ValSet (t,op s1 s2)
      with CompError msg -> error loc "%s" msg

    open V

(* pretty *)
    let rec pp_val = function
      | Unv -> "<universe>"
      | V.Empty -> "{}"
      | Tag (_,s) -> sprintf "'%s" s
      | ValSet (t,s) ->
          sprintf "<%s>{%s}" (pp_typ t) (ValSet.pp_str "," pp_val s)
      | v -> sprintf "<%s>" (pp_typ (type_val v))

(* lift a tag to a singleton set *)
    let tag2set v = match v with
      | Tag (t,_) -> ValSet (TTag t,ValSet.singleton v)
      | _ -> v


(* Add values to env *)
    let add_val k v env = { env with vals = StringMap.add k v env.vals; }

    let env_empty =
      {vals=StringMap.empty;
       enums=StringMap.empty;
       tags=StringMap.empty; }

    let add_vals mk env bds =
      let vals =
        List.fold_left
          (fun vals (k,v) -> StringMap.add k (mk v) vals)
          env.vals bds in
      { env with vals; }


    let add_rels env bds =
      add_vals (fun v -> lazy (Rel (Lazy.force v))) env bds

    and add_sets env bds =
      add_vals (fun v -> lazy (Set (Lazy.force v))) env bds

    type st = {
        env : V.env ;
        show : S.event_rel StringMap.t Lazy.t ;
        seen_requires_clause : bool ;
        skipped : StringSet.t ;
      }

    let tags_universe {enums=env} t =
      let tags =
        try StringMap.find t env
        with Not_found -> assert false in
      let tags = ValSet.of_list (List.map (fun s -> Tag (t,s)) tags) in
      tags

    let find_env {vals=env} k =
      Lazy.force begin
        try StringMap.find k env
        with
        | Not_found -> Warn.user_error "unbound var: %s" k
      end

    let find_env_loc loc env k =
      try  find_env env k
      with Misc.UserError msg -> error loc "%s" msg

    let as_rel ks = function
      | Rel r -> r
      | Empty -> E.EventRel.empty
      | Unv -> Lazy.force ks.unv
      | _ ->  assert false

    let as_set ks = function
      | Set s -> s
      | Empty -> E.EventSet.empty
      | Unv -> ks.evts
      | _ -> assert false

    let as_valset = function
      | ValSet (_,v) -> v
      | _ -> assert false

    exception Stabilised of typ

    let stabilised ks env =
      let rec stabilised vs ws = match vs,ws with
      | [],[] -> true
      | v::vs,w::ws -> begin match v,w with
        | (_,V.Empty)|(Unv,_) -> stabilised vs ws
(* Relation *)
        | (V.Empty,Rel w) -> E.EventRel.is_empty w && stabilised vs ws
        | (Rel v,Unv) ->
            E.EventRel.subset (Lazy.force ks.unv) v && stabilised vs ws
        | Rel v,Rel w ->
            E.EventRel.subset w v && stabilised vs ws
(* Event Set *)
        | (V.Empty,Set w) -> E.EventSet.is_empty w && stabilised vs ws
        | (Set v,Unv) ->
            E.EventSet.subset ks.evts v && stabilised vs ws
        | Set v,Set w ->
            E.EventSet.subset w v && stabilised vs ws
(* Value Set *)
        | (V.Empty,ValSet (_,w)) -> ValSet.is_empty w && stabilised vs ws
        | (ValSet (TTag t,v),Unv) ->
            ValSet.subset (tags_universe env t) v && stabilised vs ws
        | ValSet (_,v),ValSet (_,w) ->
            ValSet.subset w v && stabilised vs ws
        | _,_ ->
            raise (Stabilised (type_val w))

      end
      | _,_ -> assert false in
      stabilised

    open AST

(* Syntactic function *)
    let is_fun = function
      | Fun _ -> true
      | _ -> false


(* Get an expression location *)
    let get_loc = function
      | Konst (loc,_)
      | Tag (loc,_)
      | Var (loc,_)
      | ExplicitSet (loc,_)
      | Op1 (loc,_,_)
      | Op (loc,_,_)
      | Bind (loc,_,_)
      | BindRec (loc,_,_)
      | App (loc,_,_)
      | Fun (loc,_,_)
      | Match (loc,_,_,_)
      | MatchSet (loc,_,_,_)
        -> loc


(* State of interpreter *)

    let rt_loc lbl =
      if
        O.verbose <= 1 &&
        not (StringSet.mem lbl S.O.PC.symetric) &&
        not (StringSet.mem lbl S.O.PC.showraw)
      then S.rt else (fun x -> x)

    let show_to_vbpp st =
      StringMap.fold (fun tag v k -> (tag,v)::k)   (Lazy.force st.show) []

    let empty_rel = Rel E.EventRel.empty
    let noid r =
      Rel
        (E.EventRel.filter
           (fun (e1,e2) -> not (E.event_equal e1 e2))
           r)

    let error_typ loc t0 t1  =
      error loc"type %s expected, %s found" (pp_typ t0) (pp_typ t1)

    let error_rel loc v = error_typ loc TRel (type_val v)
    and error_set loc v = error_typ loc TEvents (type_val v)

    let type_list = function
      | [] -> assert false
      | (_,v)::vs ->
          let t0 = type_val v in
          let rec type_rec = function
            | [] -> []
            | (loc,v)::vs ->
                let t1 = type_val v in
                if t0 = t1 then v::type_rec vs
                else
                  error loc
                    "type %s expected, %s found" (pp_typ t0) (pp_typ t1) in
          t0,v::type_rec vs

(* Helpers for n-ary operations *)

(* Check explicit set arguments *)
    let set_args =
      let rec s_rec = function
        | [] -> []
        | (loc,Unv)::_ ->
            error loc "universe in explicit set"
        | (loc,V.Empty)::_ ->
            error loc "empty in explicit set"
        | x::xs -> x::s_rec xs in
      s_rec
(* Union is polymorphic *)
    let union_args =
      let rec u_rec = function
        | [] -> []
        | (_,V.Empty)::xs -> u_rec xs
        | (_,Unv)::_ -> raise Exit
        | (loc,v)::xs ->
            (loc,tag2set v)::u_rec xs in
      u_rec

(* Sequence applies to relations *)
    let seq_args ks =
      let rec seq_rec = function
        | [] -> []
        | (_,V.Empty)::_ -> raise Exit
        | (_,V.Unv)::xs -> Lazy.force ks.unv::seq_rec xs
        | (_,Rel r)::xs -> r::seq_rec xs
        | (loc,v)::_ -> error_rel loc v in
      seq_rec

    let is_dir = function
        (* Todo: are these still needed? *)
      | Unv_Set -> (fun _ -> true)
      | Bar_Set -> E.is_barrier
      | WriteRead -> E.is_mem
      | Write -> E.is_mem_store
      | Read -> E.is_mem_load
      | Atomic -> E.is_atomic
      | Plain -> fun e -> not (E.is_atomic e)

(* interpreter *)
    let interpret failed_requires_clause test conc m ks vb_pp =

      let rec eval_loc env e = get_loc e,eval env e

      and eval env = function
        | Konst (_,Empty SET) -> V.Empty (* Polymorphic empty *)
        | Konst (_,Empty RLN) -> empty_rel
        | AST.Tag (loc,s) ->
            begin try
              V.Tag (StringMap.find s env.tags,s)
            with Not_found ->
              error loc "tag '%s is undefined" s
            end
        | Var (loc,k) ->
            find_env_loc loc env k
        | Fun (_,xs,body) ->
            Clo {clo_args=xs; clo_env=env; clo_body=body; }
(* Unary operators *)
        | Op1 (_,Plus,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Unv
            | Rel r -> Rel (S.tr r)
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Star,e) ->
            begin match eval env e with
            | V.Empty -> Rel (Lazy.force ks.id)
            | Unv -> Unv
            | Rel r -> Rel (S.union (S.tr r) (Lazy.force ks.id))
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Opt,e) ->
            begin match eval env e with
            | V.Empty -> Rel (Lazy.force ks.id)
            | Unv -> Unv
            | Rel r -> Rel (S.union r (Lazy.force ks.id))
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Select (s1,s2),e) ->
            let f1 = is_dir s1 and f2 = is_dir s2 in
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv ->
                Rel (S.restrict f1 f2 (Lazy.force ks.unv))
            | Rel r ->
                Rel (S.restrict f1 f2 r)
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Comp _,e) -> (* Back to polymorphism *)
            begin match eval env e with
            | V.Empty -> Unv
            | Unv -> V.Empty
            | Set s ->
                Set (E.EventSet.diff ks.evts s)
            | Rel r ->
                Rel (E.EventRel.diff (Lazy.force ks.unv) r)
            | ValSet (TTag ts as t,s) ->
                ValSet (t,ValSet.diff (tags_universe env ts) s)
            | v ->
                error (get_loc e)
                  "set or relation expected, %s found"
                     (pp_typ (type_val v))
            end
        | Op1 (_,Inv,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Unv
            | Rel r -> Rel (E.EventRel.inverse r)
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Square,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Unv
            | Set s -> Rel (E.EventRel.cartesian s s)
            | v -> error_set (get_loc e) v
            end
        | Op1 (_,Ext,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Rel (U.ext (Lazy.force ks.unv))
            | Rel r -> Rel (U.ext r)
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Int,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Rel (U.internal (Lazy.force ks.unv))
            | Rel r -> Rel (U.internal r)
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,NoId,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> noid (Lazy.force ks.unv)
            | Rel r -> noid r
            | v -> error_rel (get_loc e) v
            end
        | Op1 (_,Set_to_rln,e) ->
            begin match eval env e with
            | V.Empty -> V.Empty
            | Unv -> Rel (Lazy.force ks.id)
            | Set s ->  Rel (E.EventRel.set_to_rln s)
            | v -> error_set (get_loc e) v
            end
(* One xplicit N-ary operator *)
        | ExplicitSet (loc,es) ->
            let vs = List.map (eval_loc env) es in
            let vs = set_args vs in
            begin match vs with
              | [] -> V.Empty
              | _ ->
                  let t,vs = type_list vs in
                  try ValSet (t,ValSet.of_list vs)
                  with CompError msg ->
                    error loc "%s" msg
            end
(* N-ary operators, those associative binary operators are optimized *)
        | Op (loc,Union,es) ->
            let vs = List.map (eval_loc env) es in
            begin try
              let vs = union_args vs in
              match vs with
              | [] -> V.Empty
              | _ ->
                  let t,vs = type_list vs in
                  match t with
                  | TRel -> Rel (S.unions (List.map (as_rel ks) vs))
                  | TEvents ->
                      Set (E.EventSet.unions  (List.map (as_set ks) vs))
                  | TSet telt ->
                      ValSet (telt,ValSet.unions (List.map as_valset vs))
                  | ty ->
                      error loc
                        "cannot perform union on type '%s'" (pp_typ ty)
            with Exit -> Unv end
        | Op (_,Seq,es) ->
            let vs = List.map (eval_loc env) es in
            begin try
              let vs = seq_args  ks vs in
              match vs with
              | [] -> Rel (Lazy.force ks.id)
              | _ -> Rel (S.seqs vs)
            with Exit -> empty_rel
            end
(* Binary operators *)
        | Op (loc,Inter,[e1;e2;]) -> (* Binary notation kept in parser *)
            let loc1,v1 = eval_loc env e1
            and loc2,v2 = eval_loc env e2 in
            begin match tag2set v1,tag2set v2 with
            | (V.Tag _,_)|(_,V.Tag _) -> assert false
            | Rel r1,Rel r2 -> Rel (E.EventRel.inter r1 r2)
            | Set s1,Set s2 -> Set (E.EventSet.inter s1 s2)
            | ValSet (t,s1),ValSet (_,s2) ->
                set_op loc t ValSet.inter s1 s2
            | (Unv,r)|(r,Unv) -> r
            | (V.Empty,_)|(_,V.Empty) -> V.Empty
            | (Clo _|Proc _),_ ->
                error loc1
                  "intersection on %s" (pp_typ (type_val v1))
            | _,(Clo _|Proc _) ->
                error loc2
                  "intersection on %s" (pp_typ (type_val v2))
            | (Rel _,Set _)
            | (Set _,Rel _)
            | (Rel _,ValSet _)
            | (ValSet _,Rel _) ->
                error loc "mixing sets and relations in intersection"
            | (ValSet _,Set _)
            | (Set _,ValSet _) ->
                error loc "mixing event sets and sets in intersection"
            end
        | Op (loc,Diff,[e1;e2;]) ->
            let loc1,v1 = eval_loc env e1
            and loc2,v2 = eval_loc env e2 in
            begin match tag2set v1,tag2set v2 with
            | (V.Tag _,_)|(_,V.Tag _) -> assert false
            | Rel r1,Rel r2 -> Rel (E.EventRel.diff r1 r2)
            | Set s1,Set s2 -> Set (E.EventSet.diff s1 s2)
            | ValSet (t,s1),ValSet (_,s2) ->
                set_op loc t ValSet.diff s1 s2
            | Unv,Rel r -> Rel (E.EventRel.diff (Lazy.force ks.unv) r)
            | Unv,Set s -> Set (E.EventSet.diff ks.evts s)
            | Unv,ValSet (TTag ts as t,s) ->
                ValSet (t,ValSet.diff (tags_universe env ts) s)
            | Unv,ValSet (t,_) ->
                error loc1 "cannot build universe for element type %s"
                  (pp_typ t)
            | Unv,V.Empty -> Unv
            | (Rel _|Set _|V.Empty|Unv|ValSet _),Unv
            | V.Empty,(Rel _|Set _|V.Empty|ValSet _) -> V.Empty
            | (Rel _|Set _|ValSet _),V.Empty -> v1
            | (Clo _|Proc _),_ ->
                error loc1
                  "difference on %s" (pp_typ (type_val v1))
            | _,(Clo _|Proc _) ->
                error loc2
                  "difference on %s" (pp_typ (type_val v2))
            | ((Set _|ValSet _),Rel _)|(Rel _,(Set _|ValSet _)) ->
                error loc "mixing set and relation in difference"
            | (Set _,ValSet _)|(ValSet _,Set _) ->
                error loc "mixing event set and set in difference"
            end
        | Op (_,Cartesian,[e1;e2;]) ->
            let s1 = eval_set env e1
            and s2 = eval_set env e2 in
            Rel (E.EventRel.cartesian s1 s2)
        | Op (loc,Add,[e1;e2;]) ->
            let v1 = eval env e1
            and v2 = eval env e2 in
            begin match v1,v2 with
            | V.Empty,_ -> error loc "empty in set ++"
            | V.Unv,_ -> error loc "universe in set ++"
            | _,V.Unv -> V.Unv
            | _,V.Empty -> V.ValSet (type_val v1,ValSet.singleton v1)
            | _,V.ValSet (_,s2) ->
                set_op loc (type_val v1) ValSet.add v1 s2
            | _,(Rel _|Set _|Clo _|Proc _|V.Tag (_, _)) ->
                error (get_loc e2)
                  "this expression of type '%s' should be a set"
                  (pp_typ (type_val v2))
            end
        | Op (_,(Diff|Inter|Cartesian|Add),_) -> assert false (* By parsing *)
(* Application/bindings *)
        | App (_,f,es) ->
            let f = eval_clo env f in
            let env = add_args f.clo_args es env f.clo_env in
            eval env f.clo_body
        | Bind (_,bds,e) ->
            let env = eval_bds env bds in
            eval env e
        | BindRec (loc,bds,e) ->
            let env = env_rec loc (fun pp -> pp) bds env in
            eval env e
        | Match (loc,e,cls,d) ->
            let v = eval env e in
            begin match v with
            | V.Tag (_,s) ->
                let rec match_rec = function
                  | [] ->
                      begin match d with
                      | Some e ->  eval env e
                      | None ->
                          error loc "pattern matching failed on value '%s'" s
                      end
                  | (ps,es)::cls ->
                      if s = ps then eval env es
                      else match_rec cls in
                match_rec cls
            | V.Empty ->
                error (get_loc e) "matching on empty"
            | V.Unv ->
                error (get_loc e) "matching on universe"
            | _ ->
                error (get_loc e) "matching on non-tag value of type '%s'"
                  (pp_typ (type_val v))
            end
        | MatchSet (loc,e,ife,(x,xs,ex)) ->
            let v = eval env e in
            begin match v with
            | V.Empty -> eval env ife
            | V.Unv ->
                error loc
                  "%s" "Cannot set-match on universe"
            | V.ValSet (t,s) ->
                if ValSet.is_empty s then
                  eval env ife
                else
                  let elt =
                    lazy begin
                      try ValSet.choose s
                      with Not_found -> assert false
                    end in
                  let s =
                    lazy begin
                      try ValSet (t,ValSet.remove (Lazy.force elt) s)
                      with  CompError _ -> assert false
                    end in
                  let env = add_val x elt env in
                  let env = add_val xs s env in
                  eval env ex
            | _ ->
                error (get_loc e) "set-matching on non-set value of type '%s'"
                  (pp_typ (type_val v))
            end

      and add_args xs es env_es env_clo =
        let vs = List.map (eval env_es) es in
        let bds =
          try
            List.combine xs vs
          with _ -> Warn.user_error "argument_mismatch" in
        List.fold_right
          (fun (x,v) env -> add_val x (lazy v) env)
          bds env_clo

      and eval_rel env e =  match eval env e with
      | Rel v -> v
      | _ -> error (get_loc e) "relation expected"

      and eval_set env e = match eval env e with
      | Set v -> v
      | V.Empty -> E.EventSet.empty
      | Unv -> ks.evts
      | _ -> error (get_loc e) "set expected"

      and eval_clo env e = match eval env e with
      | Clo v -> v
      | _ -> error (get_loc e) "closure expected"

      and eval_proc loc env x = match find_env_loc loc env x with
      | Proc p -> p
      | _ ->
          Warn.user_error "procedure expected"

(* For let *)
      and eval_bds env bds = match bds with
      | [] -> env
      | (k,e)::bds ->
          let v = eval env e in
          (*
            begin match v with
            | Rel r -> printf "Defining relation %s = {%a}.\n" k debug_rel r
            | Set s -> printf "Defining set %s = %a.\n" k debug_set s
            | Clo _ -> printf "Defining function %s.\n" k
            end;
           *)
          add_val k (lazy v) (eval_bds env bds)

(* For let rec *)

      and env_rec loc pp bds =
        let fs,nfs =  List.partition  (fun (_,e) -> is_fun e) bds in
        match fs,nfs with
        | [],bds -> env_rec_vals loc pp bds
        | bds,[] -> env_rec_funs loc bds
        | _,_ ->
            error loc "illegal recursion: mixing functions and other values"

(* Recursive functions *)
      and env_rec_funs _loc bds env =
        let clos =
          List.map
            (function
              | f,Fun (_,xs,body) ->
                  f,{ clo_args=xs; clo_env=env; clo_body=body;}
              | _ -> assert false)
            bds in
        let env =
          List.fold_left
            (fun env (f,clo) -> add_val f (lazy (Clo clo)) env)
            env clos in
        List.iter
          (fun (_,clo) -> clo.clo_env <- env)
          clos ;
        env

(* Compute fixpoint of relations *)
      and env_rec_vals loc pp bds =
        let rec fix  k env vs =
          if O.debug && O.verbose > 1 then begin
            let vb_pp =
              List.map2
                (fun (x,_) v ->
                  let v = match v with
                  | V.Empty -> E.EventRel.empty
                  | Unv -> Lazy.force ks.unv
                  | Rel r -> r
                  | _ -> E.EventRel.empty in
                  x, rt_loc x v)
                bds vs in
            let vb_pp = pp vb_pp in
            MU.pp_failure test conc
              (sprintf "Fix %i" k)
              vb_pp
          end ;
          let env,ws = fix_step env bds in
          let ok =
            try stabilised ks env vs ws
            with Stabilised t ->
              error loc "illegal recursion on type '%s'" (pp_typ t) in
          if ok then env
          else fix (k+1) env ws in
        fun env ->
          fix 0
            (List.fold_left
               (fun env (k,_) -> add_val k (lazy V.Empty) env)
               env bds)
            (List.map (fun _ -> V.Empty) bds)

      and fix_step env bds = match bds with
      | [] -> env,[]
      | (k,e)::bds ->
          let v = eval env e in
          let env = add_val k (lazy v) env in
          let env,vs = fix_step env bds in
          env,(v::vs) in

(* Showing bound variables, (-doshow option) *)

      let find_show_rel env x =
        try
          rt_loc x (as_rel ks (Lazy.force (StringMap.find x env.vals)))
        with Not_found -> E.EventRel.empty in

      let doshow bds st =
        let to_show =
          StringSet.inter S.O.PC.doshow (StringSet.of_list (List.map fst bds)) in
        if StringSet.is_empty to_show then st
        else
          let show = lazy begin
            StringSet.fold
              (fun x show  ->
                let r = find_show_rel st.env x in
                StringMap.add x r show)
              to_show
              (Lazy.force st.show)
          end in
          { st with show;} in

(* Execute one instruction *)

      let rec exec txt st i c =  match i with
      | Debug (_,e) ->
          let v = eval st.env e in
          eprintf "%a: value is %s\n"
            TxtLoc.pp (get_loc e) (pp_val v) ;
          run txt st c
      | Show (_,xs) ->
          let show = lazy begin
            List.fold_left
              (fun show x ->
                StringMap.add x (find_show_rel st.env x) show)
              (Lazy.force st.show) xs
          end in
          run txt { st with show;} c
      | UnShow (_,xs) ->
          let show = lazy begin
            List.fold_left
              (fun show x -> StringMap.remove x show)
              (Lazy.force st.show) xs
          end in
          run txt { st with show;} c
      | ShowAs (_,e,id) ->
          let show = lazy begin
            StringMap.add id
              (rt_loc id (eval_rel st.env e)) (Lazy.force st.show)
          end in
          run txt { st with show; } c
      | Test (_,pos,t,e,name,test_type) ->
          (* If this is a provides-clause and we've previously
             seen a requires-clause, abort. *)
          if st.seen_requires_clause && test_type = Provides then
            begin
              let pp = String.sub txt pos.pos pos.len in
              Warn.user_error
                "A provided condition must not come after an `undefined_unless' condition. Culprit: '%s'." pp
            end;
          (* If this is a requires-clause, record the fact that
             we have now seen at least one requires-clause. *)
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
            if ok then run txt st c
            else if skip_this_check then begin
              assert O.strictskip ;
              run txt
                { st with
                  skipped = StringSet.add (Misc.as_some name) st.skipped;}
                c
            end else begin
              if (O.debug && O.verbose > 0) then begin
                let pp = String.sub txt pos.pos pos.len in
                let cy = E.EventRel.get_cycle v in
                MU.pp_failure test conc
                  (sprintf "%s: Failure of '%s'" test.Test.name.Name.name pp)
                  (let k = show_to_vbpp st in
                  match cy with
                  | None -> k
                  | Some r -> ("CY",U.cycle_to_rel r)::k)
              end ;
              match test_type with
              | Provides ->
                  None
              | Requires ->
                  let () = failed_requires_clause () in
                  run txt st c
            end
          else begin
            W.warn "Skipping check %s" (Misc.as_some name) ;
            run txt st c
          end
      | Let (_,bds) ->
          let env = eval_bds st.env bds in
          let st = { st with env; } in
          let st = doshow bds st in
          run txt st c
      | Rec (loc,bds) ->
          let env =
            env_rec loc
              (fun pp -> pp@show_to_vbpp st)
              bds st.env in
          let st = { st with env; } in
          let st = doshow bds st in
          run txt st c
      | Include (loc,fname) ->
          (* Run sub-model file *)
          let module P = ParseModel.Make(LexUtils.Default) in
          let itxt,(_,_,iprog) =
            try P.parse fname
            with Misc.Fatal msg | Misc.UserError msg ->
              error loc "%s" msg  in
          begin match run itxt st iprog with
          | None -> None            (* Failure *)
          | Some st -> run txt st c (* Go on *)
          end
      | Procedure (_,name,args,body) ->
          let p =
            Proc { proc_args=args; proc_env=st.env; proc_body=body; } in
          run txt { st with env = add_val name (lazy p) st.env } c
      | Call (loc,name,es) ->
          let env0 = st.env in
          let p = eval_proc loc env0 name in
          let env1 = add_args p.proc_args es env0 p.proc_env in
          begin match run txt { st with env = env1; } p.proc_body with
          | None -> None
          | Some st_call ->
              run txt { st_call with env = env0; } c
          end
      | Enum (_loc,name,xs) ->
          let env = st.env in
          let tags =
            List.fold_left
              (fun env x -> StringMap.add x name env)
              env.tags xs in
          let enums = StringMap.add name xs env.enums in
          let env = { env with tags; enums; } in
          run txt { st with env;} c
      | Foreach (_loc,x,e,body) ->
          let env0 = st.env in
          let v = eval env0 e in
          begin match tag2set v with
          | V.Empty -> run txt st c
          | ValSet (_,set) ->
              begin try
                let st =
                  ValSet.fold
                    (fun v st ->
                      let env = add_val x (lazy v) st.env in
                      match run txt { st with env; } body with
                      | None -> raise Exit
                      | Some st -> { st with env=env0; })
                    set st in
                Some st
              with Exit -> None
              end
          | _ -> error (get_loc e) "foreach instruction applied to non-set value"
          end
      | Latex _ -> run txt st c

      and run txt st = function
        | [] ->  Some st
        | i::c -> exec txt st i c in

      let show =
        lazy begin
          let show =
            List.fold_left
              (fun show (tag,v) -> StringMap.add tag v show)
              StringMap.empty (Lazy.force vb_pp) in
          StringSet.fold
            (fun tag show -> StringMap.add tag (find_show_rel m tag) show)
            S.O.PC.doshow show
        end in
      run txt {env=m; show=show;
               seen_requires_clause=false;
               skipped=StringSet.empty;} prog



  end
