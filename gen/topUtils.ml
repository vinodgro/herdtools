(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

module type Config = sig
  val optcoherence : bool
  val do_observers : Config.do_observers
  val obs_type : Config.obs_type
  val poll : bool
end

module Make : functor (O:Config) -> functor (C:ArchRun.S) ->
  sig
(* Coherence utilities *)
    type cos0 =  (Code.loc * (C.C.node * Ints.t) list list) list
    type cos = (Code.loc * (Code.v * Ints.t) list list) list
    val pp_coherence : cos0 -> unit
    val last_map : cos0 -> C.C.event StringMap.t
    val compute_cos : cos0 ->  cos
(* prefetch *)
    type pt = { ploc:Code.loc ; pdir:Code.dir; } (* In thread/Out thread *)
    val io_of_thread : C.C.node list -> (pt * pt) option
    val io_of_detour : C.C.node -> (pt * pt) option
    val compile_prefetch_ios : int ->  (pt * pt) option list -> string
(* affinity *)
    val compile_coms : C.C.node list list -> string list
(* Misc *)
    val comp_loc_writes : C.C.node -> StringSet.t
    val check_here : C.C.node -> bool
    val do_poll : C.C.node -> bool
  end = 
  functor (O:Config) -> functor (C:ArchRun.S) ->
  struct

    type cos0 =  (Code.loc * (C.C.node * Ints.t) list list) list
    type cos = (Code.loc * (Code.v * Ints.t) list list) list

    open Printf
    open Code

    let pp_coherence cos0 =
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
        
(****************************)
(* Last in coherence orders *)
(****************************)
    let rec find_last = function
      | [] -> assert false
      | [xs] -> Misc.last xs
      | _::xss -> find_last xss


    let do_last_map cos =
      let lsts =
        List.map
          (fun (loc,xss) ->
            let r,_ = find_last xss in
            loc,r)
          cos in
      List.fold_left
        (fun m (loc,lst) -> StringMap.add loc lst.C.C.evt m)
        StringMap.empty lsts

    let last_map cos = match O.do_observers with
    | Config.Local when O.optcoherence -> do_last_map cos
    | _ -> StringMap.empty


    let compute_cos =
      List.map
        (fun (loc,ns) ->
          loc,
          List.map
            (List.map (fun (n,obs) -> n.C.C.evt.C.C.v,obs))
            ns)
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
          let open C.E in
          match last_edge ns with
          | Fr _|Leave CFr|Back CFr -> "Fr"
          | Rf _|RfStar _|Leave CRf|Back CRf -> "Rf"
          | Ws _|Leave CWs|Back CWs -> "Ws"
          | _ -> assert false)
        nss


(********)
(* Misc *)
(********)

(* Local writes *)            

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
        if C.E.is_detour n.C.C.edge  then StringSet.add n.C.C.evt.C.C.loc k
        else k in
      do_rec n0

(* insert local check *)

    let is_load_init e =
      e.C.C.dir = R && e.C.C.v = 0

    let check_here n = match n.C.C.edge.C.E.edge with
    | C.E.Ws Ext | C.E.DetourWs _
    | C.E.Fr Ext
    | C.E.Leave (CFr|CWs)
    | C.E.Back(CFr|CWs)  -> not (is_load_init n.C.C.evt)
    | _ -> false

(* Poll for value is possible *)
    let do_poll n =
      match O.poll,n.C.C.prev.C.C.edge.C.E.edge,n.C.C.evt.C.C.v with
      | true,
        (C.E.Rf Ext|C.E.Leave CRf|C.E.Back CRf),1 -> true
      | _,_,_ -> false   
  end
