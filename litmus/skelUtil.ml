(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Skeleton utilities, useful for Skel and PreSi *)

module Make
    (P:sig type code end)
    (A:Arch.Base)
    (T:Test.S with type P.code = P.code and module A = A) : sig

(* Typing utilities *)
      type env
      val build_env : T.t -> env
      val find_type : A.location -> env -> CType.t
      val select_types : (A.location -> 'a option) -> env -> ('a * CType.t) list
      val select_proc : int -> env -> (A.reg * CType.t) list
      val select_global : env -> (A.loc_global * CType.t) list
(* Locations *)
      val get_final_locs : T.t -> A.LocSet.t
      val get_final_globals : T.t -> A.LocSet.t
(* Instructions *)
      val do_store : CType.t -> string -> string -> string
      val do_load : CType.t -> string -> string
end = struct

  open Printf

  type env = CType.t A.LocMap.t

  let build_env test =
    let e = A.LocMap.empty in
    let e =
      List.fold_left
        (fun e (s,t) -> A.LocMap.add (A.Location_global s) t e)
        e test.T.globals in
    let e = 
      List.fold_left
        (fun e (proc,(_,(outs, _))) ->
          List.fold_left
            (fun e  (reg,t) ->
              A.LocMap.add (A.Location_reg (proc,reg)) t e)
            e outs)
        e test.T.code in
    e

  let find_type loc env =
    try A.LocMap.find loc env
    with Not_found -> Compile.base

  let select_types f env =
    A.LocMap.fold
      (fun loc t k -> match f loc with
      | Some r -> (r,t)::k
      | None -> k)
      env []

  let select_proc (p:int) env =
    select_types
      (function
        | A.Location_reg (q,reg) when p = q -> Some reg
        | A.Location_global _ | A.Location_reg _ -> None)
      env

  let select_global env =
    select_types
      (function
        | A.Location_reg _ -> None
        | A.Location_global loc -> Some loc)
      env

(* Locations *)
      let get_final_locs t =
        A.LocSet.union
          (T.C.locations t.T.condition)
          (A.LocSet.of_list t.T.flocs)

  let get_final_globals t =
    A.LocSet.filter
      (function
        | A.Location_global _ -> true
        | A.Location_reg _ -> false)
      (get_final_locs t)

(* Instructions *)
  let do_store t loc v =
    if CType.is_atomic t then
      sprintf "atomic_store_explicit(&%s,%s,memory_order_relaxed)" loc v
    else
      sprintf "%s = %s" loc v

  let do_load t loc =
    if CType.is_atomic t then
      sprintf "atomic_load_explicit(&%s,memory_order_relaxed)" loc
    else loc

end
