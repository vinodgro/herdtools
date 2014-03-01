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


open Printf

module Make (A : Arch.S) : (Action.S with module A = A) = 
struct
  module A = A
  module V = A.V
  open Dir

  type action = 
    | Access of dirn * A.location * V.v * CPP11Base.mem_order
    | Barrier of A.barrier
    | RMW of A.location * V.v * V.v * CPP11Base.mem_order
    | Blocked_RMW of A.location
    | Lock of A.location * bool (* true = success, false = blocked *)
    | Unlock of A.location
 
  let mk_Access_CPP11 (d,l,v,mo) = Access (d,l,v,mo)
  let mk_Barrier b = Barrier b
  let mk_RMW (l,v1,v2,mo) = RMW (l,v1,v2,mo)
  let mk_Blocked_RMW l = Blocked_RMW l
  let mk_Lock (l,o) = Lock (l,o)
  let mk_Unlock l = Unlock l

  (* Unused constructors *)
  let mk_Access _ = assert false
  let mk_Commit   = assert false

(* Local pp_location that adds [..] around global locations *)        
    let pp_location withparen loc =
      if withparen then sprintf "[%s]" (A.pp_location loc)
      else A.pp_location loc

  let pp_action withparen a = match a with
    | Access (d,l,v,mo) ->
	sprintf "%s(%s)%s=%s"
          (pp_dirn d)
          (CPP11Base.pp_mem_order mo)
          (pp_location withparen l)
	  (V.pp_v v)
    | Barrier b -> A.pp_barrier b
    | RMW (l,v1,v2,mo) ->
       	sprintf "RMW(%s)%s(%s>%s)"
          (CPP11Base.pp_mem_order mo)
          (pp_location withparen l)
	  (V.pp_v v1) (V.pp_v v2)
    | Blocked_RMW l ->
       sprintf "BRMW%s"
	  (pp_location withparen l)
    | Lock (l,o) ->
      sprintf "L%s%s"
	(if o then "S" else "B")
        (pp_location withparen l)
    | Unlock l ->
      sprintf "U%s"
        (pp_location withparen l)

(* Utility functions to pick out components *)
    let value_of a = match a with
    | Access (_,_ , v,_) -> Some v
    | _ -> None

    let location_of a = match a with
    | Access (_, l, _,_) 
    | Lock (l,_)
    | Unlock l
    | RMW (l,_,_,_)
    | Blocked_RMW l -> Some l
    | _ -> None

    let location_reg_of a = match a with
    | Access (_,A.Location_reg (_,r),_,_) -> Some r
    | _ -> None

    let global_loc_of a = match a with
    | Access (_,A.Location_global loc,_,_) -> Some loc
    | _ -> None

    let location_compare a1 a2 = match location_of a1,location_of a2 with
    | Some loc1,Some loc2 -> 
	A.location_compare loc1 loc2
    | _,_ -> assert false

(* relative to memory *)
    let is_mem_store a = match a with
    | Access (W,A.Location_global _,_,_) -> true
    | _ -> false

    let is_mem_load a = match a with
    | Access (R,A.Location_global _,_,_) -> true
    | _ -> false

    let is_mem a = match a with
    | Access (_,A.Location_global _,_,_) -> true
    | _ -> false

    let is_atomic _ = assert false

    let get_mem_dir a = match a with
    | Access (d,A.Location_global _,_,_) -> d
    | _ -> assert false

(* relative to the registers of the given proc *)
    let is_reg_store a (p:int) = match a with
    | Access (W,A.Location_reg (q,_),_,_) -> p = q
    | _ -> false

    let is_reg_load a (p:int) = match a with
    | Access (R,A.Location_reg (q,_),_,_) -> p = q
    | _ -> false

    let is_reg a (p:int) = match a with
    | Access (_,A.Location_reg (q,_),_,_) -> p = q
    | _ -> false


(* Store/Load anywhere *)
    let is_store a = match a with
    | Access (W,_,_,_) -> true
    | _ -> false

    let is_load a = match a with
    | Access (R,_,_,_) -> true
    | _ -> false

    let is_reg_any a = match a with
    | Access (_,A.Location_reg _,_,_) -> true
    | _ -> false

    let is_reg_store_any a = match a with
    | Access (W,A.Location_reg _,_,_) -> true
    | _ -> false

    let is_reg_load_any a = match a with
    | Access (R,A.Location_reg _,_,_) -> true
    | _ -> false

(* Barriers *)
    let is_barrier a = match a with
    | Barrier _ -> true
    | _ -> false

    let barrier_of a = match a with
    | Barrier b -> Some b
    | _ -> None

(* Commits *)
   let is_commit _ = assert false

(* Equations *)

    let undetermined_vars_in_action a =
      match a with
      | Access (_,l,v,_) -> 
	  let undet_loc = match A.undetermined_vars_in_loc l with
	  | None -> V.ValueSet.empty
	  | Some v -> V.ValueSet.singleton v in
	  if V.is_var_determined v then undet_loc
	  else V.ValueSet.add v undet_loc
      | RMW(l,v1,v2,_) ->
         let undet_loc = match A.undetermined_vars_in_loc l with
	   | None -> V.ValueSet.empty
	   | Some v -> V.ValueSet.singleton v in
         let undet_loc = 
	   (if V.is_var_determined v1 then undet_loc 
	    else V.ValueSet.add v1 undet_loc) in
         let undet_loc =
           (if V.is_var_determined v2 then undet_loc
	    else V.ValueSet.add v2 undet_loc) in
         undet_loc
      | Blocked_RMW l
      | Lock(l,_) 
      | Unlock l -> 
	 (match A.undetermined_vars_in_loc l with
	  | None -> V.ValueSet.empty
	  | Some v -> V.ValueSet.singleton v) 
      | Barrier _ -> V.ValueSet.empty

    let simplify_vars_in_action soln a =
      match a with
      | Access (d,l,v,mo) -> 
	 let l' = A.simplify_vars_in_loc soln l in
	 let v' = V.simplify_var soln v in
	 Access (d,l',v',mo)
      | RMW(l,v1,v2,mo) ->
        let l' = A.simplify_vars_in_loc soln l in
        let v1' = V.simplify_var soln v1 in
	let v2' = V.simplify_var soln v2 in
        RMW(l',v1',v2',mo)
      | Blocked_RMW l ->
	 let l' = A.simplify_vars_in_loc soln l in
        Blocked_RMW l'
      | Lock(l,o) ->
        let l' = A.simplify_vars_in_loc soln l in
        Lock(l',o)
      | Unlock l  ->
        let l' = A.simplify_vars_in_loc soln l in
        Unlock l'
      | Barrier _ -> a

(*************************************************************)	      
(* Add together event structures from different instructions *)
(*************************************************************)	 

    let make_action_atomic _ = assert false

end

