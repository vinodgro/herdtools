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

(** Implementation of the action interface for machine models *)

open Printf

module type S = sig
  (* Module "A_" is really the same as "A". We just 
     need to pick a different name to pacify the 
     OCaml module system. Same goes for types 
     "action" and "action_" *)
  module A_ : Arch.S
  type action_ =    
    | Access of Dir.dirn * A_.location * A_.V.v * bool (* atomicity flag *)
    | Barrier of A_.barrier
    | Commit
  include Action.S with module A = A_ and type action = action_

end

module Make (A : Arch.S) : (S with module A_ = A) = struct

  module A = A
  module A_ = A
  module V = A.V
  open Dir

  type action_ = 
    | Access of dirn * A.location * V.v * bool 
          (* atomicity flag *)
    | Barrier of A.barrier
    | Commit
 
  type action = action_
  
  
  let mk_init_write l v = Access(W,l,v,false)

(* Local pp_location that adds [..] around global locations *)        
    let pp_location withparen loc =
      if withparen then sprintf "[%s]" (A.pp_location loc)
      else A.pp_location loc

  let pp_action withparen a = match a with
    | Access (d,l,v,ato) ->
	Printf.sprintf "%s%s%s=%s"
          (pp_dirn d)
          (pp_location withparen l)
	  (if ato then "*" else "")
	  (V.pp_v v)
    | Barrier b -> A.pp_barrier b
    | Commit -> "Commit"

(* Utility functions to pick out components *)
    let value_of a = match a with
    | Access (_,_ , v,_) -> Some v
    | _ -> None

    let read_of = value_of
    and written_of = value_of

    let location_of a = match a with
    | Access (_, l, _,_) -> Some l
    | _ -> None

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

    let is_atomic a = match a with
      | Access (_,_,_,true) -> 
	 assert (is_mem a); true
      | _ -> false

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
   let is_commit a = match a with
   | Commit -> true
   | _ -> false

(* Mutex operations *)
   let is_mutex_action _ = false

(* Architecture-specific sets *)
   let arch_sets = []

(* Equations *)

    let undetermined_vars_in_action a =
      match a with
      | Access (_,l,v,_) -> 
	  let undet_loc = match A.undetermined_vars_in_loc l with
	  | None -> V.ValueSet.empty
	  | Some v -> V.ValueSet.singleton v in
	  if V.is_var_determined v then undet_loc
	  else V.ValueSet.add v undet_loc
      | Barrier _|Commit -> V.ValueSet.empty

    let simplify_vars_in_action soln a =
      match a with
      | Access (d,l,v,ato) -> 
	 let l' = A.simplify_vars_in_loc soln l in
	 let v' = V.simplify_var soln v in
	 Access (d,l',v',ato)
      | Barrier _ | Commit -> a

(*************************************************************)	      
(* Add together event structures from different instructions *)
(*************************************************************)	 

    let make_action_atomic a = match a with
      | Access (d,l,v,_) -> Access (d,l,v,true)
      | _ -> a


end

