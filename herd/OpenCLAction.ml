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

(** Define OpenCL actions *)

open Printf

module type S = sig
  (* Module "A_" is really the same as "A". We just 
     need to pick a different name to pacify the 
     OCaml module system. Same goes for types 
     "action" and "action_" *)
  module A_ : Arch.S
  type action_ =    
    | Access of Dir.dirn * A_.location * A_.V.v * OpenCLBase.mem_order * OpenCLBase.mem_scope
    | Fence of OpenCLBase.gpu_memory_space * OpenCLBase.mem_order * OpenCLBase.mem_scope
    | RMW of A_.location * A_.V.v * A_.V.v * OpenCLBase.mem_order * OpenCLBase.mem_scope
    | Blocked_RMW of A_.location
  include Action.S with module A = A_ and type action = action_

end

module Make (A : Arch.S) : (S with module A_ = A) = 
struct
  module A = A
  module A_ = A
  module V = A.V
  open Dir

  type action_ = 
    | Access of dirn * A.location * V.v * OpenCLBase.mem_order * OpenCLBase.mem_scope
    | Fence of OpenCLBase.gpu_memory_space * OpenCLBase.mem_order * OpenCLBase.mem_scope
    | RMW of A.location * V.v * V.v * OpenCLBase.mem_order * OpenCLBase.mem_scope
    | Blocked_RMW of A.location
  type action = action_
 
  let mk_init_write l v = Access (W,l,v,OpenCLBase.NA,OpenCLBase.S_all_svn_devices)

(* Local pp_location that adds [..] around global locations *)        
    let pp_location withparen loc =
      if withparen then sprintf "[%s]" (A.pp_location loc)
      else A.pp_location loc

  let pp_action withparen a = match a with
    | Access (d,l,v,mo,s) ->
	sprintf "%s(%s,%s)%s=%s"
          (pp_dirn d)
          (OpenCLBase.pp_mem_order mo)
          (OpenCLBase.pp_mem_scope s)
          (pp_location withparen l)
	  (V.pp_v v)
    | Fence (mr,mo,s) -> 
       sprintf "F(%s,%s,%s)"
         (OpenCLBase.pp_gpu_memory_space mr)
         (OpenCLBase.pp_mem_order mo)
         (OpenCLBase.pp_mem_scope s)
    | RMW (l,v1,v2,mo,s) ->
       	sprintf "RMW(%s,%s)%s(%s>%s)"
          (OpenCLBase.pp_mem_order mo)
          (OpenCLBase.pp_mem_scope s)
          (pp_location withparen l)
	  (V.pp_v v1) (V.pp_v v2)
    | Blocked_RMW l ->
       sprintf "BRMW%s"
	  (pp_location withparen l)

(* Utility functions to pick out components *)
    let value_of a = match a with
    | Access (_,_ , v,_,_) -> Some v
    | _ -> None

    let location_of a = match a with
    | Access (_, l, _,_,_) 
    | RMW (l,_,_,_,_)
    | Blocked_RMW l -> Some l
    | _ -> None

    let location_reg_of a = match a with
    | Access (_,A.Location_reg (_,r),_,_,_) -> Some r
    | _ -> None

    let global_loc_of a = match a with
    | Access (_,A.Location_global loc,_,_,_) -> Some loc
    | _ -> None

    let location_compare a1 a2 = match location_of a1,location_of a2 with
    | Some loc1,Some loc2 -> 
	A.location_compare loc1 loc2
    | _,_ -> assert false

(* relative to memory *)
    let is_mem_store a = match a with
    | Access (W,A.Location_global _,_,_,_) -> true
    | _ -> false

    let is_mem_load a = match a with
    | Access (R,A.Location_global _,_,_,_) -> true
    | _ -> false

    let is_mem a = match a with
    | Access (_,A.Location_global _,_,_,_) -> true
    | _ -> false

    (* The following definition of is_atomic
       is quite arbitrary. *)
    let is_atomic a = match a with
    | Access (_,A.Location_global _,_,mo,_) -> mo != OpenCLBase.NA
    | RMW _ -> true
    | _ -> false

    let get_mem_dir a = match a with
    | Access (d,A.Location_global _,_,_,_) -> d
    | _ -> assert false

(* relative to the registers of the given proc *)
    let is_reg_store a (p:int) = match a with
    | Access (W,A.Location_reg (q,_),_,_,_) -> p = q
    | _ -> false

    let is_reg_load a (p:int) = match a with
    | Access (R,A.Location_reg (q,_),_,_,_) -> p = q
    | _ -> false

    let is_reg a (p:int) = match a with
    | Access (_,A.Location_reg (q,_),_,_,_) -> p = q
    | _ -> false


(* Store/Load anywhere *)
    let is_store a = match a with
    | Access (W,_,_,_,_) -> true
    | _ -> false

    let is_load a = match a with
    | Access (R,_,_,_,_) -> true
    | _ -> false

    let is_reg_any a = match a with
    | Access (_,A.Location_reg _,_,_,_) -> true
    | _ -> false

    let is_reg_store_any a = match a with
    | Access (W,A.Location_reg _,_,_,_) -> true
    | _ -> false

    let is_reg_load_any a = match a with
    | Access (R,A.Location_reg _,_,_,_) -> true
    | _ -> false

(* Barriers *)
    let is_barrier _ = false
    let barrier_of _ = None

(* Commits *)
   let is_commit _ = false

(* Fences *)
   let is_fence a = match a with
     | Fence _ -> true
     | _ -> false

   let is_global_fence a = match a with
     | Fence (OpenCLBase.GlobalMem,_,_) -> true
     | _ -> false

   let is_local_fence a = match a with
     | Fence (OpenCLBase.LocalMem,_,_) -> true
     | _ -> false

(* RMWs *)
   let is_rmw a = match a with
     | RMW _ -> true
     | _ -> false

(* Blocked RMWs *)
   let is_blocked_rmw a = match a with
     | Blocked_RMW _ -> true
     | _ -> false

(* Mutex operations *)
   let is_mutex_action _a = false

   let mo_matches target a = match a with
     | Access(_,_,_,mo,_)
     | RMW (_,_,_,mo,_) 
     | Fence (_,mo,_) -> mo=target
     | _ -> false

   let scope_matches target a = match a with
     | Access(_,_,_,_,s)
     | RMW (_,_,_,_,s) 
     | Fence (_,_,s) -> s=target
     | _ -> false

(* Architecture-specific sets *)
   let arch_sets = [
     "rmw", is_rmw; 
     "brmw", is_blocked_rmw;
     "F", is_fence;
     "gF", is_global_fence;
     "lF", is_local_fence;
   ] @ List.map (fun (k,v) -> (k,mo_matches v)) [
     "acq", OpenCLBase.Acq;
     "sc", OpenCLBase.SC;
     "rel", OpenCLBase.Rel; 
     "acq_rel", OpenCLBase.Acq_Rel;
     "rlx", OpenCLBase.Rlx;
     "na", OpenCLBase.NA;
   ] @ List.map (fun (k,v) -> (k,scope_matches v)) [
     "wi", OpenCLBase.S_workitem;
     "sg", OpenCLBase.S_subgroup;
     "wg", OpenCLBase.S_workgroup;
     "dev", OpenCLBase.S_device;
     "all_dev", OpenCLBase.S_all_svn_devices;
   ]

(* Equations *)

    let undetermined_vars_in_action a =
      match a with
      | Access (_,l,v,_,_) -> 
	  let undet_loc = match A.undetermined_vars_in_loc l with
	  | None -> V.ValueSet.empty
	  | Some v -> V.ValueSet.singleton v in
	  if V.is_var_determined v then undet_loc
	  else V.ValueSet.add v undet_loc
      | RMW(l,v1,v2,_,_) ->
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
      | Blocked_RMW l -> 
	 (match A.undetermined_vars_in_loc l with
	  | None -> V.ValueSet.empty
	  | Some v -> V.ValueSet.singleton v) 
      | Fence _ -> V.ValueSet.empty

    let simplify_vars_in_action soln a =
      match a with
      | Access (d,l,v,mo,s) -> 
	 let l' = A.simplify_vars_in_loc soln l in
	 let v' = V.simplify_var soln v in
	 Access (d,l',v',mo,s)
      | RMW(l,v1,v2,mo,s) ->
        let l' = A.simplify_vars_in_loc soln l in
        let v1' = V.simplify_var soln v1 in
	let v2' = V.simplify_var soln v2 in
        RMW(l',v1',v2',mo,s)
      | Blocked_RMW l ->
	 let l' = A.simplify_vars_in_loc soln l in
        Blocked_RMW l'
      | Fence _ -> a

(*************************************************************)	      
(* Add together event structures from different instructions *)
(*************************************************************)	 

    let make_action_atomic _ = assert false

end

