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

(* The basic types of architectures and semantics, just parsed *)

type maybev = SymbConstant.v

type reg = string (* Registers not yet parsed *)

type location =
  | Location_reg of int * reg
  | Location_sreg of string
  | Location_global of maybev

let location_compare loc1 loc2 = match loc1,loc2 with
| Location_reg (i1,r1), Location_reg (i2,r2) ->
    begin match Misc.int_compare i1 i2 with
    | 0 -> String.compare r1 r2
    | c -> c
    end
| Location_sreg r1,Location_sreg r2 ->
    String.compare r1 r2
| Location_global v1,Location_global v2 ->
    SymbConstant.compare v1 v2
| Location_reg _,(Location_sreg _|Location_global _) -> -1
| (Location_sreg _|Location_global _),Location_reg _ -> 1
| Location_sreg _, Location_global _ -> -1
| Location_global _, Location_sreg _ -> 1

let dump_location = function
  | Location_reg (i,r) -> Printf.sprintf "%i:%s" i r
  | Location_sreg s -> s
  | Location_global v -> SymbConstant.pp_v v

let dump_rval loc = match loc with
  | Location_reg (i,r) -> Printf.sprintf "%i:%s" i r
  | Location_sreg s -> s
  | Location_global v -> Printf.sprintf "*%s" (SymbConstant.pp_v v)

let is_global = function
  | Location_global _ -> true
  | Location_reg _ -> false
  | _ -> assert false

let as_local_proc i = function
  | Location_reg (j,reg) -> if i=j then Some reg else None
  | Location_global _ -> None
  | _ -> assert false


module LocSet =
  MySet.Make
    (struct type t = location let compare = location_compare end)

type prop = (location, maybev) ConstrGen.prop
type constr = prop ConstrGen.constr
type quantifier = ConstrGen.kind

type atom = location * maybev
type state = atom list
type outcome = atom list

open Printf

let pp_atom (loc,v) =
  sprintf "%s=%s" (dump_location loc) (SymbConstant.pp_v v)

let pp_outcome o =
  String.concat " "
    (List.map (fun a -> sprintf "%s;" (pp_atom a)) o)

type run_type = Ty of string | Pointer of string

(*********************************)
(* GPU memory map and scope tree *)
(*********************************)

type gpu_memory_space = 
| Global
| Shared

let pp_gpu_memory_space x =
  match x with
  | Global -> "global"
  | Shared -> "shared"

type thread      = int
type warp        = thread list
type cta         = warp list
type kernel      = cta list
type device      = kernel list

type scope_tree = 
| Scope_tree of device list
| No_scope_tree
    
let rec create_list total acc l = 
  if acc = total then l
  else
    create_list total (acc+1) l@[acc]
      
(*The default scope tree for CPUs (used in -ext and -int operator in herd)*)
let cpu_scope_tree n = 
  let w = (create_list n 0 []) in 
  let cta = List.map (fun x -> [x]) w in
  let ker = List.map (fun x -> [x]) cta in
  let dev = List.map (fun x -> [x]) ker in
  let top = List.map (fun x -> [x]) dev in
  Scope_tree(top)
  
  

type mem_space_map = 
| Mem_space_map of (string * gpu_memory_space) list
| No_mem_space_map

let pp_warp w =
  let mapped_list = List.map (fun (i) -> sprintf "P%d" i) w in
  "(warp " ^(String.concat " " mapped_list) ^ ")"

let pp_cta c =
  let mapped_list = List.map (fun (w) -> pp_warp w) c in
  "(cta " ^(String.concat " " mapped_list) ^ ")"

let pp_kernel k =
  let mapped_list = List.map (fun (c) -> pp_cta c) k in
  "(kernel " ^(String.concat " " mapped_list) ^ ")"

let pp_device k =
  let mapped_list = List.map (fun (c) -> pp_kernel c) k in
  "(device " ^(String.concat " " mapped_list) ^ ")"

let pp_scope_tree s = 
  let mapped_list = List.map (fun (k) -> pp_device k) s in
  String.concat " " mapped_list
  

let pp_scope_tree s = 
  match s with
  | Scope_tree s ->  pp_scope_tree s
  | _ -> ""

let pp_memory_space_map m = 
  match m with 
  | Mem_space_map m -> let str_list = 
			 List.map 
			   (fun (x,y) -> sprintf "%s:%s" x (pp_gpu_memory_space y)) m 
		       in
		       String.concat "; " str_list
		       

  | _ -> ""


(* Packed result *)
type info = (string * string) list
type ('i, 'p, 'c, 'loc) result =
    { info : info ;
      init : 'i ;
      prog : 'p ;
      condition : 'c ;
      locations : ('loc * run_type) list;
      scope_tree : scope_tree ;
      mem_space_map : mem_space_map ;
}

(* Easier to handle *)
type ('loc,'v,'ins) r3 =
      (('loc * 'v) list,
       (int * 'ins list) list,
       ('loc, 'v) ConstrGen.prop ConstrGen.constr,
       'loc) result

type ('loc,'v,'code) r4 =
      (('loc * 'v) list,
       'code list,
       ('loc, 'v) ConstrGen.prop ConstrGen.constr,
       'loc) result 

(* Result of generic parsing *)
type 'pseudo t =
    (state, (int * 'pseudo list) list, constr, location) result

let get_hash p =
  try List.assoc "Hash" p.info
  with Not_found -> assert false
