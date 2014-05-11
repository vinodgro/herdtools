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
  | Location_sreg _ -> assert false

let as_local_proc i = function
  | Location_reg (j,reg) -> if i=j then Some reg else None
  | Location_global _ -> None
  | Location_sreg _ -> assert false


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


(* Packed result *)
type info = (string * string) list
type gpu_data = {
      scope_tree : ScopeTree.scope_tree option ;
      mem_space_map : MemSpaceMap.mem_space_map ;
      lk_map : LocationKindMap.lk_map ;
  }

let empty_gpu =
  { scope_tree = None; mem_space_map = []; lk_map = []; }

type ('i, 'p, 'c, 'loc) result =
    { info : info ;
      init : 'i ;
      prog : 'p ;
      condition : 'c ;
      locations : ('loc * run_type) list ;
      gpu_data : gpu_data option ;
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
(* type 'pseudo t =
    (state, (int * 'pseudo list) list, constr, location) result *)

type 'code t =
    (state, 'code list, constr, location) result

let get_hash p =
  try List.assoc "Hash" p.info
  with Not_found -> assert false
