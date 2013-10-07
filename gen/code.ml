(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Event components *)
type loc = string
let ok = "ok"

type v = int
type proc = int

(* Direction of event *)
type dir = W | R

(* Edges compoments that do not depend on architecture *)

(* Change or proc accross edge *)
type ie = Int|Ext

(* Change of location across edge *)
type sd = Same|Diff

(* Direction of related events *)
type extr = Dir of dir | Irr 

(* Atomicity of events *)
type atom = Atomic | Plain | Reserve

(* Associated pretty print & generators *)
let pp_dir = function
  | W -> "W"
  | R -> "R"


let pp_ie = function
  | Int -> "i"
  | Ext -> "e"

let pp_extr = function
  | Dir d -> pp_dir d
  | Irr -> "*"

let pp_sd = function
  | Same -> "s"
  | Diff -> "d"

let pp_atom = function
  | Atomic -> "A"
  | Plain -> "P"
  | Reserve -> "R"

let fold_ie f r = f Ext (f Int r)
let fold_sd f r = f Diff (f Same r)
let fold_extr f r = f (Dir W) (f (Dir R) (f Irr r))
let fold_sd_extr f = fold_sd (fun sd -> fold_extr (fun e -> f sd e))
let fold_sd_extr_extr f =
  fold_sd_extr (fun sd e1 -> fold_extr (fun e2 -> f sd e1 e2))
let fold_atom f r = f Reserve (f Atomic (f Plain r))

type check =  Sc | Uni | Thin | Critical | Free | Ppo | Transitive | Total


