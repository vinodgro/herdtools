(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Atomicity of events *)
type atom = Atomic | Reserve

let default_atom = Atomic

open Code

let applies_atom a d = match a,d with
| Reserve,Dir W -> false
| _,_ -> true

let sig_of_atom = function
  | Atomic -> 'A'
  | Reserve -> 'B'

let pp_atom = function
  | Atomic -> "A"
  | Reserve -> "R"

let compare_atom = Pervasives.compare

let fold_atom f r = f Reserve (f Atomic r)
