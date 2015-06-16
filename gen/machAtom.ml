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
module type Config = MachMixed.Config

module Make(C:Config) = struct

  module Mixed = MachMixed.Make(C)
  type hidden_atom = Atomic | Reserve | Mixed of MachMixed.t
  type atom = hidden_atom

  let default_atom = Atomic

  open Code

  let applies_atom a d = match a,d with
  | Reserve,W -> false
  | _,_ -> true

  let applies_atom_rmw ar aw = match ar,aw with
  | None,None -> true
  | _,_ -> false

  let pp_plain = Code.plain
  let pp_as_a = None

  let pp_atom = function
    | Atomic -> "A"
    | Reserve -> "R"
    | Mixed mix -> Mixed.pp_mixed mix

  let compare_atom = Pervasives.compare

  let fold_atom f r =
    let r = Mixed.fold_mixed (fun mix r -> f (Mixed mix) r) r in
    f Reserve (f Atomic r)

  let worth_final = function
    | Atomic -> true
    | Reserve -> false
    | Mixed _ -> false

  let tr_value ao v = match ao with
  | None| Some (Atomic|Reserve) -> v
  | Some (Mixed (sz,_)) -> Mixed.tr_value sz v

  let overwrite_value v ao w = match ao with
  | None| Some (Atomic|Reserve) -> w (* total overwrite *)
  | Some (Mixed (sz,o)) ->
      let sz_bits =  MachSize.nbytes sz * 8 in
      let nshift =  o * 8 in
      let wshifted = w lsl nshift in
      let mask = lnot (((1 lsl sz_bits) - 1) lsl nshift) in
      (v land mask) lor wshifted


end
