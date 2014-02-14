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

val default_atom : atom
val sig_of_atom : atom -> char
val applies_atom : atom -> Code.extr -> bool

val pp_atom : atom -> string
val compare_atom : atom -> atom -> int

val fold_atom : (atom -> 'a -> 'a) -> 'a -> 'a
