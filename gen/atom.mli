(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

module type S = sig
  type atom
  val default_atom : atom
  val applies_atom : atom -> Code.extr -> bool
  val compare_atom : atom -> atom -> int
  val sig_of_atom : atom -> char
  val pp_as_a : atom option
  val pp_atom : atom -> string
  val fold_atom : (atom -> 'a -> 'a) -> 'a -> 'a
end
