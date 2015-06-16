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

module Make : functor (C:Config) ->
  sig
    type hidden_atom = Atomic | Reserve | Mixed of MachMixed.t
    include Atom.S with type atom = hidden_atom
  end
