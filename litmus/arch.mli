(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type Config = sig
  include ArchExtra.Config
end

module type Base = sig
  type reg

  include Location.S
    with type loc_reg = reg
     and type loc_global = string

  val parse_reg : string -> reg option
  val reg_compare : reg -> reg -> int

  type state = (location * MiscParser.Maybev.t) list

  module Out : Template.S
    with type arch_reg = reg

  val arch : Archs.t

  val find_in_state : location -> state -> MiscParser.Maybev.t
  val pp_reg : reg -> string
end

module type S =
  sig
    include ArchBase.S
    module V :
        sig
          type v = MiscParser.Maybev.t
          val maybevToV : v -> v
        end

    val reg_to_string : reg -> string

    include ArchExtra.S with module I.V = V
    and type I.arch_reg = reg

  end
