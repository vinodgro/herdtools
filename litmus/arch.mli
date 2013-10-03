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

module type S =
  sig
    include ArchBase.S
    module V :
        sig
          type v = Constant.v
          include Constant.S
          val maybevToV  : v -> v
        end

    val reg_to_string : reg -> string

    include ArchExtra.S with module I.V = V
    and type I.arch_reg = reg

  end
