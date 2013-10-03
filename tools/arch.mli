(*********************************************************************)
(*                       DIY                                         *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* We need this to apply symbolic register renaming (used in mprog) *)

module type S = sig
  include ArchBase.S
  module V :
      sig
        type v = Constant.v
        include Constant.S
        val maybevToV  : v -> v
      end
  type location = 
    | Location_global of Constant.v
    | Location_reg of int * reg

  val maybev_to_location : V.v -> location
  val pp_location : location -> string
  val pp_rval : location -> string
end
