(*********************************************************************)
(*                        Diy                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type I = sig
  type v
  val dump_v : v -> string

  type loc
  val dump_loc : loc -> string

end

module Make: functor (I:I) ->
  sig
    val dump : out_channel -> (I.loc,I.v) ConstrGen.prop -> unit 
  end
