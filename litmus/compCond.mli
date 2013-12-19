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
module type X = sig
  type t
  val compare : t -> t -> int
  val dump : t -> string
end

module type I = sig
  module C : Constr.S
  module V : X with type t = Constant.v
  module Loc : X with type  t = C.A.location
end

module Make: functor (O:Indent.S) -> functor (I:I) ->
  sig
    val fundef :
        (I.Loc.t -> string) -> (* For types *)
          (I.Loc.t,I.V.t) ConstrGen.cond -> unit 
    val funcall :
        I.C.constr ->
          (I.Loc.t -> string) -> (string -> string) -> string
  end
