(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type S = sig
  type arch_reg
  type t

  val dump :
    out_channel ->
    string ->
    (arch_reg * RunType.t) list ->
    (string * RunType.t) list ->
    string list ->
    int ->
    t ->
    unit
end
