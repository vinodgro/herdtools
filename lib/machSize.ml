(*********************************************************************)
(*                        Diy                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

type sz = Byte | Short | Word | Quad

let pp = function
  | Byte -> "b"
  | Short -> "h"
  | Word -> "w"
  | Quad -> "q"

let nbytes = function
  | Byte -> 1
  | Short -> 2
  | Word -> 4
  | Quad -> 8
