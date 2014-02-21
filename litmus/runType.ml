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

type t = Ty of string | Pointer of string

let dump = function
  | Ty x -> x
  | Pointer x -> x ^ "*"

(* Awfull ack TODO : more explict atomics *)
let is_atomic = function
  | Pointer _ -> false
  | Ty s ->
      String.length s >= 6 && String.sub s 0 6 = "atomic"
