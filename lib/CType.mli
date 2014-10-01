(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* The subset of C types that we use *)

type base = string

type t =
  | Base of base
  | Volatile of t
  | Atomic of t
  | Pointer of t
(** OpenCL *)
  | Global of t
  | Local of t

val dump : t -> string

val is_ptr : t -> bool
val is_atomic : t -> bool
val is_global : t -> bool
val is_local : t -> bool
val is_private : t -> bool
val strip_atomic : t -> t

val strip_volatile : t -> t
val is_ptr_to_atomic : t -> bool
val is_ptr_to_global : t -> bool
val is_ptr_to_local : t -> bool
val is_ptr_to_private : t -> bool
val is_mutex : t -> bool
