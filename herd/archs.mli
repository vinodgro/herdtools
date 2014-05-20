(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** All architectures *)

type t =
  | X86
  | PPC
  | ARM
  | C
  | OpenCL
  | GPU_PTX

val tags : string list
val parse : string -> t option
val lex : string -> t
val pp : t -> string


val arm : t
val ppc : t
val x86 : t
val c : t
val opencl : t
val gpu_ptx : t

