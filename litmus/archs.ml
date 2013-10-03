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

(*********)
(* Archs *)
(*********)

type t =
  | X86
  | PPC
  | ARM
  | PPCGen

let tags = ["X86";"PPC";"ARM";"PPCGen";]

let parse s = match s with
| "X86" -> Some X86
| "PPC" -> Some PPC
| "PPCGen" -> Some PPCGen
| "ARM" -> Some ARM
| _ -> None

let lex s = match parse s with
| Some a -> a
| None -> assert false


let pp a = match a with
| X86 -> "X86"
| PPC -> "PPC"
| PPCGen -> "PPCGen"
| ARM -> "ARM"

