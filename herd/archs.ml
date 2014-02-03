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
  | CPP11
  | OpenCL
  | GPU_PTX

let tags = ["X86";"PPC";"ARM";"CPP11";"OpenCL";"GPU_PTX"]

let parse s = match s with
| "X86" -> Some X86
| "PPC" -> Some PPC
| "ARM" -> Some ARM
| "CPP11" -> Some CPP11
| "OpenCL" -> Some OpenCL
| "GPU_PTX" -> Some GPU_PTX
| _ -> None

let lex s = match parse s with
| Some a -> a
| None -> assert false


let pp a = match a with
| X86 -> "X86"
| PPC -> "PPC"
| ARM -> "ARM"
| CPP11 -> "C++11"
| OpenCL -> "OpenCL"
| GPU_PTX -> "GPU_PTX"

let arm = ARM
let ppc = PPC
let x86 = X86
let cpp11 = CPP11
let opencl = OpenCL
let gpu_ptx = GPU_PTX
