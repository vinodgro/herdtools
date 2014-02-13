(*********************************************************************)
(*                        Diy                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Base type for produced tests *)

type t = Int | Short | Char 

let tags =  ["int";"short";"char";]

let parse s = match s with
| "int" -> Some Int
| "short" -> Some Short
| "char" -> Some Char
| _ -> None

let pp = function
  | Int -> "int"
  | Short -> "short"
  | Char -> "char"
