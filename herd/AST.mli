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

(* Syntax tree of model definition *)
type pos = { pos:int; len:int;}
type direction = Write | Read | WriteRead | Atomic | Plain
type op2 = Union | Inter | Seq | Diff
type op1 = Plus | Star | Opt | Select of direction * direction
type konst = Empty

type exp =
  | Konst of  konst
  | Var of string
  | Op1 of op1 * exp
  | Op of op2 * exp list

type test = Acyclic | Irreflexive | TestEmpty

type binding = (string * exp)
type ins =
  | Let of binding list
  | Rec of binding list
  | Test of pos * test * exp * string option
  | UnShow of string list
  | Show of string list
  | ShowAs of exp * string

(* Name X model definition *)
type t = string * ins list
type pp_t = string * t
