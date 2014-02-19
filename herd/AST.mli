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
type direction = Write | Read | WriteRead | Atomic | Plain | Filter of string list | Unv_Set | Bar_Set
type op2 = Union | Inter | Seq | Diff
type op1 = Plus | Star | Opt | Comp | Inverse | Select of direction * direction
type ext_int = External | Internal
type scope = Device | Kernel | Work_Group | Sub_Group | Work_Item
type var = string

type exp =
  | Empty_rel 
  | Empty_set
  | Scope_op of scope * ext_int
  | Var of var
  | Op1 of op1 * exp
  | Op of op2 * exp list
  | Cartesian of exp * exp
  | App of exp * exp list
  | Bind  of binding list * exp
  | BindRec  of binding list * exp
  | Fun of var list * exp

and binding = var * exp

type test = Acyclic | Irreflexive | TestEmpty

type test_type = Provides | Requires


type ins =
  | Let of binding list
  | Rec of binding list
  | Test of pos * test * exp * string option * test_type
  | UnShow of string list
  | Show of string list
  | ShowAs of exp * string

(* Name X model definition *)
type t = bool * string * ins list
type pp_t = string * t
