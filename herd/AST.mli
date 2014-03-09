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
type direction = 
  | Write | Read | WriteRead | Atomic | Plain
  | Unv_Set (* universal set *)
  | Bar_Set (* set of barriers *)
type op2 = 
  | Union | Inter | Diff (* these apply to sets and relations *)
  | Seq (* sequential composition of relations *) 
  | Cartesian (* build a relation from two sets *)
type op1 =
  | Plus | Star | Opt | Select of direction * direction
  | Inv  (* Relation inverse *)
  | Ext  (* External subrelation (events from <> threads) *)
  | Int  (* Internal subrelation (events from = threads) *)
  | NoId (* Irreflexive subrelation (<> events, aka r\id) *)
type konst = Empty_set | Empty_rel
type var = string

type scope = Device | Kernel | Work_Group | Sub_Group | Work_Item

type exp =
  | Konst of  konst
  | Var of var
  | Op1 of op1 * exp
  | Op of op2 * exp list
  | App of exp * exp list
  | Bind  of binding list * exp
  | BindRec  of binding list * exp
  | Fun of var list * exp

and binding = var * exp

type test = Acyclic | Irreflexive | TestEmpty

type test_type = Requires | Provides

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
