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

(** Syntax tree of model definitions *)

type pos = { pos:int; len:int;}

type set_or_rln = SET | RLN

type direction = 
  | Write | Read | WriteRead | Atomic | Plain
  | Unv_Set (** universal set *)
  | Bar_Set (** set of barriers *)
type op2 = 
  | Union (** applies to sets or relations *)
  | Inter (** applies to sets or relations *) 
  | Diff  (** applies to sets or relations *)
  | Seq   (** sequential composition of relations *) 
  | Cartesian (** build a relation from two sets *)
type op1 =
  | Plus | Star | Opt  | Select of direction * direction
  | Comp of set_or_rln (** Set or relation complement *)
  | Inv  (** Relation inverse *)
  | Square (** x^2 is shorthand for x * x *)
  | Ext  (** External subrelation (events from <> threads) *)
  | Int  (** Internal subrelation (events from = threads) *)
  | NoId (** Irreflexive subrelation (<> events, aka r\id) *)
  | Set_to_rln (** Convert a set to a relation (viz. identity restricted to that set) *)
type konst = Empty of set_or_rln
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
  | Latex of string
  | Include of string (* file name, interpreter will read/parse file... *)
(** Name X model definition *)
type t = ModelOption.t * string * ins list
type pp_t = string * t
