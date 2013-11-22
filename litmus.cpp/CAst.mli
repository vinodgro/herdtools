(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* TODO: Remove CAst.ty and use RunType.t instead ? *)
type ty =
  | Int_ptr

type param = { param_ty : ty; param_name : string }

type body = string

type t =
  { proc : int
  ; params : param list
  ; body : body
  }
