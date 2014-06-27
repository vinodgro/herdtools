(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

type t = { co : bool ; init : bool } 

let default = {co=true; init=false;}

let pp { co; init; } = match co,init with
| true,true -> "[withco withinit]"
| true,false -> ""
| false,_ -> "[withoutco]"


let set_enumco b t =
  if not b then { co=false; init=true; }
  else { t with co=b; }

let set_init b t =
  if not t.co then t else { t with init=b; }
