(*********************************************************************)
(*                        Diy                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type I = sig
  type v
  val dump_v : v -> string

  type loc
  val dump_loc : loc -> string

end

module Make(I:I) :
  sig
    val dump : out_channel -> (I.loc,I.v) ConstrGen.prop -> unit  
  end = struct
    open Printf
    open ConstrGen

    let dump  =
      let rec dump_prop chan p = match p with
      | Atom (LV (loc,v)) ->
          fprintf chan "%s == %s" (I.dump_loc loc) (I.dump_v v)
      | Atom (LL (loc1,loc2)) ->
          fprintf chan "%s == %s" (I.dump_loc loc1) (I.dump_loc loc2)
      | Not p -> fprintf chan "!(%a)" dump_prop p
      | Or [] -> fprintf chan "0"
      | Or [p] -> dump_prop chan p
      | Or (p::ps) ->
          fprintf chan "(%a) || (%a)" dump_prop p dump_prop (Or ps)
      | And [] -> fprintf chan "1"
      | And [p] -> dump_prop chan p
      | And (p::ps) ->
          fprintf chan "(%a) && (%a)" dump_prop p dump_prop (And ps)
      | Implies (p1,p2) ->
          fprintf chan "!(%a) || (%a)" dump_prop p1 dump_prop p2 in
      dump_prop
  end
