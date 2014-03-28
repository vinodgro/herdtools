(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Locations, e.g. registers and memory cells *)

module type Config  = sig
  val texmacros : bool
end

module type I = sig
  type arch_reg
  val pp_reg : arch_reg -> string
  val reg_compare : arch_reg -> arch_reg -> int

  type arch_global
  val string_to_global : string -> arch_global
  val symbConstant_to_global : SymbConstant.v -> arch_global
  val maybev_to_global : MiscParser.Maybev.t -> arch_global
  val pp_global : arch_global -> string
  val global_compare : arch_global -> arch_global -> int
end

module type S = sig

 type loc_reg
 type loc_global

 type location =
    | Location_reg of int*loc_reg
    | Location_global of loc_global

  val string_to_location : string -> location
  val symbConstant_to_location : SymbConstant.v -> location
  val maybev_to_location : MiscParser.Maybev.t -> location
  val dump_location : location -> string (* Just dump *)
  val pp_location : location -> string
  val location_compare : location -> location -> int
  val location_equal : location -> location -> bool

  module LocSet : MySet.S with type elt = location
end

module Make(C:Config)(A:I) : S
with type loc_reg = A.arch_reg and type loc_global = A.arch_global =
  struct

    type loc_reg = A.arch_reg
    type loc_global = A.arch_global

    type location =
      | Location_reg of int*loc_reg
      | Location_global of loc_global

    let string_to_location m = Location_global (A.string_to_global m)
    let symbConstant_to_location m = Location_global (A.symbConstant_to_global m)
    let maybev_to_location m = Location_global (A.maybev_to_global m)

    let dump_location = function
      | Location_reg (proc,r) ->
          string_of_int proc ^ ":" ^ A.pp_reg r
      | Location_global a -> A.pp_global a

    let pp_location l = match l with
    | Location_reg (proc,r) ->
	let bodytext = string_of_int proc ^ ":" ^ A.pp_reg r in
	if C.texmacros
	then "\\asm{Proc " ^ bodytext ^ "}" else bodytext
    | Location_global a -> A.pp_global a


(*
  The following compare  comes from ancient code
  that used that order to pretty print states.
  I guess I can use it for ordering keys in maps
 *)
    let pair_compare cmpx x1 x2 cmpy y1 y2 = match cmpx x1 x2 with
    | 0 -> cmpy y1 y2
    | r -> r

    let location_compare l1 l2 = match l1,l2 with
    | Location_reg (p1,r1), Location_reg (p2,r2) ->
        pair_compare Misc.int_compare p1 p2 A.reg_compare r1 r2
    | Location_global a1, Location_global a2 -> A.global_compare a1 a2
    | Location_reg _, Location_global _ -> -1
    |  Location_global _, Location_reg _ -> 1

    let location_equal l1 l2 = location_compare l1 l2 = 0

    module LocSet =
      MySet.Make
        (struct
          type t = location
          let compare = location_compare
        end)
  end
