(*********************************************************************)
(*                          Offence                                  *)
(*                                                                   *)
(* Luc Maranget, jade Alglave INRIA Paris-Rocquencourt, France.      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module Permut = struct

  type t = Random | Permut of int list

  let tags = ["random"; "[<int>,]*";]

  let parse tag = match String.lowercase tag with
  | "random" -> Some Random
  | _ ->
      try Some (Permut (LexSplit.ints tag))
      with LexSplit.Error -> None

  let pp = function
    | Random -> "random"
    | Permut is -> LexSplit.pp_ints is
end

module Action = struct

  type t = Mix  | Append | Cat

  let tags = ["mix";"append";"cat";]

  let parse s = match String.lowercase s with
  | "mix" -> Some Mix
  | "append"|"app" -> Some Append
  | "cat" -> Some Cat
  | _ -> None

  let pp = function
    | Mix -> "mix"
    | Append -> "append"
    | Cat -> "cat"

end

module type S = sig
  val verbose : int
  val action : Action.t
  val permut : Permut.t
  val name : string option
end
