(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


module type S = sig
  module A : Arch.Base

  type prop = (A.location, MiscParser.Maybev.t) ConstrGen.prop
  type constr = prop ConstrGen.constr

(* List of read locations *)
  val locations : constr -> A.location list
(* List locations that appears as  values *)
  val location_values : constr -> string list
end

open ConstrGen

module Make(A : Arch.Base) : S with module A = A  =
  struct
    module A = A

    type prop = (A.location, MiscParser.Maybev.t) ConstrGen.prop
    type constr = prop ConstrGen.constr

    module LocSet =
      MySet.Make
        (struct
          type t = A.location
          let compare = A.location_compare
        end)

    let locations_atom a r =
      let open ConstrGen in
      match a with
      | LV (loc,_) -> LocSet.add loc r
      | LL (loc1,loc2) -> LocSet.add loc1 (LocSet.add loc2 r)

    let locations c =
      let locs = fold_constr locations_atom c LocSet.empty in
      LocSet.elements locs

    module Strings = Set.Make(String)

    let location_values c =
      let locs =
        fold_constr
          (fun a k ->
            let open ConstrGen in
            match a with
            | LV (_,v) ->
                begin
                  match v with
                  | MiscParser.Maybev.Symbolic s -> Strings.add s k
                  | MiscParser.Maybev.Concrete _ -> k
                end
            | LL _ -> k)
          c Strings.empty in
      Strings.elements locs
  end
