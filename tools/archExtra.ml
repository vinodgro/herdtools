(*********************************************************************)
(*                          DIY                                      *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


module Make (O:sig val hexa : bool end)(A:ArchBase.S) = struct
  open Printf
  include A
  type v = SymbConstant.v
  let maybevToV c = c
  let pp_v = SymbConstant.pp

  type global = SymbConstant.v
  let maybevToGlobal c = c

  type location = 
    | Location_global of global
    | Location_reg of int * A.reg
          
  let pp_location = function
    | Location_global c -> SymbConstant.pp O.hexa c
    | Location_reg (i,r) -> sprintf "%i:%s" i (pp_reg r)

  let pp_rval = function
    | Location_global c -> sprintf "*%s" (SymbConstant.pp O.hexa c)
    | Location_reg (i,r) -> sprintf "%i:%s" i (pp_reg r)

    type test = (location,v,pseudo) MiscParser.r3
end
