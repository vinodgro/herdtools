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

module type Config = sig
  include SemExtra.Config
end

module type Semantics =
  sig
    include SemExtra.S

(* Barrier pretty print (for minimal model) *)
    val barriers : pp_barrier list
    val isync : pp_barrier option

(* Instruction semantics, highly arch dependant *)
    val build_semantics  : 
    int list (*real proc list*) -> instruction -> A.inst_instance_id -> branch M.t
  end

