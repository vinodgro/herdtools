(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(*               Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Semantics of MIPS instructions *)

module Make (C:Sem.Config)(V:Value.S)
=
  struct
    module MIPS = MIPSArch.Make(C.PC)(V)
    module Act = MachAction.Make(MIPS)
    include SemExtra.Make(C)(MIPS)(Act)

(* Barrier pretty print *)
    let sync = {barrier=MIPS.Sync; pp="sync";}
        
    let barriers = [sync;]
    let isync = None


    let build_semantics _ii = assert false
end
