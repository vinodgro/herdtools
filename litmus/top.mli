(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type CommonConfig = sig
  val verbose : int
  val limit : bool
  val timeloop : int
  val stride : int option
  val avail : int option
  val runs : int
  val size : int
  val isync : bool
  val speedcheck : Speedcheck.t
  val safer : Safer.t
  val cautious : bool
  val preload : Preload.t
  val memory : Memory.t
  val prealloc : bool
  val launch : Launch.t
  val barrier : Barrier.t
  val linkopt : string
  val logicalprocs : int list option
  val affinity : Affinity.t
  val targetos : TargetOS.t
  val is_out : bool
  val sleep : int
  val driver : Driver.t
  val crossrun : Crossrun.t
  val gcc : string
  val xy : bool
  val pldw : bool
  val morearch : MoreArch.t
  val syncconst : int
  val signaling : bool
  val numeric_labels : bool
  val kind : bool
  val force_affinity : bool
  val smtmode : Smt.t
  val smt : int
  val contiguous : bool
  val syncmacro : int option
  val collect : Collect.t
  val hexa : bool
  val verbose_barrier : bool
  val verbose_prelude : bool
  val check_kind : string -> ConstrGen.kind option
  val check_cond : string -> string option
  val cross : bool
  val tarname : string
  val hint : string option
  val no : string option
  val index : string option
end

module type TopConfig = sig
  include CommonConfig
  val check_name : string -> bool
  val check_rename : string -> string option
(* Arch dependent options *)
  val mkopt : Option.opt -> Option.opt
end

(* Compile/Run tests *)
module Top(OT:TopConfig)(Tar:Tar.S) : sig
  val from_files : string list -> unit
end
