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

module type Config = sig
  val verbose_prelude : bool
  val verbose_barrier : bool
  val hexa : bool
  val speedcheck : Speedcheck.t
  val driver : Driver.t
  val safer : Safer.t
  val cautious : bool
  val preload : Preload.t
  val word : Word.t
  val barrier : Barrier.t
  val collect : Collect.t
  val syncmacro : int option
  val isync : bool
  val memory : Memory.t
  val contiguous : bool
  val prealloc : bool
  val launch : Launch.t
  val affinity : Affinity.t
  val logicalprocs : int list option
  val smt : int
  val smtmode : Smt.t
  val force_affinity : bool
  val kind : bool
  val numeric_labels : bool
  val delay : int
  val signaling : bool
  val syncconst : int
  val morearch : MoreArch.t
  val carch : Archs.System.t Lazy.t
  val xy : bool
  val pldw : bool
  include DumpParams.Config
end

module type Utils = sig
  module T : Test.S

  module LocSet : MySet.S with type elt = T.A.location

  val do_affinity : bool
  val do_timebase : bool
  val have_timebase : bool
  val do_custom : bool
  val do_verbose_barrier_local : bool
  val do_collect_local : bool
  val do_safer : bool
  val do_verbose_barrier : bool
  val do_force_affinity : bool
  val do_check_globals : bool
  val barrier : Barrier.t
  val do_staticpl : bool
  val stride : int option
  val find_global_type : T.A.loc_global -> (T.A.location * RunType.t) list -> RunType.t
  val dump_global_type : RunType.t -> string
  val dump_type : RunType.t -> string
  val indent3 : Indent.t
  val loop_test_prelude : Indent.t -> string -> unit
  val indent : Indent.t
  val indent2 : Indent.t
  val dump_a_addr : string -> string
  val get_global_names : T.t -> string list
  val get_prefetch_info : T.t -> string
  val do_staticNpl : bool
  val do_isync : bool
  val get_final_locs : T.t -> LocSet.elt list
  val dump_cond_fun_call : T.t -> (T.C.A.location -> string) -> (string -> string) -> string
  val dump_ctx_loc : string -> T.A.location -> string
  val test_witness : T.t -> string -> string
  val dump_loc_name : T.A.location -> T.A.loc_global
  val find_type : T.A.location -> (T.A.location * RunType.t) list -> RunType.t
  val remark_pos : T.t -> bool
  val loop_test_postlude : Indent.t -> unit
  val do_collect_after : bool
  val have_finals_globals : T.t -> bool
end

module type S =
  functor (Cfg : Config) ->
  functor (O : Indent.S) ->
  functor (U : Utils) -> sig
    val dump_test : (U.T.A.location * RunType.t) list -> string -> U.T.t -> unit
  end
