(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Alternative test generation, for PreSi *)
open Printf

module type Config = sig
  val verbose : int
  val hexa : bool
  val driver : Driver.t
  val word : Word.t
  val line : int
  val logicalprocs : int list option
  val smt : int
  val nsockets : int
  val smtmode : Smt.t
  val force_affinity : bool
  val numeric_labels : bool
  val delay : int
  val c11 : bool
  include DumpParams.Config
end

module Make
         (Cfg:sig include Config val sysarch : Archs.System.t end)
         (P:sig type code end)
         (A:Arch.Base)
         (T:Test.S with type P.code = P.code and module A = A)
         (O:Indent.S)
         (Lang:Language.S with type arch_reg = T.A.reg and type t = A.Out.t) : sig
  val dump : Name.t -> T.t -> unit
end = struct


(*******************************************)
(* Set compile time parameters from config *)
(*******************************************)

 let have_timebase = function
  | `ARM -> false
  | `PPCGen
  | `PPC|`X86 -> true
  | _ -> false

  let have_timebase = have_timebase Cfg.sysarch

(* Utilities *)
  module U = SkelUtil.Make(P)(A)(T)

(***************)
(* File header *)
(***************)

 let dump_header test =
    O.o "/* Parameters */" ;
    let module D = DumpParams.Make(Cfg) in
    D.dump O.o ;
    let n = T.get_nprocs test in
    O.f "#define N %i" n ;
    let nvars = List.length test.T.globals in
    O.f "#define NVARS %i" nvars ;
    let nexe =
      match Cfg.avail  with 
      | None -> 1
      | Some a -> if a < n then 1 else a / n in
    O.f "#define NEXE %i" nexe ;
    O.f "#define NTHREADS %i" (nexe*n) ;
    if have_timebase then begin
      let delta = sprintf "%i" Cfg.delay in
      if have_timebase then O.f "#define DELTA_TB %s" delta
    end ;
    O.o "/* Includes */" ;
    O.o "#include <stdio.h>" ;
    O.o "#include <stdlib.h>" ;
    O.o "#include <inttypes.h>" ;
    O.o "#include <unistd.h>" ;
    O.o "#include <errno.h>" ;
    O.o "#include <assert.h>" ;
    O.o "#include <time.h>" ;
    O.o "#include <limits.h>" ;
    O.o "#include \"utils.h\"" ;
    if Cfg.c11 then O.o "#include <stdatomic.h>";
    O.o "#include \"affinity.h\"" ;
    O.o "" ;
    O.o "typedef uint64_t count_t;" ;
    O.o "#define PCTR PRIu64" ;
    O.o "" ;
    ()

(**********)
(* Delays *)
(**********)

let nsteps = 5

let dump_delay_def () =
  O.f "#define NSTEPS %i\n" nsteps ;
  O.o "#define STEP (DELTA_TB/(2*(NSTEPS-1)))" ;
  ()

(***************************************)
(* Various inclusions from C utilities *)
(***************************************)

  module Insert =
    ObjUtil.Insert
      (struct
        let sysarch = Cfg.sysarch
        let word = Cfg.word
      end)

(* Time base *)
  let dump_read_timebase () =
    if have_timebase then begin
      O.o "/* Read timebase */" ;
      O.o "typedef uint64_t tb_t ;" ;
      O.o "#define PTB PRIu64" ;
      Insert.insert O.o "timebase.c"
    end

(* Memory barrier *)
  let dump_mbar_def () =
    O.o "/* Full memory barrier */" ;
    Insert.insert O.o "mbar.c" ;
    O.o ""

(* Cache *)
  let dump_cache_def () =
    O.o "/* Cache flush/fetch instructions */" ;
    Insert.insert O.o "cache.c" ;
    O.o ""



(* Synchronisation barrier *)
  let lab_ext = if Cfg.numeric_labels then "" else "_lab"

  let dump_barrier_def () =
    let fname =
      function
        | `PPCGen
        | `PPC
        | `X86
        | `ARM ->
            sprintf "barrier%s.c" lab_ext 
        | _ -> assert false in
    Insert.insert O.o (fname Cfg.sysarch)

(**************)
(* Topologies *)
(**************)

  let dump_topology test =
    let n = T.get_nprocs test in
    let module Topo =
      Topology.Make
        (struct
          let verbose = Cfg.verbose
          let nthreads = n
          let avail = match Cfg.avail with
          | None -> 0
          | Some a -> a

          let smt = Cfg.smt
          let nsockets = Cfg.nsockets
          let smtmode = Cfg.smtmode
          let mode = Mode.PreSi
        end) (O) in
    O.o "/************/" ;
    O.o "/* Topology */" ;
    O.o "/************/" ;
    O.o "" ;
    Topo.dump_alloc ()

(************)
(* Outcomes *)
(************)

let dump_loc_tag = function
  | A.Location_reg (proc,reg) -> A.Out.dump_out_reg proc reg
  | A.Location_global s -> s

(* Collected locations *)

  let fmt_outcome env locs =
    let pp_loc loc =  match loc with
    | A.Location_reg (proc,reg) ->
        sprintf "%i:%s" proc (A.pp_reg reg)
    | A.Location_global s -> s in

    let rec pp_fmt t = match t with
    | CType.Pointer _ -> "%s"
    | CType.Base t ->
        let fmt = Compile.get_fmt Cfg.hexa t in
        if Cfg.hexa then "0x%" ^ fmt else "%" ^ fmt
    | CType.Atomic t|CType.Volatile t -> pp_fmt t
    | CType.Global _|CType.Local _ -> assert false in

    "\"" ^
    A.LocSet.pp_str " "
      (fun loc -> sprintf "%s=%s;"
          (pp_loc loc) (pp_fmt (U.find_type loc env)))
      locs ^
    "\""

  let dump_outcomes env test =
    let locs = U.get_final_locs test in
    O.o "/************/" ;
    O.o "/* Outcomes */" ;
    O.o "/************/" ;
    O.o "" ;
    O.o "typedef struct {" ;
    A.LocSet.iter
      (fun loc ->
        let t = U.find_type loc env in
        O.fi "%s %s;" (CType.dump t) (dump_loc_tag loc))
      locs ;
    O.o "} log_t;" ;
    O.o "" ;

    O.o "/* Dump of outcome */" ;
    O.o "static void pp_log(FILE *chan,log_t *p) {" ;
    let fmt = fmt_outcome env locs
    and args =
      A.LocSet.map_list (fun loc -> sprintf "p->%s" (dump_loc_tag loc)) locs in
    O.fi "fprintf(chan,%s);" (String.concat "," (fmt::args)) ;            
    O.o "}" ;
    O.o "" ;
    let locs = A.LocSet.elements locs in (* Now use lists *)
    O.o "/* Equality of outcomes */" ;
    O.o "static int eq_log(log_t *p,log_t *q) {" ;
    O.oi "return" ;
    let do_eq loc suf =
      let loc = dump_loc_tag loc in
      O.fii "p->%s == q->%s%s" loc loc suf in
    let rec do_rec = function
      | [] -> O.oii "1;" ;
      | [x] -> do_eq x ";"
      | x::rem  -> do_eq x " &&" ; do_rec rem in
    do_rec  locs ;
    O.o "}" ;
    O.o "" ;

    O.o "/* Hash of outcome */" ;
    ObjUtil.insert_lib_file O.o "_mix.h" ;
    O.o "" ;
    O.o "static uint32_t hash_log(log_t *p) {" ;
    O.oi "uint32_t a,b,c; ";
    O.oi "a = b = c = 0xdeadbeef;" ;
    let rec do_rec = function
      | [] -> ()
      | [x] ->
          O.fi "a += p->%s;" (dump_loc_tag x) ;
          O.oi "final(a,b,c);"
      | [x;y;] ->
          O.fi "a += p->%s;" (dump_loc_tag x) ;
          O.fi "b += p->%s;" (dump_loc_tag y) ;
          O.oi "final(a,b,c);"          
      | [x;y;z;] ->
          O.fi "a += p->%s;" (dump_loc_tag x) ;
          O.fi "b += p->%s;" (dump_loc_tag y) ;
          O.fi "c += p->%s;" (dump_loc_tag z) ;
          O.oi "final(a,b,c);"
      | x::y::z::rem ->
          O.fi "a += p->%s;" (dump_loc_tag x) ;
          O.fi "b += p->%s;" (dump_loc_tag y) ;
          O.fi "c += p->%s;" (dump_loc_tag z) ;
          O.oi "mix(a,b,c);" ;
          do_rec rem in
    do_rec locs ;
    O.oi"return c;" ;
    O.o "}" ;
    O.o "" ;
    ()

 module DC =
    CompCond.Make(O)
      (struct
        open Constant
        module C = T.C
        module V = struct
          type t = Constant.v
          let compare = A.V.compare
          let dump = function
            | Concrete i -> sprintf "%i" i
            | Symbolic s -> sprintf "p->%s" s
        end
        module Loc = struct
          type t = A.location
          let compare = A.location_compare
          let dump loc = sprintf "p->%s" (dump_loc_tag loc)
        end
      end)


  let dump_cond_fun env test =    
    let cond = test.T.condition in
    DC.fundef_onlog cond

  let _dump_cond_fun_call test dump_loc dump_val =
    DC.funcall (test.T.condition) dump_loc dump_val

  let dump_cond_def env test =
    O.o "/* Condition check */" ;
    dump_cond_fun env test ;
    O.o "" ;
    ()

(**************)
(* Parameters *)
(**************)
  let dump_param_tag s = s ^"p"

  let dump_parameters env test =
    let v_tags = List.map (fun (s,_) -> dump_param_tag s)  test.T.globals
    and d_tags =
      List.map (sprintf "d%i") (Misc.interval 0 (T.get_nprocs test)) in
      
    O.o "/**************/" ;
    O.o "/* Parameters */" ;
    O.o "/**************/" ;
    O.o "" ;
    O.o "typedef struct {" ;
    O.oi "int part;" ;
    O.fi "int %s;" (String.concat "," v_tags) ;
    O.fi "int %s;" (String.concat "," d_tags) ;
    O.o "} param_t;" ;
    O.o "" ;
    let all_tags = "part"::v_tags@d_tags in
    O.o "static void pp_param(FILE *fp,param_t *p) {" ;
    let fmt =
      "\"{" ^
      String.concat ", " (List.map (fun _ -> "%i") all_tags) ^
      "}\""
    and params = List.map (sprintf "p->%s") all_tags  in
    O.fi "fprintf(fp,%s);" (String.concat "," (fmt::params)) ;     
    O.o "}" ;
    O.o ""
      
(*************)
(* Hashtable *)
(*************)

  let hash_max = 128 * 1024 

  let hash_size n =
    let rec c_rec n k =
      if n <= 0 then k
      else
        let k = 3 * k in
        if k > hash_max then hash_max
        else c_rec (n-1) k in
    c_rec n 2

  let dump_hash_def env test =
    let locs = U.get_final_locs test in
    O.f "#define HASHSZ %i" (hash_size (A.LocSet.cardinal locs)) ;
    O.o "" ;
    ObjUtil.insert_lib_file O.o "_hash.c" ;
    O.o ""

(*****************)
(* Test instance *)
(*****************)

  let dump_instance_def env test =
    O.o "/***************/" ;
    O.o "/* Memory size */" ;
    O.o "/***************/" ;
    O.o "" ;
    O.o "/* Cache line */" ;
    O.f "#define LINE %i" Cfg.line ;
    O.o "" ;
    O.o "/************/" ;
    O.o "/* Instance */" ;
    O.o "/************/" ;
    ObjUtil.insert_lib_file O.o "_instance.c" ;
    O.o "" ;
    ()
(***************)
(* Entry point *)
(***************)

  let dump doc test =
    dump_header test ;
    dump_delay_def () ;
    dump_read_timebase () ;
    dump_mbar_def () ;
    dump_cache_def () ;
    dump_barrier_def () ;
    dump_topology test ;
    let env = U.build_env test in
    dump_outcomes env test ;
    dump_cond_def env test ;
    dump_parameters env test ;
    dump_hash_def env test ;
    dump_instance_def env test ;
    ()
    
end
