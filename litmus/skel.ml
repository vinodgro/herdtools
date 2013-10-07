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

open Printf


let indent = Indent.indent
and indent2 = Indent.indent2
and indent3 = Indent.indent3
and indent4 = Indent.indent4


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
  val xy : bool
  val pldw : bool
  include DumpParams.Config
end

module Make(Cfg:Config) (T:Test.S) (O:Indent.S) : sig
  val dump : Name.t -> T.t -> unit
end = struct
  module A = T.A
  module C = T.C
  open Archs
  open Constant

(* Final Conditions *)

  open ConstrGen
  module LocSet =
    MySet.Make
      (struct
        type t = A.location
        let compare = A.location_compare
      end)

(* Options *)
  open Speedcheck

  let do_vp = Cfg.verbose_prelude
  let driver = Cfg.driver
  let do_speedcheck = match Cfg.speedcheck  with
  | NoSpeed -> false
  | SomeSpeed|AllSpeed -> true
  let do_safer = match Cfg.safer with
  | Safer.No -> false
  | Safer.All|Safer.Write -> true
  let do_safer_write = match Cfg.safer with
  | Safer.No -> false
  | Safer.All|Safer.Write -> true

  open Preload
  let do_randompl =
    match Cfg.preload with RandomPL -> true
    | NoPL|CustomPL|StaticPL|Static2PL -> false
  let do_custom =
    match Cfg.preload with CustomPL -> true 
    | Static2PL|StaticPL|NoPL|RandomPL -> false
  let do_staticpl =
    match Cfg.preload with StaticPL -> true
    | CustomPL|NoPL|RandomPL|Static2PL -> false
  let do_static2pl =
    match Cfg.preload with Static2PL -> true
    | CustomPL|NoPL|RandomPL|StaticPL -> false

  let ws = Cfg.word

  open Barrier
  let barrier =  Cfg.barrier
  let do_timebase = match barrier with
  | TimeBase -> true
  | _ -> false
  let have_timebase = match A.arch with
  | ARM -> false
  | PPCGen
  | PPC|X86 -> true

  let do_collect_local = match Cfg.collect with
  | Collect.Local|Collect.Both ->
      if do_timebase then true
      else Warn.user_error "Local collect requires timebase barrier"
  | Collect.After -> false

  let do_collect_after = match Cfg.collect with
  | Collect.After|Collect.Both -> true
  | Collect.Local -> false

  let do_collect_both = do_collect_local && do_collect_after

  let do_verbose_barrier = Cfg.verbose_barrier
  let do_verbose_barrier_local =
    do_verbose_barrier && do_collect_local && not do_collect_after

  let do_check_globals = do_collect_after && (do_safer || do_randompl)


  let do_sync_macro = match Cfg.syncmacro with
  | None -> false
  | Some _ -> true

  let sync_macro_n  = match Cfg.syncmacro with
  | None -> 0
  | Some n -> n

  let do_isync = Cfg.isync

  open Memory
  let memory = Cfg.memory
  let stride = Cfg.stride
  let do_contiguous = Cfg.contiguous
  let do_prealloc = Cfg.prealloc
  open Launch
  let launch = Cfg.launch      
  let affinity = Cfg.affinity
  let smt = Cfg.smt
  let smtmode = Cfg.smtmode

  let do_affinity = match affinity with
  | Affinity.No -> false
  | Affinity.Incr _|Affinity.Random|Affinity.Custom -> true

  let mk_dca test =
    let f test =
      if do_affinity then
        try      
          let res = List.assoc "Com" test.T.info in
          let coms =
            try LexAffinity.coms res
            with _ -> assert false in          
          let nprocs = List.length test.T.code in
          if nprocs <> List.length coms then None
          else if smt > 1 then match smtmode with
          | Smt.No -> None
          | Smt.Seq|Smt.End -> Some res 
          else None
        with Not_found -> None
      else None in
    match  f test with
    | None -> false,""
    | Some s -> true,s

  let do_cores =
    do_affinity &&
    smt > 1 &&
    (match smtmode with
    | Smt.No -> false
    | Smt.Seq|Smt.End -> true)

  let do_force_affinity = Cfg.force_affinity
  let do_kind = Cfg.kind
  let do_numeric_labels = Cfg.numeric_labels

(* Location utilities *)
  let get_global_names t = List.map fst t.T.globals
  let find_index  v =
    let rec find_rec k = function
      | [] -> assert false
      | w::ws ->          
          if String.compare v w = 0 then k
          else find_rec (k+1) ws in
    find_rec 0

  let get_final_locs t =
    LocSet.elements
      (LocSet.of_list (C.locations t.T.condition@t.T.flocs)) 

  let get_finals_globals t =
    List.filter
      (fun loc -> match loc with
      | A.Location_global _ -> true
      | _ -> false)
      (get_final_locs t)

  let find_global_init a t =
    A.find_in_state (A.Location_global a) t.T.init

  let have_finals_globals t = match get_finals_globals t with
  | [] -> false
  | _::_ -> true

  let dump_loc_name loc =  match loc with
  | A.Location_reg (proc,reg) -> A.Out.dump_out_reg  proc reg
  | A.Location_global s -> s
        
  let dump_loc_copy loc = "_" ^ dump_loc_name loc ^ "_i" 
  let dump_loc_param loc = "_" ^ dump_loc_name loc
  let dump_val_param loc =  "_val_" ^ loc

  let dump_ctx_loc pref loc = match loc with
  | A.Location_reg (proc,reg) ->
      sprintf "%s%s[_i]" pref (A.Out.dump_out_reg  proc reg)
  | A.Location_global s ->
      begin match memory with
      | Direct ->
          sprintf "%s%s[_i]" pref s
      | Indirect ->
          sprintf "*(%s%s[_i])" pref s
      end

  let dump_loc = dump_ctx_loc ""


(* Dump left value *)
  let dump_leftval a = match memory with
  | Direct -> sprintf "%s[_i]" a
  | Indirect -> sprintf "*(%s[_i])" a

(* Dump left & right values when pointer to context is available *)

(* Left value *)
  let dump_a_leftval a = match memory with
  | Direct -> sprintf "_a->%s[_i]" a
  | Indirect -> sprintf "*(_a->%s[_i])" a

        
(* Right value *)
  let dump_a_addr = match memory with
  | Direct -> sprintf "&(_a->%s[_i])"
  | Indirect -> sprintf "_a->%s[_i]"

  let dump_a_v = function
    | Concrete i ->  sprintf "%i" i
    | Symbolic s -> dump_a_addr s

(* Right value, casted if pointer *)
  let dump_a_v_casted = function
    | Concrete i ->  sprintf "%i" i
    | Symbolic s -> sprintf "((int *)%s)" (dump_a_addr s)

(* Dump left & right values when context is available *)

(* Left value *)
  let dump_ctx_tag = match memory with
  | Direct -> sprintf "%s"
  | Indirect -> sprintf "mem_%s"

        
(* Right value from ctx *)
  let dump_ctx_addr = match memory with
  | Direct -> sprintf "&(ctx.%s[_i])"
  | Indirect -> sprintf "ctx.%s[_i]"

(* Idem casted *)

  let build_env test =
    List.map
      (fun (s,t) -> A.Location_global s,t)
      test.T.globals @
    List.flatten
      (List.map
         (fun (proc,(_,outs)) ->
           List.map
             (fun (reg,t) -> A.Location_reg (proc,reg),t)
             outs)
         test.T.code)

  let rec find_type loc env = match env with
  | [] -> RunType.Int
  | (loc0,t)::env ->
      if A.location_compare loc loc0 = 0 then t
      else find_type loc env

  let find_global_type a = find_type (A.Location_global a)

(* Test condition *)
  let dump_cond_v = function
    | Concrete i -> sprintf "%i" i
    | Symbolic s -> dump_val_param s

  let remark_pos test = match test.T.condition with
  | ForallStates _ -> false
  | ExistsState _
  | NotExistsState _
    -> true

  let test_witness test v = match test.T.condition with
  | ForallStates _
  | ExistsState _ -> v
  | NotExistsState _ -> "!" ^ v

  let dump_condition dump_loc c =
    let rec dump_prop chan p = match p with
    | Atom (LV (loc,v)) ->
        fprintf chan "%s == %s" (dump_loc loc) (dump_cond_v v)
    | Atom (LL (loc1,loc2)) ->
        fprintf chan "%s == %s" (dump_loc loc1) (dump_loc loc2)
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

    let dump_def p = O.fi "cond = %a;" dump_prop p in
    match c with
    | ForallStates p
    | ExistsState p
    | NotExistsState p ->
        dump_def p

  let dump_header test =
    O.o "/* Parameters */" ;
    let module D = DumpParams.Make(Cfg) in
    D.dump O.o ;
    let n = T.get_nprocs test in
    O.f "#define N %i" n ;
    O.f "#define AFF_INCR (%i)"
      (match affinity with
      | Affinity.Incr i -> i
      | Affinity.Random|Affinity.Custom -> 0
      | Affinity.No -> -1) ;
    if do_timebase then begin
      let delta = sprintf "%i" Cfg.delay in
      if have_timebase then O.f "#define DELTA_TB %s" delta
    end ;
    O.o "/* Includes */" ;
    O.o "#include <stdio.h>" ;
    O.o "#include <stdlib.h>" ;
    O.o "#include <unistd.h>" ;
    O.o "#include <errno.h>" ;
    O.o "#include <assert.h>" ;
    O.o "#include <time.h>" ;
    O.o "#include <limits.h>" ;
    O.o "#include \"utils.h\"" ;
    O.o "#include \"outs.h\"" ;
    if do_affinity then O.o "#include \"affinity.h\"" ;
    O.o "" ;
    O.o "/* params */" ;
    O.o "typedef struct {" ;
    O.oi "int verbose;" ;
    O.oi "int size_of_test,max_run;" ;
    if do_verbose_barrier then O.oi "int verbose_barrier;" ;
    begin match stride with
    | None -> ()
    | Some _ -> O.oi "int stride;"
    end ;
    if Cfg.timeloop > 0 then O.oi "int max_loop;" ;
    if Cfg.signaling then O.oi "int sig_cell, *sig_addr;" ;
    if do_affinity then O.oi "int aff_rand, ncpus, ncpus_used;" ;
    if do_speedcheck then O.oi "int speedcheck, stop_now;" ;
    begin match memory with
    | Direct -> ()
    | Indirect -> O.oi "int do_shuffle;"
    end ;
    begin match launch with
    | Changing -> O.oi "int do_change;"
    | Fixed -> ()
    end ;
    if do_timebase && have_timebase then O.oi "int *delays;" ;
    if do_custom then O.oi "prfdirs_t *prefetch;" ;
    if do_staticpl then O.oi "unsigned int static_prefetch;" ;
    if do_cores then O.oi "cpus_t *cm;" ;
    if  do_sync_macro then O.oi "int max_idx;" ;
    O.o "} param_t;" ;
    O.o"" ;
    if do_sync_macro then begin
      O.f "#define SYNC_K %i" Cfg.syncconst ;
      O.f "#define SYNC_N %i" sync_macro_n ;
      ()
    end ;
    ()


(*************************************************)
(* Soft barriers: to synchronize test iterations *)
(*************************************************)

(*
  This barrier is very sensitive to machine load.

  1- With unlimited spinning running time becomes huge when
  there are more threads than processors.
  test on conti (2 procs..)
  2- with settings 1000 * 200 for instance on conti, running
  times become erratic. Maybe because a daemon running literally
  stops the test: one proc is runnable while the other is spinning.
  With higher sizes, running times are less variable from one
  run to the other.

  Running the test under low priority (nice) also demonstrates the effect:
  running time doubles.

  However this is the best barrier we have, in normal conditions,
  especially with high SIZE_OF_TEST parameter. A benefit of
  such a high setting resides in pthread costs (lauch/join/fst_barrier) being
  amortized over a larger number of tests. It also often increases outcome
  variety, maybe by introduction of capacity cache misses.

  Limiting spinning is a BAD idea, it hinders outcome variety, even in
  normal load conditions.
 *)

type fence = No | FenceW | Fence2

let user_barrier_def fence =
  O.o "/* Barriers macros */" ;
  O.o "inline static void barrier_wait(unsigned int id, unsigned int k, int volatile *b) {" ;
  O.oi "if ((k % N) == id) {" ;
  O.oii "*b = 1 ;" ;
  begin match fence with
  | No -> if Cfg.cautious then O.oii "mcautious();"
  | FenceW|Fence2 -> O.oii "mbar();"
  end ;
  O.oi "} else {" ;
  O.oii "while (*b == 0) ;" ;
  begin match fence with
  | No|FenceW -> if Cfg.cautious then O.oii "mcautious();"
  | Fence2 -> O.oii "mbar();"
  end ;
  O.oi "}" ;
  O.o "}"

let user2_barrier_def () =
  O.o "/* Barriers macros, compact version */" ;
  O.o "inline static void barrier_wait(unsigned int id, unsigned int k, int volatile *b) {" ;
  O.oi "unsigned int x = k % (2*N);" ;
  O.oi "int free = x < N;" ;
  O.oi "unsigned idx =  k % N;" ;          
  O.oi "if (idx == id) {" ;
  O.oii "b[idx] = free;" ;
  O.oi "} else {" ;
  O.oii "while (b[idx] != free);" ;
  O.oi "}" ;
  if Cfg.cautious then O.oi "mcautious();" ;
  O.o "}"

let dump_read_timebase () =
  if (do_verbose_barrier || do_timebase) && have_timebase then begin
    O.o "typedef uint64_t tb_t ;" ;
    O.o "#define PTB PRIu64" ;
    O.o "" ;
    begin match A.arch with
    | X86 ->
       O.o "inline static tb_t read_timebase(void) {" ;
       O.oi "unsigned int a,d;" ;
       O.oi "asm __volatile__ (\"rdtsc\" : \"=a\" (a), \"=d\" (d));" ;
       O.oi "return ((tb_t)a) | (((tb_t)d)<<32);" ;
       O.o "}"
    | PPCGen
    | PPC ->
       O.oi "tb_t r;" ;
       O.o "inline static tb_t read_timebase(void) {" ;
       begin match ws with
       | Word.W32|Word.WXX ->
         let asm s = O.o (sprintf "\"%s\\n\\t\"" s) in
         O.oi "uint32_t r1,r2,r3;" ;
         O.o "asm __volatile__ (" ;
         asm "0:" ;
         asm "mftbu %[r1]" ;
         asm "mftb %[r2]" ;
         asm "mftbu %[r3]" ;
         asm "cmpw %[r1],%[r3]" ;
         asm "bne 0b" ;
         O.o ":[r1] \"=r\" (r1), [r2] \"=r\" (r2), [r3] \"=r\" (r3)" ;
         O.o ": :\"memory\" );" ;
         O.oi "r = r2;" ;
         O.oi "r |= ((tb_t)r1) << 32;" ;
         ()
       | Word.W64->
          O.oi "asm __volatile__ (\"mftb %[r1]\" :[r1] \"=r\" (r) : : \"memory\");"
       end ;
       O.oi "return r;" ;
       O.o "}"
   | ARM -> assert false
    end ;
   end

let lab_ext = if do_numeric_labels then "" else "_lab"

  let dump_tb_barrier_def () =
    let fname =
      match A.arch with
      | PPCGen
      | PPC -> sprintf "_ppc_barrier%s.c" lab_ext
      | X86 -> sprintf "_x86_barrier%s.c" lab_ext
      | ARM ->
          begin match Cfg.morearch with
          | MoreArch.ARMv6K ->
              Warn.fatal
                "timebase barrier not supported for ARMv6K" ;
          | _ -> ()
          end ;
          sprintf "_arm_barrier%s.c" lab_ext in
    ObjUtil.insert_lib_file O.out fname

  let dump_user_barrier_vars () = O.oi "int volatile *barrier;"

  let dump_tb_barrier_vars () =
    O.oi "sense_t barrier;" ;
    if have_timebase then O.oi "tb_t volatile next_tb;"


(*******************************************)
(* Pthread utilies, included hard barriers *)
(*******************************************)

  let pthread_barrier_def () =
    O.o DefString.pthread_barrier_def

  let dump_pthread_barrier_vars () = O.oi "barrier_t *barrier;"

  let dump_barrier_vars () =
    O.o "/* Barrier for litmus loop */" ;
    begin match barrier with
    | NoBarrier -> ()
    | Pthread -> dump_pthread_barrier_vars ()
    | User|User2|UserFence|UserFence2 -> dump_user_barrier_vars ()
    | TimeBase -> dump_tb_barrier_vars ()
    end ;
    if do_verbose_barrier then begin
      if do_affinity then O.ox indent "int ecpu[N];" ;
      if have_timebase then begin
        if do_timebase then
          O.ox indent "int *tb_delta[N],*tb_count[N];"
        else
          O.ox indent "tb_t *tb_start[N];\n"
      end
    end

  let barrier_def () =
    begin match barrier with
    | NoBarrier -> ()
    | _ -> O.o ""
    end ;
    begin match barrier with
    | User ->
        user_barrier_def No
    | UserFence ->
        user_barrier_def FenceW
    | UserFence2 ->
        user_barrier_def Fence2
    | User2 ->
        user2_barrier_def ()
    | TimeBase ->
        dump_tb_barrier_def ()
    | Pthread ->
        pthread_barrier_def ()
    | NoBarrier -> ()
    end

  let asm s = O.o (sprintf "\"%s\\n\\t\"" s)

  let flush_def ()=
    O.o "inline static void cache_flush(void *p) {" ;
    begin match A.arch with
    | PPCGen|PPC ->
        O.o "asm __volatile__ (" ;
        asm "dcbf 0,%[p]" ;
        O.o ": : [p] \"r\" (p) : \"memory\");"
    | X86 ->
        O.o "asm __volatile__ (" ;
        asm "clflush 0(%[p])" ;
        O.o ": : [p] \"r\" (p) : \"memory\");"
    | ARM -> (* No cache flush for ARM? *)
        ()
    end ;
    O.o "}"

  let touch_def ()=
    O.o "inline static void cache_touch(void *p) {" ;
    O.o "asm __volatile__ (" ;
    begin match A.arch with
    | PPCGen|PPC ->
        asm "dcbt 0,%[p]" ;
        O.o ": : [p] \"r\" (p) : \"memory\");"
    | X86 ->
        asm "prefetcht0 0(%[p])" ;
        O.o ": : [p] \"r\" (p) : \"memory\");"
    | ARM ->
        asm "pld [%[p]]" ;
        O.o ": : [p] \"r\" (p) : \"memory\");"
    end ;
    O.o "}"

  let touch_store_def ()=
    O.o "inline static void cache_touch_store(void *p) {" ;
    begin match A.arch with
    | PPCGen|PPC ->
        O.o "asm __volatile__ (" ;
        asm "dcbtst 0,%[p]" ;
        O.o ": : [p] \"r\" (p) : \"memory\");" ;
    | X86 ->
        O.o "/* Did not find how to announce intention to store for x86 */" ;
        O.o "asm __volatile__ (" ;
        asm "prefetcht0 0(%[p])" ;
        O.o ": : [p] \"r\" (p) : \"memory\");"
    | ARM ->
        O.o "/* to get pldw: -mcpu=cortex-a9 -marm */" ;
        O.o "asm __volatile__ (" ;
        asm (if Cfg.pldw then "pldw [%[p]]" else "pld [%[p]]") ;
        O.o ": : [p] \"r\" (p) : \"memory\");"
    end ;
    O.o "}"

  let get_prefetch_info test =
    try List.assoc "Prefetch" test.T.info
    with Not_found -> ""

  let preload_def = match Cfg.preload with
  | NoPL|RandomPL -> fun _test -> ()
  | CustomPL ->
      fun test ->
        O.o "" ;
        O.o
          (sprintf "static char *global_names[] = {%s};"
             (String.concat ","
                (List.map (sprintf "\"%s\"") (get_global_names test)))) ;
        O.o "" ; flush_def () ;  O.o "" ;
        O.o "" ; touch_def () ;  O.o "" ;
        O.o "" ; touch_store_def () ;  O.o "" ;
        ()
  |StaticPL|Static2PL ->
      fun test ->
        let prf =
          Prefetch.parse (get_prefetch_info test) in
        let is_here t = List.exists  (fun (_,_,i) -> t=i) prf in
        if is_here Prefetch.Flush then begin
          O.o "" ; flush_def () ;  O.o ""
        end ;
        if is_here Prefetch.Touch then begin
          O.o "" ; touch_def () ;  O.o ""
        end ;
        if is_here Prefetch.TouchStore then begin
          O.o "" ; touch_store_def () ;  O.o ""
        end ;
        ()

  let dumb_one_mbar name fence =
    O.f "inline static void %s(void) {"  name ;
    O.fi "asm __volatile__ (\"%s\" ::: \"memory\");" fence ;    
    O.o "}"

  let dump_mbar_def () =    
    O.o "" ;
    O.o "/* Full memory barrier */" ;
    dumb_one_mbar "mbar" 
      (match A.arch with
      | PPCGen|PPC -> "sync"
      | X86 -> "mfence"
      | ARM ->
          begin match Cfg.morearch with
          | MoreArch.ARMv6K -> ""
          | _ -> "dsb"
          end) ;
    if Cfg.cautious then begin
      O.o "" ;
      dumb_one_mbar "mcautious" 
        (match A.arch with
        | PPCGen|PPC -> "sync"
        | X86 -> "mfence"
        | ARM ->
            begin match Cfg.morearch with
            | MoreArch.ARMv6K -> ""
            | _ -> "dmb"
            end)
    end

(* All of them *)

  let dump_threads test =
    (* mbar *)
    dump_mbar_def () ;
    (* Barrier *)
    barrier_def () ;
    (* Preload *)
    preload_def test ;
    ()




(*************)
(* Variables *)
(*************)

  let dump_type t = match t with
  | RunType.Int -> "int"
  | RunType.Pointer -> "int*"

  let dump_global_type t = match memory with
  | Direct -> dump_type t
  | Indirect -> sprintf "%s*" (dump_type t)

  let dump_vars test =
    O.o "/* Shared variables */" ;
    if do_contiguous then O.oi "void *mem;" ;
    List.iter
      (fun (s,t) ->
        O.fi "%s *%s;" (dump_global_type t) s)
      test.T.globals ;
    begin match memory with
    | Direct -> []
    | Indirect ->
        let r =
          List.fold_right
            (fun (a,t) k ->
              O.fi "%s *mem_%s ;" (dump_type t) a ;
              if Cfg.cautious then
                List.fold_right
                  (fun (loc,v) k -> match loc,v with
                  | A.Location_reg(p,r),Symbolic s when s = a ->
                      let cpy = A.Out.addr_cpy_name a p in
                      O.fi "%s* *%s ;" (dump_type t) cpy ;
                      (cpy,a)::k        
                  | _,_ -> k)                    
                  test.T.init k
              else k)
            test.T.globals [] in
        O.oi "int *_idx;" ;
        r
    end

  let is_ptr = function
    | RunType.Pointer -> true
    | RunType.Int -> false

  let ptr_in_outs env test =
    let locs = get_final_locs test in
    List.exists (fun loc -> is_ptr (find_type loc env)) locs
      
  let iter_outs f proc = List.iter (f proc)

  let iter_all_outs f test =
    List.iter
      (fun (proc,(_,outs)) -> iter_outs f proc outs)
      test.T.code

  let dump_out_vars test =
    O.o "/* Final content of observed  registers */" ;
    iter_all_outs
      (fun proc (reg,t) ->
        O.fi "%s *%s;"
          (dump_type t)
          (A.Out.dump_out_reg proc reg))
      test

  let fmt_outcome locs env =
    let pp_loc loc =  match loc with
    | A.Location_reg (proc,reg) ->
        sprintf "%i:%s" proc (A.pp_reg reg)
    | A.Location_global s -> sprintf "%s" s in

    let pp_fmt loc = match find_type loc env with
    | RunType.Int -> if Cfg.hexa then "0x%x" else "%i"
    | RunType.Pointer -> "%s" in

    String.concat " "
      (List.map
         (fun loc -> sprintf "%s=%s;" (pp_loc loc) (pp_fmt loc))
         locs)

  let get_xys =
    let rec do_rec k n = function
      | [] -> k
      | (x,_)::rem ->
          let rec do_rem k n = function
            | [] -> do_rec k n rem
            | (y,_)::rem -> do_rem ((n,(x,y))::k) (n+1) rem in
          do_rem k n rem in
    do_rec [] 0

  let dump_cond_fun env test =
    let cond = test.T.condition in
    let locs = C.locations cond in
    let plocs =
      List.map
        (fun loc ->
          let t = find_type loc env in
          sprintf "%s %s" (dump_type t) (dump_loc_param loc))
        locs in
    let vals = C.location_values cond in
    let pvals =
      List.map (fun loc -> sprintf "void *%s" (dump_val_param loc)) vals in
    O.f "inline static int final_cond(%s) {"
      (String.concat "," (plocs@pvals)) ;
    O.oi "int cond;" ;
    dump_condition dump_loc_param cond ;
    O.oi "return cond;" ;
    O.o "}" ;
    O.o ""

  let dump_cond_fun_call test dump_loc dump_val = 
    let cond = test.T.condition in
    let locs = C.locations cond in
    let plocs = List.map dump_loc locs in
    let vals = C.location_values cond in
    let pvals = List.map dump_val vals in
    sprintf "final_cond(%s)" (String.concat "," (plocs@pvals))

  let dump_defs_outs doc env test =
    (* If some of the output registers is of pointer type,
       we need a special function to print addresses *)
    if ptr_in_outs env test then begin
(*  Translation to indices *)
      let dump_test k = match memory with
      | Direct ->
          fun s ->
            O.fi "else if (v_addr == (void *)&(_a->%s[_i])) return %i;"
              s k
      | Indirect ->
          fun s ->
            O.fi "else if (v_addr == (void *)_a->%s[_i]) return %i;"
              s k in
      O.o "static int idx_addr(ctx_t *_a,int _i,void *v_addr) {" ;
      O.oi "if (v_addr == NULL) { err(2,\"NULL\"); return -1;}" ;
      Misc.iteri (fun k (s,_) -> dump_test k s) test.T.globals ;
      O.oi "else { err(2,\"???\"); return -1;}" ;
      O.o "}" ;
      O.o "" ;
(* Pretty-print indices *)
      let naddrs = List.length test.T.globals in
      O.f "static char *pretty_addr[%i] = {%s};"
        naddrs
        (String.concat ""
           (List.map (fun (s,_) -> sprintf "\"%s\"," s) test.T.globals)) ;
      O.o "" ;
    end ;
(* Outcome collection *)
    O.o "/**********************/" ;
    O.o "/* Outcome collection */" ;
    O.o "/**********************/" ;
(* Outcome type definition *)
    let outs = get_final_locs test in
    let nouts = List.length outs in
    O.f "#define NOUTS %i" nouts ;
    O.o "typedef int outcome_t[NOUTS];" ;
    O.o "" ;
    Misc.iteri
      (fun i loc ->
        O.f "static const int %s_f = %i ;" (dump_loc_name loc) i)
      outs ;
    O.o "" ;
(* Constant wrappers *)    
    O.o DefString.hist_defs ;
(* Checking *)
    if do_collect_local && do_safer then begin
      O.o "static int same_hist(hist_t *h0,hist_t*h1) {" ;
      O.oi "return" ;
      O.oii "h0->n_pos == h1->n_pos && h0->n_neg == h1->n_neg &&" ;
      O.oii "same_outs(h0->outcomes,h1->outcomes);" ;
      O.o "}" ;
      O.o "" ;
    end ;
    if do_verbose_barrier then begin
      O.o "" ;
      O.o "static void pp_tb_log(pm_t *_m,ctx_t *p,int _i,int cond) {";
      O.oi "param_t *_b = p->_p;" ;
      O.oi "if (_b->verbose_barrier <= 1 && !cond) return ;";
(* Show address diffs *)
      let do_diff = Cfg.xy in
      if do_diff then begin
        let xys = get_xys test.T.globals in
        List.iter
          (fun (d,(x,y)) ->
            (match memory with
            | Indirect ->
                O.fi "int d_%02i = abs(((void *)p->%s[_i]-(void *)p->%s[_i]));"
            | Direct ->
                O.fi "int d_%02i = abs(((void *)&p->%s[_i]-(void *)&p->%s[_i]));")
              d x y)
          xys
      end ;
(* Show timebase delays *)
      if not do_timebase && have_timebase then
        O.oi "tb_t _start0 = p->tb_start[0][_i];" ;
      O.oi "pm_lock(_m);" ;
      O.oi "fprintf(stderr,\"%04i:\",_i);" ;
      O.oi "putc(cond ? '*' : ' ',stderr) ;" ;
      if do_diff then begin
        let xys = get_xys test.T.globals in
        List.iter
          (fun (d,(x,y)) ->
            let fmt = sprintf "%s,%s=%%i " x y in
            O.fi "fprintf(stderr,\"%s\",d_%02i);" fmt d)
          xys
      end ;
      if have_timebase then begin
        O.oi "int some = 0;" ;
        O.fi  "for (int _p = %s ; _p < N ; _p++) {"
          (if do_timebase then "0" else "1") ;
        O.oii "if (some) putc(' ',stderr); else some = 1;" ;
        if do_timebase then begin
          let fmt = "%5i[%i]" in
          O.fii "fprintf(stderr,\"%s\",p->tb_delta[_p][_i],p->tb_count[_p][_i]);" fmt
        end else begin
          O.oii "fprintf(stderr,\"%6\"PRIi64,p->tb_start[_p][_i]-_start0);"
        end ;
        O.oi "}"
      end ;
      if do_affinity then begin
        O.oi "int *cpu = p->ecpu;" ;
        O.oi "if (cpu[0] >= 0) {" ;
        O.oii "putc(' ',stderr); putc('[',stderr);" ;
        O.oii "pp_ints(stderr,cpu,N);" ;
        O.oii "putc(']',stderr);" ;
        if do_cores then begin
          O.oii "int t[N];" ;
          O.oii
            "for (int _k = 0 ; _k < N ; _k++) t[_k] = _b->cm->cpu[cpu[_k]];" ;
          O.oii "putc(' ',stderr); putc('{',stderr);" ;
          O.oii "pp_ints(stderr,t,N);" ;
          O.oii "putc('}',stderr);" ;
        end ;
        O.oi "}"
      end ;
      O.oi "putc('\\n',stderr);" ;
      O.oi "pm_unlock(_m);" ;
      O.o "}" ;
      O.o "" 
    end ;
(* Dumping *)
    O.o
      "static void do_dump_outcome(FILE *fhist, int *o, count_t c, int show) {" ;
    let fmt_var = fmt_outcome outs env in
    let fmt ="\"%-6\"PCTR\"%c>" ^ fmt_var ^ "\\n\"" in
    let args =
      String.concat ","
        ("c"::"show ? '*' : ':'"::
         List.map
           (fun loc ->
             let sloc = dump_loc_name loc in
             match find_type loc env with
             | RunType.Int -> sprintf "o[%s_f]" sloc
             | RunType.Pointer -> sprintf "pretty_addr[o[%s_f]]" sloc)
           outs) in
    O.fi "fprintf(fhist,%s,%s);" fmt args ;
    O.o "}" ;
    O.o "" ;
    O.o "static void just_dump_outcomes(FILE *fhist, hist_t *h) {" ;
    O.oi "outcome_t buff ;" ;
    O.oi "dump_outs(fhist,do_dump_outcome,h->outcomes,buff,NOUTS) ;" ;
    O.o "}" ;
    O.o "" ;
    O.o "static void dump_hist(FILE *fhist,hist_t *h) {" ;
    let c = test.T.condition in
    if do_kind then
      O.fi "fprintf(fhist,\"Test %s %s\\n\") ;"
        doc.Name.name (pp_kind (kind_of c))
    else
      O.fi "fprintf(fhist,\"Test %s\\n\") ;"
        doc.Name.name ;
    O.fi "fprintf(fhist,\"%s\",finals_outs(h->outcomes)) ;"
      "Histogram (%\"PCTR\" states)\\n" ;
    O.oi "just_dump_outcomes(fhist,h) ;" ;

    let pp_atom a =
      let open ConstrGen in
      match a with
      | LV (loc,v) ->
          sprintf "%s=%s" (A.pp_location loc) (A.V.pp_v v)
      | LL (loc1,loc2) ->
          sprintf "%s=%s" (A.pp_location loc1) (A.pp_rval loc2) in
    let pp_cond =
      String.escaped
        (ConstrGen.constraints_to_string pp_atom c) in
    if do_kind then begin
      let to_check =
        if ConstrGen.is_existential c then "h->n_pos > 0"
        else "h->n_neg == 0" in
      O.fi "int cond = %s;" to_check ;
      O.fi "fprintf(fhist,\"%%s\\n\",%s);" "cond?\"Ok\":\"No\"" ;
      O.oi "fprintf(fhist,\"\\nWitnesses\\n\");" ;
      O.oi "fprintf(fhist,\"Positive: %\"PCTR\", Negative: %\"PCTR\"\\n\",h->n_pos,h->n_neg);" ;    
      O.fi "fprintf(fhist,\"Condition %s is %%svalidated\\n\",%s);"
        pp_cond
        (sprintf "%s ? \"\" : \"NOT \"" to_check)
    end else begin
      O.fi "fprintf(fhist,\"\\nCondition %s\\n\");"
        pp_cond
    end ;
    O.o "}" ;
    O.o "" ;
    ()

(* Loops *)
  let loop_test_prelude indent ctx =
    O.fx indent "for (int _i = %ssize_of_test-1 ; _i >= 0 ; _i--) {" ctx
      
  and loop_test_postlude indent = O.ox indent "}"

  let loop_proc_prelude indent =
    O.ox indent "for (int _p = N-1 ; _p >= 0 ; _p--) {"
      
  and loop_proc_postlude indent = O.ox indent "}"

  and choose_proc_prelude indent =
    O.ox indent "for (int _p = NT-1 ; _p >= 0 ; _p--) {"
      
(* Safer more, checking globals *)
  let dump_check_vars env test =
    if do_check_globals then begin
      O.o "/* Check data */" ;
      O.oi "pb_t *fst_barrier;" ;
      begin match get_finals_globals test with
      | [] -> ()
      | locs ->
          if do_safer_write then begin
            O.oi "po_t *s_or;" ;
            List.iter
              (fun loc ->
                O.fi "%s* cpy_%s[N] ;"
                  (dump_type (find_type loc env)) (dump_loc_name loc))
              locs
          end                                   
      end  
    end

  let dump_check_globals env test =
    if do_check_globals then begin
(* CHECKGLOBALS *)
      O.f "/**************************************/" ;
      O.f "/* Prefetch (and check) global values */" ;
      O.f "/**************************************/" ;
      O.f "" ;
      O.f "static void check_globals(ctx_t *_a) {" ;
(* LOCALS *)
      List.iter
        (fun (a,t) -> match memory with
        | Indirect -> 
            if t=RunType.Int then O.fi "int *mem_%s = _a->mem_%s;" a a
        | Direct ->
            O.fi "%s *%s = _a->%s;" (dump_global_type t) a a)
        test.T.globals ;
(* LOOPS *)
      loop_test_prelude indent "_a->_p->" ;

      let dump_test (a,t) =
        let v = find_global_init a test in
        match memory,t with
        | Indirect,RunType.Int->
            sprintf "mem_%s[_i] != %s" a  (A.Out.dump_v v)
        | (Indirect,RunType.Pointer) ->
            sprintf "%s != %s"
              (dump_a_leftval a) (dump_a_v_casted v)
        | (Direct,_) ->
            sprintf "%s != %s"
              (dump_leftval a) (dump_a_v_casted v) in
      List.iter
        (fun x ->
          O.fii "if (%s%s) err(2,\"check_globals failed\");"
            (if do_randompl then "rand_bit(&(_a->seed)) && " else "")
            (dump_test x))
        test.T.globals ;
      loop_test_postlude indent ;
(*END LOOP*)
      O.fi "pb_wait(_a->fst_barrier);" ;
      O.f "}\n" ;

(* STABILIZE *)
      let locs = get_finals_globals test in
      begin match locs with
      | _::_ when do_safer_write ->
          O.f "" ;
          O.f "static void stabilize_globals(int _id, ctx_t *_a) {" ;
          O.fi "int size_of_test = _a->_p->size_of_test;" ;
          O.f "" ;
          List.iter
            (fun loc ->
              let a = dump_loc_name loc
              and t = find_type loc env in
              O.fi "%s *%s = _a->%s;" (dump_global_type t) a a ;
              O.fi "%s **cpy_%s = _a->cpy_%s;" (dump_type t) a a)
            locs ;
          O.f "" ;
          O.fi "pb_wait(_a->fst_barrier); " ;
          O.fi "for ( ; ; ) {" ;
          loop_test_prelude indent2 "" ;      
          List.iter
            (fun loc ->
              O.fiii "cpy_%s[_id][_i] = %s;"
                (dump_loc_name loc) (dump_loc loc))
            locs ;   
          loop_test_postlude indent2 ;
          O.fii "po_reinit(_a->s_or);" ;
          O.fii "int _found;" ;
          O.fii "%s" "int _nxt_id = (_id+1) % N;" ;
          O.fii "_found = 0;" ;
          O.fii "for (int _i = size_of_test-1 ; _i >= 0 && !_found ; _i--) {" ;
          List.iter
            (fun loc ->
              let a = dump_loc_name loc in
              O.fiii "if (cpy_%s[_id][_i] != cpy_%s[_nxt_id][_i]) { _found = 1; break; }" a a)
            locs ;
          O.oii "}" ;
          let fmt = "%i: Stabilizing final state!\\n" in
          O.fii "if (_found) { fprintf(stderr,\"%s\",_id); }" fmt ;
          O.oii "if (!po_wait(_a->s_or,_found)) return ;" ;
          O.oi "}" ;
          O.o "}" ;
          O.o "" ;                                                                      
          ()
      | _ -> ()
      end
    end
        
        
  let dump_reinit test cpys =
    O.o "/*******************************************************/" ;
    O.o "/* Context allocation, freeing and reinitialization    */" ;
    O.o "/*******************************************************/" ;
    O.o "" ;
    begin match memory with
    | Indirect ->
        O.o "static void shuffle(ctx_t *_a) {" ;
        begin match test.T.globals with
        | [] -> ()
        | globs ->
            O.oi "int size_of_test = _a->_p->size_of_test;" ;
            O.o "" ;
            List.iter
              (fun (a,_) ->
                O.oi "perm_ints(&(_a->seed),_a->_idx,size_of_test);" ;
                loop_test_prelude indent "" ;
                if Cfg.cautious then O.oii "mcautious();" ;
                O.fii "_a->%s[_i] = &(_a->mem_%s[_a->_idx[_i]]);" a a ;
                loop_test_postlude indent)
              globs
        end ;
        if Cfg.cautious then O.oi "mcautious();" ;
        O.o "}" ;
        O.o ""
    | Direct -> ()
    end ;
(* Initialization, called once *)
    let malloc_gen sz indent name =
      O.fx  indent "_a->%s = malloc_check(%s*sizeof(*(_a->%s)));"
        name sz name in
    let malloc  = malloc_gen "size_of_test"
    and malloc2 = malloc_gen "N" in

    O.o "static void init(ctx_t *_a) {" ;
    O.oi "int size_of_test = _a->_p->size_of_test;" ;
    O.o "" ;
    O.oi "_a->seed = rand();" ;
    iter_all_outs
      (fun proc (reg,_) -> malloc indent (A.Out.dump_out_reg proc reg))
      test ;
(* Shared locations *)
    if do_contiguous then begin
      let sz =
        String.concat "+"
          (List.map
             (fun (a,_) -> sprintf "sizeof(_a->%s[0])" (dump_ctx_tag a))
             test.T.globals) in
      (* +1 for allignement *)            
      let sz = sprintf "(size_of_test+1) * (%s)" sz in
      O.fx indent "_a->mem = malloc_check(%s);" sz ;
      O.ox indent "void * _am = _a->mem;" ;
      List.iter
        (fun (a,t) ->
          O.fx indent "_a->%s = do_align(_am,sizeof(%s));"
            (dump_ctx_tag a) (dump_type t) ;
          O.fx indent "_a->%s = (%s *)_am;" (dump_ctx_tag a) (dump_type t) ;
          O.fx indent "_am += sizeof(%s)*size_of_test;"  (dump_type t))
        test.T.globals ;
      begin match memory with
      | Direct -> ()
      | Indirect ->
          List.iter (fun (a,_) -> malloc indent a) test.T.globals ;
          ()
      end
    end else begin
      List.iter (fun (a,_) -> malloc indent a) test.T.globals ;
      begin match memory with
      | Direct -> ()
      | Indirect ->
          List.iter
            (fun (a,_) -> malloc indent (sprintf "mem_%s" a))
            test.T.globals ;
          ()
      end
    end ;
    begin match memory with
    | Direct -> ()
    | Indirect ->
        O.oi "if (_a->_p->do_shuffle) {" ;    
        malloc indent2 "_idx" ;
        loop_test_prelude indent2 "" ;
        O.oiii "_a->_idx[_i] = _i;" ;
        loop_test_postlude indent2 ;
        O.oi "}" ;
        loop_test_prelude indent "" ;
        List.iter
          (fun (a,_) -> O.fii "_a->%s[_i] = &(_a->mem_%s[_i]);" a a)
          test.T.globals ;
        loop_test_postlude indent ;
        ()
    end ;
    List.iter (fun (cpy,_) -> malloc indent cpy) cpys ;
    if do_check_globals then  begin
      O.oi "_a->fst_barrier = pb_create(N);" ;
    end ;
    if do_safer && do_collect_after then begin
      match get_finals_globals test with
      | _::_ as locs ->
          O.oi "_a->s_or = po_create(N);" ;
          loop_proc_prelude indent ;
          List.iter
            (fun loc ->
              malloc indent2 (sprintf "cpy_%s[_p]" (dump_loc_name loc)))
            locs ;
          loop_proc_postlude indent
      | _ -> ()
    end ;
    begin match barrier with
    | NoBarrier -> ()
    | Pthread -> O.oi "_a->barrier = barrier_create();"
    | User|UserFence|UserFence2 -> malloc indent "barrier"
    | TimeBase -> ()
    | User2 -> malloc2 indent "barrier"
    end ;
    if do_verbose_barrier && have_timebase then begin
      loop_proc_prelude indent ;      
      if do_timebase then begin
        malloc indent2 "tb_delta[_p]" ;
        malloc indent2 "tb_count[_p]"
      end else begin
        malloc indent2 "tb_start[_p]"
      end ;
      loop_proc_postlude indent
    end ;
    if do_sync_macro then begin
      O.oi "_a->_scratch = malloc_check(_a->_p->max_idx*sizeof(*(_a->_scratch)));"
    end ;
    O.o "}" ;
    O.o "" ;

(* Finalization, called once *)
    let free indent name = O.fx indent "free((void *)_a->%s);" name
    and pb_free name = O.fi "pb_free(_a->%s);" name
    and po_free name = O.fi "po_free(_a->%s);" name in

    O.o "static void finalize(ctx_t *_a) {" ;
    if do_contiguous then
      free indent "mem"
    else
      List.iter (fun (a,_) -> free indent (dump_ctx_tag a)) test.T.globals ;
    begin match memory with
    | Direct -> ()
    | Indirect ->
        List.iter (fun (a,_) -> free indent a) test.T.globals ;
        O.oi "if (_a->_p->do_shuffle) {" ;
        free indent2 "_idx" ;
        O.oi "}"
    end ;
    iter_all_outs
      (fun proc (reg,_) -> free indent (A.Out.dump_out_reg proc reg))
      test ;
    if do_safer && do_collect_after then  begin
      pb_free "fst_barrier" ;
      match get_finals_globals test with
      | [] -> ()
      | locs -> 
          po_free "s_or" ;
          loop_proc_prelude indent ;
          List.iter
            (fun loc ->
              free indent2 (sprintf "cpy_%s[_p]" (dump_loc_name loc)))
            locs ;
          loop_proc_postlude indent
    end ;
    begin match barrier with
    | NoBarrier -> ()
    | Pthread -> O.oi "barrier_free(_a->barrier);"
    | User|User2|UserFence|UserFence2 -> free indent "barrier"
    | TimeBase -> ()
    end ;
    if do_verbose_barrier && have_timebase then begin
      loop_proc_prelude indent ;
      if do_timebase then begin
        free indent2 "tb_delta[_p]" ;
        free indent2 "tb_count[_p]"
      end else begin
        free indent2 "tb_start[_p]"
      end ;
      loop_proc_postlude indent
    end ;
    if do_sync_macro then free indent "_scratch" ;
    O.o "}" ;
    O.o "" ;

(* Re-initialization, called once per run *)
    O.o "static void reinit(ctx_t *_a) {" ;
    begin match memory with
    | Indirect ->
        O.oi "if (_a->_p->do_shuffle) shuffle(_a);"
    | Direct -> ()
    end ;
    loop_test_prelude indent "_a->_p->" ;
    List.iter
      (fun (a,t) ->
        let v = A.find_in_state (A.Location_global a) test.T.init in
        if Cfg.cautious then O.oii "mcautious();" ;
        match t,memory with
        | RunType.Int,Indirect ->
            O.fii "_a->mem_%s[_i] = %s;" a (dump_a_v v)
        | _,_ ->
            O.fii "%s = %s;"
              (dump_a_leftval a) (dump_a_v_casted v))
      test.T.globals ;
    begin if do_safer && do_collect_after then
      List.iter
        (fun (proc,(_,outs)) ->
          List.iter
            (fun (reg,t) ->
              if Cfg.cautious then O.oii "mcautious();" ;
              O.fii "_a->%s[_i] = %s;"
                (A.Out.dump_out_reg proc reg)
                (match t with
                | RunType.Int -> "-239487" (* Susmit's sentinel *)
                | RunType.Pointer -> "NULL"))
            outs)
        test.T.code ;
    end ;
    begin match barrier with
    | User|UserFence|UserFence2 ->
        O.oii "_a->barrier[_i] = 0;"
    | Pthread|NoBarrier|User2|TimeBase -> ()
    end ;
    O.oi "}" ;
    if Cfg.cautious then O.oi "mcautious();" ;
    begin match barrier with
    | User2 ->
        O.o "/* Initialisation is a bit complex, due to decreasing index in litmus loop */" ;
        O.oi "for (int _i = 0 ; _i < N ; _i++) {" ;
        O.oii "_a->barrier[_i] = !(((size_of_test -_i -1) %% (2*N)) < N);" ;
        O.oi "}"
    | TimeBase ->
        O.oi "barrier_init(&_a->barrier);"
    | NoBarrier|Pthread|User|UserFence|UserFence2 -> ()
    end ;
    O.o "}" ;
    O.o ""      


  let dump_templates env tname test =
    O.f "/***************/" ;
    O.f "/* Litmus code */" ;
    O.f "/***************/" ;
    O.f "" ;
    O.f "typedef struct {" ;
    O.fi "int th_id; /* I am running on this thread */" ;
    if do_affinity then O.fi "int *cpu; /* On this cpu */" ;
    if do_timebase && have_timebase then
      O.fi "int delay; /* time base delay */" ;
    if do_custom then O.fi "prfproc_t *prefetch;" ;
    if do_verbose_barrier_local then O.fi "pm_t *p_mutex;" ;
    O.fi "ctx_t *_a;   /* In this context */" ;
    O.f "} parg_t;" ;
    O.f "" ;
    List.iter
      (fun (proc,(out,outregs)) ->
        let  do_collect =  do_collect_local && (do_safer || proc=0) in
        O.f "static void *P%i(void *_vb) {" proc ;
        O.fi "mbar();" ;
        if do_collect then O.fi "hist_t *hist = alloc_hist();" ;
        O.fi "parg_t *_b = (parg_t *)_vb;" ;
        O.fi "ctx_t *_a = _b->_a;" ;

        if do_affinity then begin
          O.fi "int _ecpu = _b->cpu[_b->th_id];" ;
          if do_verbose_barrier then O.fi "_a->ecpu[%i] = _ecpu;" proc ;
          let fun_name,arg =
            if do_force_affinity then
              "force_one_affinity",sprintf ",AVAIL,_a->_p->verbose,\"%s\"" tname
            else 
              "write_one_affinity","" in
          O.fi "%s(_ecpu%s);" fun_name arg
        end ;
        if do_check_globals then begin
          O.fi "check_globals(_a);"
        end ;
        begin match barrier with
        | User|User2|UserFence|UserFence2 ->
            O.fi "int _th_id = _b->th_id;" ;
            O.fi "int volatile *barrier = _a->barrier;"
        | TimeBase ->
            O.fi "int mySense = 0;" ;
            O.fi "sense_t *barrier = &_a->barrier;"
        | Pthread ->
            O.fi "barrier_t *barrier = _a->barrier;"
        | NoBarrier -> ()
        end ;
        O.fi "int _size_of_test = _a->_p->size_of_test;" ;
        if do_staticpl then
          O.oi "unsigned int _static_prefetch = _a->_p->static_prefetch;" ;
        begin match stride with
        | None -> ()
        | Some _ -> O.fi "int _stride = _a->_p->stride;"
        end ;
        let addrs = A.Out.get_addrs out in
        List.iter
          (fun a ->
            let t = find_global_type a env in
            O.fi "%s *%s = _a->%s;" (dump_global_type t) a a)
          addrs ;
        List.iter
          (fun (r,t) ->
            let name = A.Out.dump_out_reg  proc r in
            O.fi "%s *%s = _a->%s;" (dump_type t) name name)
          outregs ;

        let iloop =
          match stride with
          | Some _ ->
              O.fi "for (int _j = _stride ; _j > 0 ; _j--) {" ;
              O.fii "for (int _i = _size_of_test-_j ; _i >= 0 ; _i -= _stride) {" ;
              indent3
          | None ->
              loop_test_prelude indent "_" ;
              indent2 in
        if do_custom then begin
          let i = iloop in
          begin match addrs with
          | [] -> ()
          | _::_ ->
              O.fx i "prfone_t *_prft = _b->prefetch->t;" ;
              O.fx i "prfdir_t _dir;" ;
              Misc.iteri
                (fun k loc ->
                  let addr = dump_a_addr loc in
                  O.fx i "_dir = _prft[%i].dir;" k ;
                  O.fx i "if (_dir == flush) cache_flush(%s);" addr ;
                  O.fx i "else if (_dir == touch) cache_touch(%s);" addr ;
                  O.fx i "else if (_dir == touch_store) cache_touch_store(%s);" addr ;
                  ())
                addrs
          end
        end else begin
          let vars = get_global_names test in
          let prf = Prefetch.parse (get_prefetch_info test) in
          let iter pp =
            List.iter
              (fun (xproc,loc,t) ->
                if xproc = proc then begin
                  if List.mem loc vars then begin
                    try
                      let f = match t with
                      | Prefetch.Ignore -> raise Exit
                      | Prefetch.Flush -> "cache_flush"
                      | Prefetch.Touch -> "cache_touch"
                      | Prefetch.TouchStore -> "cache_touch_store" in
                      pp f (dump_a_addr loc)
                    with Exit -> ()
                  end else
                    Warn.warn_always
                      "Variable %s from prefetch is absent in test" loc
                end) prf in
          if do_static2pl then begin
            iter
              (fun f loc -> O.fx iloop "if (rand_bit(&(_a->seed))) %s(%s);" f loc)
          end else if do_staticpl then begin
            O.fx iloop "switch (_static_prefetch) {" ;
            let i = iloop in
            let j = Indent.tab i in
            O.fx i "case 0:" ;
            O.fx j "break;" ;
            O.o "" ;
            O.fx i "case 1:" ;
            iter
              (fun f loc -> O.fx j "%s(%s);" f loc) ;
            O.fx j "break;" ;
            O.o "" ;
            O.fx i "case 2:" ;
            iter
              (fun f loc -> O.fx j "if (rand_bit(&(_a->seed))) %s(%s);" f loc) ;
            O.fx j "break;" ;
            O.o "" ;
            O.fx i "default:" ;
            iter
              (fun f loc ->
                O.fx j "if (rand_k(&(_a->seed),_static_prefetch) == 0) %s(%s);"
                  f loc) ;
            O.fx j "break;" ;
            O.fx iloop "}" ;          
          end
        end ;
        begin match barrier with
        | User|UserFence|UserFence2 ->
            O.fx iloop "barrier_wait(_th_id,_i,&barrier[_i]);" ;
            ()
        | User2 ->
            O.fx iloop "barrier_wait(_th_id,_i,barrier);"  ;
            ()
        | TimeBase ->
            if have_timebase then begin
              O.fx iloop
                "if (_i %% N == %i) _a->next_tb = read_timebase();"
                proc ;
              O.fx iloop "barrier_wait(barrier,&mySense);" ;
              O.fx iloop "tb_t _tb0 = _a->next_tb;"
            end else begin
              O.fx iloop "barrier_wait(barrier,&mySense);" ;
            end             
        | Pthread ->
            O.fx iloop "barrier_wait(%i,barrier);" proc ;
            if Cfg.cautious then O.fx iloop "mcautious();"
        | NoBarrier ->
            if Cfg.cautious then O.fx iloop "mcautious();"
        end ;

        begin match barrier with
        | TimeBase ->
            if have_timebase then begin
              if do_verbose_barrier then begin
                O.fx iloop "int _delta, _count=0;" ;
                O.fx iloop "do { _delta =  read_timebase() - _tb0; _count++; } while (_delta < _b->delay);"
              end else begin
                O.fx iloop "int _delta;" ;
                O.fx iloop "do { _delta =  read_timebase() - _tb0; } while (_delta < _b->delay);"
              end
            end ;
            if Cfg.cautious then O.fx iloop "mcautious();"
        | _ ->
            if do_verbose_barrier && have_timebase then begin
              O.fx iloop "tb_t _start = read_timebase();"
            end
        end ;
        if do_isync then begin match barrier with
        | User | User2 | UserFence | UserFence2 | TimeBase ->
            begin match A.arch with
            | PPCGen
            | PPC ->
                O.fx iloop "asm __volatile__ (\"isync\" : : : \"memory\");"
            | ARM -> 
                O.fx iloop "asm __volatile__ (\"isb\" : : : \"memory\");"
            | X86 -> ()
            end
        | Pthread|NoBarrier -> ()
        end ;
        let myenv =
          List.fold_right
            (fun (loc,t) k -> match loc with
            | A.Location_reg (p,reg) -> if p = proc then (reg,t)::k else k
            | A.Location_global _ -> k) env [] in     
(* Dump real code now *)
        A.Out.dump O.out (Indent.as_string iloop) myenv proc out ;
        if do_verbose_barrier && have_timebase  then begin
          if do_timebase then begin
            O.fx iloop "_a->tb_delta[%i][_i] = _delta;" proc ; 
            O.fx iloop "_a->tb_count[%i][_i] = _count;" proc
          end else begin
            O.fx iloop "_a->tb_start[%i][_i] = _start;" proc
          end
        end ;

        if do_collect then begin          
          let locs = get_final_locs test in 
          O.fx iloop "barrier_wait(barrier,&mySense);" ;
          O.fx iloop "int cond = %s;"
            (dump_cond_fun_call test
               (dump_ctx_loc "_a->") dump_a_addr) ;
(*          dump_condition chan iloop (dump_ctx_loc "_a->") test.T.condition ; *)
          O.fx iloop "if (%s) { hist->n_pos++; } else { hist->n_neg++; }"
            (test_witness test "cond") ;

(* My own private outcome collection *)
          O.fx iloop "outcome_t _o;" ;
          List.iter
            (fun loc ->
              O.fx iloop "_o[%s_f] = %s;"
                (dump_loc_name loc)
                (let sloc =  dump_ctx_loc "_a->" loc in
                match find_type loc env with
                | RunType.Int -> sloc
                | RunType.Pointer -> sprintf "idx_addr(_a,_i,%s)" sloc))
            locs ;
          O.fx iloop "add_outcome(hist,1,_o,%scond);"
            (if remark_pos test then "" else "!") ;
          if do_verbose_barrier_local && proc = 0 then begin
            O.fx iloop "if (_a->_p->verbose_barrier) {" ;
            O.fx (Indent.tab iloop) "pp_tb_log(_b->p_mutex,_a,_i,%scond);"
              (if remark_pos test then "" else "!") ;
            O.fx iloop "}"
          end
        end else if do_collect_local then begin
          O.fx iloop "barrier_wait(barrier,&mySense);" 
        end else if do_timebase && have_timebase then begin
(*          O.fx iloop "barrier_wait(barrier,&mySense);"
            Problematic 4.2W on squale *)
        end  ;
        begin match stride with
        | Some _ ->
            loop_test_postlude indent2 ;
            loop_test_postlude indent
        | None ->
            loop_test_postlude indent
        end ;

        if do_safer && do_collect_after && have_finals_globals test then begin
          O.fi "stabilize_globals(%i,_a);" proc ;
        end ;
        O.oi "mbar();" ;
        begin if do_collect then
          O.fi "return hist;"
        else
          O.fi "return NULL;"
        end ;
        O.o "}" ;
        O.o "")
      test.T.code
      

  let dump_zyva doc cpys env test =
    let procs = List.map (fun (proc,_) -> proc) test.T.code in    

    O.o "typedef struct {" ;
    O.oi "pm_t *p_mutex;" ;
    O.oi "pb_t *p_barrier;" ;
    O.oi "param_t *_p;" ;
    if do_prealloc then O.oi "ctx_t ctx;" ;
    if do_affinity then begin
      O.oi "int z_id;" ;
      O.oi "int *cpus;" ;
      ()
    end ;
    O.o "} zyva_t;" ;
    O.o "" ;
    O.f "#define NT %s" "N" ;
    O.o "" ;    
    O.o "static void *zyva(void *_va) {" ;
(* Define local vars *)
    O.oi "zyva_t *_a = (zyva_t *) _va;" ;
    O.oi "param_t *_b = _a->_p;" ;
    O.oi "pb_wait(_a->p_barrier);" ;
    O.oi "pthread_t thread[NT];" ;
    O.oi "parg_t parg[N];" ;
    O.fi "f_t *fun[] = {%s};"
      (String.concat ","
         (List.map (sprintf "&P%i") procs)) ;
    if do_collect_after then O.oi "hist_t *hist = alloc_hist();" ;
    if do_collect_local then begin
      if do_safer then
        O.oi "hist_t *hist0=alloc_hist(), *phist[N];"
      else
        O.oi "hist_t *hist0=alloc_hist(), *phist = NULL, *_tmp;" ;      
    end ;
    begin if do_prealloc then
      O.oi "ctx_t ctx = _a->ctx;"
    else
      O.oi "ctx_t ctx;"
    end ;
    O.oi "ctx._p = _b;" ;
    O.o "" ;

(* Initialize *)
    if not do_prealloc then O.oi "init(&ctx);" ;
(* Build T preads arguments (which are constant) *)
    loop_proc_prelude indent ;
    O.oii "parg[_p].th_id = _p; parg[_p]._a = &ctx;" ;
    if do_affinity then O.oii "parg[_p].cpu = &(_a->cpus[0]);" ;
    if do_timebase && have_timebase then O.oii "parg[_p].delay = _b->delays[_p];" ;
    if do_custom then
      O.oii "parg[_p].prefetch = &_b->prefetch->t[_p];" ;
    if do_verbose_barrier_local then
      O.oii "parg[_p].p_mutex = _a->p_mutex;" ;
    loop_proc_postlude indent ;
(* Launch cached threads, if needed *)

    O.o "" ;
(* Loop max_run times *)
    O.oi "for (int n_run = 0 ; n_run < _b->max_run ; n_run++) {" ;
    let do_break idt =
      if do_speedcheck then begin
        O.ox idt "if (_b->stop_now) {" ;
        let idt2 = Indent.tab idt in
        O.ox idt2 "break;" ;
        O.ox idt "}"
      end in

    if do_affinity then begin
      O.oii "if (_b->aff_rand) {" ;     
      O.oiii "pb_wait(_a->p_barrier);" ;
      do_break indent3 ;
      O.oiii "if (_a->z_id == 0) perm_prefix_ints(&ctx.seed,_a->cpus,_b->ncpus_used,_b->ncpus);" ;
      O.oiii "pb_wait(_a->p_barrier);" ;
      O.oii "} else {" ;            
      do_break indent3 ;
      O.oii "}" ;            
      ()
    end else begin
      do_break indent2
    end ;
    O.oii
      "if (_b->verbose>1) fprintf(stderr, \"Run %i of %i\\r\", n_run, _b->max_run);" ;
    O.oii "reinit(&ctx);"  ;
(* Start/join threads *)

    begin match launch with
    | Changing -> O.oii "if (_b->do_change) perm_funs(&ctx.seed,fun,N);"
    | Fixed -> ()
    end ;

    choose_proc_prelude indent2 ;
    if Cfg.cautious then O.oiii "mbar();" ;
    O.oiii "launch(&thread[_p],fun[_p],&parg[_p]);" ;
    loop_proc_postlude indent2 ;
    if Cfg.cautious then O.oii "mbar();" ;

    begin match launch with
    | Changing ->
        O.oii "if (_b->do_change) perm_threads(&ctx.seed,thread,NT);"
    | Fixed -> ()
    end ;

    let set_result i _p call =      
      if do_collect_local then
        if do_safer then
          O.fx i "phist[%s] = %s" _p call
        else begin
          O.fx i "_tmp = %s" call ;
          O.fx i "if (_tmp) phist = _tmp;" ;
          ()
        end
      else
        O.fx i "%s" call in
    choose_proc_prelude indent2 ;
    if Cfg.cautious then O.oiii "mbar();" ;
    set_result indent3 "_p" "join(&thread[_p]);" ;
    loop_proc_postlude indent2 ;
    if Cfg.cautious then O.oii "mbar();" ;

(* Check histograms collectect by test threads *)
    if do_collect_local then begin
      if do_safer then begin
        O.oii "/* Check local histograns */" ;
        O.oii "for (int _p = 0 ; _p < N-1 ; _p++) {" ;
        O.oiii "if (!same_hist(phist[_p],phist[_p+1])) err(1,\"check hist\") ;" ;
        O.oii "}" ;
        O.oii "merge_hists(hist0,phist[0]);" ;
        O.oii "for (int _p = 0 ; _p < N-1 ; _p++) {" ;
        O.oiii "free_hist(phist[_p]);" ;
        O.oii "}"
      end else
        O.oii "merge_hists(hist0,phist);"
    end ;
(* Log final states *)
    if do_collect_after then begin
      O.oii "/* Log final states */" ;
      loop_test_prelude indent2 "_b->" ;    
      let locs = get_final_locs test in 

(* Make copies of final locations *)
      if Cfg.cautious then begin match locs with
      | [] -> ()
      | _ ->  O.oiii "mcautious();"
      end ;
      List.iter
        (fun loc ->
          O.fiii "%s %s = %s;"
            (dump_type (find_type loc env))
            (dump_loc_copy loc)
            (dump_ctx_loc "ctx." loc) ;
          if Cfg.cautious then O.oiii "mcautious();")
        locs ;
      O.oiii "outcome_t o;" ;
      O.oiii "int cond;" ;
      O.o "" ;
(* check globals against stabilized value *)
      if do_safer && do_collect_after then begin
        match get_finals_globals test with
        | [] -> ()
        | locs -> begin
            List.iter
              (fun loc ->
                loop_proc_prelude indent3 ;
                O.fiv
                  "if (%s != ctx.cpy_%s[_p][_i]) err(1,\"%s: global %s unstabilized\") ;"
                  (dump_loc_copy loc) (dump_loc_name loc)
                  (doc.Name.name)  (dump_loc_name loc) ;
                loop_proc_postlude indent3)
              locs ;
        end
      end ;
(* Cautious check of indirect mode *)
      List.iter
        (fun (cpy,loc) ->
          O.fiii
            "if (ctx.%s[_i] != ctx.%s[_i]) err(1,\"%s: address copy %s is wrong\") ; "
            cpy loc doc.Name.name cpy)
        cpys ;
(* Compute final condition *)
      O.fiii "cond = %s;"
        (dump_cond_fun_call test dump_loc_copy dump_ctx_addr) ;
(* Save outcome *)
      List.iter
        (fun loc ->
          O.fiii "o[%s_f] = %s;"
            (dump_loc_name loc)
            (match find_type loc env with
            | RunType.Int -> dump_loc_copy loc
            | RunType.Pointer -> sprintf "idx_addr(&ctx,_i,%s)" (dump_loc_copy loc)))
        locs ;
      O.fiii "add_outcome(hist,1,o,%scond);"
        (if remark_pos test then "" else "!") ;
      

(****************)
      
(*      dump_condition chan indent3 dump_loc_copy test.T.condition  ; *)
      O.fiii "if (%s) { hist->n_pos++; } else { hist->n_neg++; }"
        (test_witness test "cond") ; 
      if (do_verbose_barrier) then begin
        O.oiii "if (_b->verbose_barrier) {" ;
        O.fiv "pp_tb_log(_a->p_mutex,&ctx,_i,%scond);"          
          (if remark_pos test then "" else "!") ;
        O.oiii "}"
      end ;
      loop_test_postlude indent2 ;
      ()
    end ;
(* Cautious check of indirect pointers *)
    if Cfg.cautious then begin match Cfg.memory with
    | Indirect ->
        let locs = get_global_names test in
        List.iter
          (fun loc ->
            O.fii
              "if (!check_shuffle(ctx.%s,ctx.%s,_b->size_of_test))"
              loc (dump_ctx_tag loc) ;
            O.fiii
              "err(1,\"%s: check_shuffle for %s\");"
              doc.Name.name loc)

          locs
    | Direct -> ()
    end ;
(* Check histogram against sum of local histogram *)
    if do_collect_local && do_collect_after && do_safer then begin
      O.oii "/* Check histogram against sum of local histogram */" ;
      O.oii "if (!same_hist(hist,hist0)) {" ;
      O.oiii "fprintf(stderr,\"Zyva histogram:\\n\");" ;
      O.oiii "just_dump_outcomes(stderr,hist);" ;
      O.oiii "fprintf(stderr,\"Local histogram:\\n\");" ;
      O.oiii "just_dump_outcomes(stderr,hist0);" ;
      O.oiii "err(1,\"check summed hist\");" ;
      O.oii "}"
    end ;
    if do_speedcheck then begin
      let cond =
        let hist =
          if do_collect_after then "hist" else "hist0" in
        if ConstrGen.is_existential test.T.condition then
          sprintf "%s->n_pos > 0" hist
        else
          sprintf "%s->n_neg > 0" hist in
      O.fii "if (_b->speedcheck && %s) {" cond ;
      O.oiii "_b->stop_now = 1;" ;
      O.oii "}"
    end ;
    O.oi "}" ;

    O.o "" ;
    if do_collect_both then O.oi "free_hist(hist0);" ;
    O.oi "finalize(&ctx);" ;
    if do_collect_after || do_collect_both then
      O.oi "return hist;"
    else
      O.oi "return hist0;" ;
    O.o "}" ;
    O.o "" ;
    ()


  let dump_def_ctx env test =
    O.o "/**********************/" ;
    O.o "/* Context definition */" ;
    O.o "/**********************/" ;
    O.o "" ;
    O.o "typedef struct {" ;
    let cpys = dump_vars test in
    dump_out_vars test ;
    dump_check_vars env test ;
    dump_barrier_vars () ;
    O.oi "st_t seed;" ;
    if do_sync_macro then O.oi "char *_scratch;" ;
    O.o "/* Parameters */" ;
    O.oi "param_t *_p;" ;
    O.o "} ctx_t;" ;
    O.o "" ;
    cpys

  module D = TestDump.Make(T)

  let dump_report doc _env test = 
    O.o "#ifdef ASS" ;
    O.o "static void ass(FILE *out) { }" ;
    O.o "#else" ;
    O.f "#include \"%s\"" (MyName.outname doc.Name.file ".h") ;
    O.o "#endif" ;
    O.o "" ;
    let dstring s = O.fi "fprintf(out,\"%%s\\n\",\"%s\");" (String.escaped s) in
(* Static information *)
    O.o "static void report(FILE *out) {" ;
    let title = sprintf "%% Results for %s %%" doc.Name.file in
    let nice = String.make (String.length title) '%' in
    dstring nice ;
    dstring title ;
    dstring nice ;
    let xs = D.lines doc test.T.src in
    List.iter dstring xs ;
    O.oi "fprintf(out,\"Generated assembler\\n\");" ;
    O.oi "ass(out);" ;
(*
  O.fi "cat_file(\"%s\",\"%s\",out);"
  (MyName.outname doc.Name.file ".t")
  "Generated assembler" ;
 *)
    O.o "}" ;
    O.o "" ;
    ()

  let check_speedcheck i f =
    if do_speedcheck then begin
      O.fx i "%s" "if (!prm.speedcheck) {" ;
      f (Indent.tab i) ;
      O.fx i "}"
    end else f i 

  let dump_run doc _env test = 
(* Custom affinity information *)
    let dca,ca = mk_dca test in
    let affi =        
      if dca then begin
        let cs,ne =
          try Affi.compute (LexAffinity.coms ca)
          with LexAffinity.Error msg -> Warn.user_error "%s" msg in
        Misc.iteri
          (fun k c ->
            O.f "static int color_%i[] = {%s, -1};" k
              (String.concat ", " (List.map (sprintf "%i") c)))
          cs ;
        O.f "static int *color[] = {%s, NULL};"
          (String.concat ", "
             (Misc.mapi
                (fun k _c -> sprintf "color_%i" k)
                cs)) ;
        begin match ne with
        | [] -> O.o "static int diff[] = {-1};"
        | _ ->
            O.f "static int diff[] = {%s, -1};"
              (String.concat ", "
                 (List.map (fun (x,y) -> sprintf "%i, %i" x y) ne))
        end ;
        O.o "" ;
        Some (cs,ne)
      end else
        None in
    O.o "static int run(cmd_t *cmd,cpus_t *def_all_cpus,FILE *out) {" ;
(* Prelude *)
    if do_vp then O.oi "if (cmd->prelude) report(out);" ;
(* Starting time *)
    O.oi "tsc_t start = timeofday();" ;
(* Parameters recorded in param_t structure *)    
    O.oi "param_t prm ;" ;
    O.o "/* Set some parameters */" ;
    O.oi "prm.verbose = cmd->verbose;" ;
    if do_verbose_barrier then
      O.oi "prm.verbose_barrier = cmd->verbose_barrier;" ;
    O.oi "prm.size_of_test = cmd->size_of_test;" ;
    O.oi "prm.max_run = cmd->max_run;" ;
    begin match stride with
    | None -> ()
    | Some _ -> O.oi "prm.stride = cmd->stride;"
    end ;
    if do_speedcheck then
      O.oi "prm.speedcheck = cmd->speedcheck; prm.stop_now = 0;" ;
    begin match memory with
    | Direct -> ()
    | Indirect -> O.oi "prm.do_shuffle = cmd->shuffle;"
    end ;
    begin match launch with
    | Fixed -> ()
    | Changing ->
        if dca then begin
          O.oi "prm.do_change = cmd->aff_mode != aff_custom;"
        end else begin
          O.oi "prm.do_change = 1;"
        end ;
        O.oi "if (cmd->fix) prm.do_change = 0;"
    end ;
    if do_timebase && have_timebase then
      O.oi "prm.delays = cmd->delta_tb->t;" ;
    if do_custom then  O.oi "prm.prefetch = cmd->prefetch;" ;
    if do_staticpl then  O.oi "prm.static_prefetch = (unsigned int)cmd->static_prefetch;" ;
    if do_cores then begin
      O.fi "prm.cm = coremap_%s(def_all_cpus->sz,%i);"
        (Smt.pp smtmode) smt
    end ;
    if Cfg.timeloop > 0 then O.oi "prm.max_loop = cmd->max_loop;" ;
    if do_sync_macro then O.oi "prm.max_idx = SYNC_K * cmd->sync_n;\n" ;
    O.o "/* Computes number of test concurrent instances */" ;
    if do_affinity then begin
      O.oi "int n_avail = cmd->avail > 0 ? cmd->avail : cmd->aff_cpus->sz;";
      let fmt = "Warning: avail=%i, available=%i\\n" in
      O.fi "if (n_avail >  cmd->aff_cpus->sz) fprintf(stderr,\"%s\",n_avail, cmd->aff_cpus->sz);" fmt
    end else begin
      O.oi "int n_avail = cmd->avail;"
    end ;
    O.oi "int n_exe;" ;
    O.oi "if (cmd->n_exe > 0) {" ;
    O.oii "n_exe = cmd->n_exe;" ;
    O.oi "} else {" ;
    O.oii "n_exe = n_avail < N ? 1 : n_avail / N;" ;
    O.oi "}" ;
    if do_affinity then begin
      O.o "/* Set affinity parameters */" ;
      O.oi "cpus_t *all_cpus = cmd->aff_cpus;" ;
      O.oi "int aff_cpus_sz = cmd->aff_mode == aff_random ? max(all_cpus->sz,N*n_exe) : N*n_exe;" ;
      O.oi "int aff_cpus[aff_cpus_sz];" ;
      O.oi "prm.aff_rand = cmd->aff_mode == aff_random;" ;
      O.oi "prm.ncpus = aff_cpus_sz;" ;
      O.oi "prm.ncpus_used = N*n_exe;"
    end ;
    O.o "/* Show parameters to user */" ;
    O.oi "if (prm.verbose) {" ;
    let fmt = doc.Name.name ^ ": n=%i, r=%i, s=%i" in
    O.fii "fprintf(stderr, \"%s\",n_exe,prm.max_run,prm.size_of_test);" fmt ;
    begin match stride with
    | None -> ()
    | Some _ ->
        let fmt = ", st=%i" in
        O.fii "fprintf(stderr,\"%s\",prm.stride);" fmt
    end ;
    begin match memory with
    | Direct -> ()
    | Indirect ->
        let fmt = ", %crm" in
        O.fii "fprintf(stderr,\"%s\",prm.do_shuffle?'+':'-');" fmt
    end ;
    if do_affinity then begin
      O.oii "if (cmd->aff_mode == aff_incr) {" ;
      let fmt = ", i=%i" in
      O.fiii "fprintf(stderr, \"%s\",cmd->aff_incr);" fmt ;
      O.oii "} else if (cmd->aff_mode == aff_random) {" ;
      O.oiii "fprintf(stderr,\", +ra\");" ;
      O.oii "} else if (cmd->aff_mode == aff_custom) {" ;
      O.oiii "fprintf(stderr,\", +ca\");" ;
      O.oii "}" ;
      O.oii "fprintf(stderr,\", p='\");" ;
      O.oii "cpus_dump(stderr,cmd->aff_cpus);" ;
      O.oii "fprintf(stderr,\"'\");"
    end ;
    if do_timebase && have_timebase then begin
      O.oii "fprintf(stderr,\", tb=\");" ;
      O.oii "ints_dump(stderr,cmd->delta_tb);" ;
      O.oii "fprintf(stderr,\"'\");"
    end ;
    if do_custom then begin
      O.oii "fprintf(stderr,\", prf='\");" ;
      O.oii "prefetch_dump(stderr,cmd->prefetch);" ;
      O.oii "fprintf(stderr,\"'\");"
    end ;
    if do_staticpl then begin
      O.oii "fprintf(stderr,\", prs=%i\",cmd->static_prefetch);"
    end ;
    if Cfg.timeloop > 0 then begin
      let fmt = ", l=%i" in
      O.fii "fprintf(stderr,\"%s\",prm.max_loop);" fmt
    end ;
    O.oii "fprintf(stderr,\"\\n\");" ;
    if dca then begin
      O.oii "if (prm.verbose > 1 && prm.cm) {" ;
      O.oiii"fprintf(stderr,\"logical proc -> core: \");" ;
      O.oiii "cpus_dump(stderr,prm.cm);" ;
      O.oiii "fprintf(stderr,\"\\n\");" ;
      O.oii "}"
    end ;
    O.oi "}" ;
    if do_affinity then begin
      O.oi "if (cmd->aff_mode == aff_random) {" ;
      O.oii "for (int k = 0 ; k < aff_cpus_sz ; k++) {" ;
      O.oiii "aff_cpus[k] = all_cpus->cpu[k % all_cpus->sz];" ;        
      O.oii "}" ;
      if dca then begin
        O.oi "} else if (cmd->aff_mode == aff_custom) {" ;
        O.oii "st_t seed = 0;" ;
        O.oii "custom_affinity(&seed,prm.cm,color,diff,all_cpus,n_exe,aff_cpus);" ;
        O.oii "if (prm.verbose) {" ;
        O.oiii"fprintf(stderr,\"thread allocation: \\n\");" ;
        O.oiii "cpus_dump_test(stderr,aff_cpus,aff_cpus_sz,prm.cm,N);" ;
        O.oii "}"
      end ;
      O.oi "}" ;
    end ;

(* Signaling writes *)
    if Cfg.signaling then begin
      O.oi "prm.sig_addr = &prm.sig_cell;" ;
      let fmt = "Signaling to %p\\n" in
      O.fi "fprintf(out,\"%s\",prm.sig_addr); fflush(out);" fmt
    end ;
(*********************)
(* Spawn experiments *)
(*********************)
    O.oi "pthread_t th[n_exe];" ;
    O.oi "zyva_t zarg[n_exe];" ;
    O.oi "pm_t *p_mutex = pm_create();" ;
    O.oi "pb_t *p_barrier = pb_create(n_exe);" ;    
(* Compute affinity settings *)
    if do_affinity then begin
      O.oi "int next_cpu = 0;" ;
      O.oi "int delta = cmd->aff_incr;" ;
      O.oi "if (delta <= 0) {" ;
      O.oii "for (int k=0 ; k < all_cpus->sz ; k++) all_cpus->cpu[k] = -1;" ;
      O.oii "delta = 1;" ;
      O.oi "} else {" ;
      O.oii "delta %= all_cpus->sz;" ;
      O.oi"}" ;
      O.oi "int start_scan=0, max_start=gcd(delta,all_cpus->sz);" ;
      O.oi "int *aff_p = aff_cpus;"
    end ;
(* launching loop *)
    O.oi "for (int k=0 ; k < n_exe ; k++) {" ;
    O.oii "zyva_t *p = &zarg[k];" ;
    O.oii "p->_p = &prm;" ;
    O.oii "p->p_mutex = p_mutex; p->p_barrier = p_barrier; " ;
    if do_prealloc then O.oii "init(&p->ctx);" ;
    if do_affinity then begin
      O.oii "p->z_id = k;" ;      
      O.oii "p->cpus = aff_p;" ;
      O.oii "if (cmd->aff_mode != aff_incr) {" ;
      O.oiii "aff_p += N;" ;
      O.oii "} else {" ;
      O.oiii "for (int i=0 ; i < N ; i++) {" ;
      O.oiv "*aff_p = all_cpus->cpu[next_cpu]; aff_p++;" ;
      O.oiv "next_cpu += delta; next_cpu %= all_cpus->sz;" ;
      O.oiv "if (next_cpu == start_scan) {" ;
      O.ov "start_scan++ ; start_scan %= max_start;" ;
      O.ov  "next_cpu = start_scan;" ;
      O.oiv "}" ;
      O.oiii "}" ;
      O.oii "}"
    end ;
    O.oii "launch(&th[k],zyva,p);" ;
    O.oi "}" ;
(* end of loop *)

(********)
(* Join *)
(********)
    O.o "" ;
    O.oi "count_t n_outs = prm.size_of_test; n_outs *= prm.max_run;" ;
(* join first thread *)
    O.oi "hist_t *hist = (hist_t *)join(&th[0]);" ;
(* join loop *)
    O.oi "for (int k=1 ; k < n_exe ; k++) {" ;
    O.oii "hist_t *hk = (hist_t *)join(&th[k]);" ;
    check_speedcheck indent2
      (fun i ->
        O.ox i
          "if (sum_hist(hk) != n_outs || hk->n_pos + hk->n_neg != n_outs) {" ;
        O.oy i "err(1,\"sum_hist\");" ;
        O.ox i "}") ;
    O.oii "merge_hists(hist,hk);" ;
    O.oii "free_hist(hk);" ;
    O.oi "}" ;
(* end of join loop *)
    if do_affinity then O.oi"cpus_free(all_cpus);" ;
    O.oi "tsc_t total = timeofday() - start;" ;
    O.oi "pm_free(p_mutex);" ;
    O.oi "pb_free(p_barrier);" ;
    O.o "" ;
    O.oi "n_outs *= n_exe ;" ;
    check_speedcheck indent
      (fun i ->
        O.ox i
          "if (sum_hist(hist) != n_outs || hist->n_pos + hist->n_neg != n_outs) {"  ;
        O.oy i "err(1,\"sum_hist\") ;" ;
        O.ox i "}") ;
    O.oi "dump_hist(out,hist);" ;
    begin match test.T.condition with
    | ForallStates _
    | ExistsState _ ->
        O.oi "count_t p_true = hist->n_pos, p_false = hist->n_neg;"
    | NotExistsState _ ->
        O.oi "count_t p_false = hist->n_pos, p_true = hist->n_neg;"
    end ;
    O.oi "free_hist(hist);" ;
    if do_cores then  O.oi "cpus_free(prm.cm);" ;

(* Print meta-information *)            
    List.iter
      (fun (k,vs) ->
        if k = "Relax" then
          let fmt = sprintf "Relax %s %%s %%s\\n" doc.Name.name in
          O.fi "fprintf(out,\"%s\",p_true > 0 ? \"Ok\" : \"No\",\"%s\");"
            fmt (String.escaped vs) ;
        else if k = "Prefetch" then begin
        end else
          let fmt = "%s=%s\\n" in
          O.fi "fprintf(out,\"%s\",\"%s\",\"%s\");"
            fmt (String.escaped k) (String.escaped vs))
      test.T.info ;
(* Prefetch shown whenever activated *)
    begin match Cfg.preload with
    | CustomPL ->
        let fmt = "%s=" in
        O.fi "fprintf(out,\"%s\",\"%s\");" fmt "Prefetch" ;
        O.oi "prefetch_dump(out,cmd->prefetch);" ;
        O.oi "putc('\\n',out);"
    | StaticPL|Static2PL ->
        let fmt = "%s=%s\\n" in
        let prf = get_prefetch_info test in
        O.fi "fprintf(out,\"%s\",\"%s\",\"%s\");" fmt "Prefetch" prf
    | NoPL|RandomPL -> ()
    end ;
(* Affinity info, as computed *)
    if dca then begin
      O.oi "if (cmd->aff_mode == aff_custom) {" ;
      let fmt = "%s=%s\\n" in
      O.fii "fprintf(out,\"%s\",\"%s\",\"%s\");"
        fmt "Affinity" (Affi.pp (Misc.as_some affi)) ;
      O.oi "}"
    end ;
(* Observation summary *)
    let fmt =
      sprintf
        "Observation %s %%s %%\"PCTR\" %%\"PCTR\"\\n"
        doc.Name.name  in
    let obs = "!p_true ? \"Never\" : !p_false ? \"Always\" : \"Sometimes\"" in
    O.fi "fprintf(out,\"%s\",%s,p_true,p_false) ;" fmt obs;      
(* Show running time *)
    let fmt = sprintf "Time %s %%.2f\\n"  doc.Name.name in
    O.fi "fprintf(out,\"%s\",total / 1000000.0) ;" fmt ;
    if Cfg.signaling then begin
      let fmt = "Signaled %s\\n" in
      let arg = "prm.sig_cell ? \"Yes\" : \"No\"" in
      O.fi "fprintf(out,\"%s\",%s);" fmt arg
    end ;
    O.oi "fflush(out);" ;
    O.oi "return(p_true && p_false);" ;
    O.o "}" ;
    ()

(* Main *)

  let dump_main doc _env test = 
    let dca,_ca = mk_dca test in
    O.o "" ;
(* Static list of logical processors *)
    begin match Cfg.logicalprocs with
    | Some procs when do_affinity ->
        O.f "static int logical_procs[] = {%s};" (LexSplit.pp_ints procs) ;
        O.o "" ;
        ()
    | None|Some _ -> ()
    end ;
    let outchan = 
      match driver with
      | Driver.Shell ->
          O.o "int main(int argc, char **argv) {" ;
          "stdout"
      | Driver.C|Driver.XCode ->
          O.f "int %s(int argc, char **argv, FILE *out) {"
            (MyName.as_symbol doc) ;
          "out" in
    let alloc_def_all_cpus =
      if do_affinity then begin
        begin match Cfg.logicalprocs with
        | None ->
            if do_force_affinity then
              O.oi "cpus_t *def_all_cpus = read_force_affinity(AVAIL,0);"
            else
              O.oi "cpus_t *def_all_cpus = read_affinity();"
        | Some procs ->
            O.fi "cpus_t *def_all_cpus = cpus_create_init(%i,logical_procs);"
              (List.length procs)
        end ;
        true
      end else begin
        O.oi "cpus_t *def_all_cpus = NULL;" ;
        false
      end in
    if do_timebase && have_timebase then begin
      O.fi "int delta_t[] = {%s};"
        (String.concat ","
           (List.map (fun _ -> "DELTA_TB") test.T.code)) ;
      O.oi "ints_t delta_tb = { N, delta_t };" ;
    end ;
    let vars = get_global_names test
    and nprocs = List.length test.T.code in
    if do_custom then begin
      List.iter
        (fun (i,(out,_)) ->
          let addrs = A.Out.get_addrs out in
          O.fi "prfone_t _prf_t_%i[] = { %s };" i
            (String.concat ", "
               (List.map
                  (fun loc ->
                    sprintf "{ global_names[%i], none, }"
                      (find_index loc vars))
               addrs)) ;            
          O.fi "prfproc_t _prf_%i = { %i, _prf_t_%i}; "
            i (List.length addrs) i)
        test.T.code ;
      O.fi "prfproc_t _prf_procs_t[] = { %s };"
        (String.concat ", "
           (List.map
              (fun k -> sprintf "_prf_%i" k)
              (Misc.interval 0 nprocs))) ;
      O.fi "prfdirs_t _prefetch = { %i, _prf_procs_t }; " nprocs ;
      begin
        try
          let prf = List.assoc "Prefetch" test.T.info in
          O.fi "char _prefetch_txt[] = {%s};"
            (String.concat ","
               (List.map
                  (sprintf "'%c'")
                  (Misc.explode prf)@["'\\0'"])) ;
          O.oi
            "if (!parse_prefetch(_prefetch_txt,&_prefetch)) err(1,\"parse_prefetch\");"
        with Not_found -> ()
      end 
    end ;
    O.fi "cmd_t def = { 0, NUMBER_OF_RUN, SIZE_OF_TEST, STRIDE, AVAIL, 0, %s, %s, %i, AFF_INCR, def_all_cpus, %i, %s, %s, %s, %s, %s, %s, %s, %s};"
      (if do_sync_macro then "SYNC_N" else "0")
      (match affinity with
      | Affinity.No -> "aff_none"
      | Affinity.Incr _ -> "aff_incr"
      | Affinity.Random -> "aff_random"
      | Affinity.Custom ->
          if dca then "aff_custom"
          else begin
            Warn.warn_always
              "%s: custom affinity degraded to random affinity"
              doc.Name.name ;
            "aff_random"
          end)
      (if dca then 1 else 0)
      (match memory with | Direct -> -1 | Indirect -> 1)
      "MAX_LOOP"
      (if do_timebase && have_timebase then "&delta_tb" else "NULL")
      (if do_custom then "&_prefetch" else "NULL")
      (if do_staticpl then "1" else "-1")
      (if do_verbose_barrier then "1" else "-1")
      (if do_speedcheck then "1" else "-1")
      (match launch with | Fixed -> "1" | Changing -> "0")
      (if do_vp then "1" else "0") ;
    O.oi "cmd_t cmd = def;" ;
(* Parse command line *)
    O.oi "parse_cmd(argc,argv,&def,&cmd);" ;
    O.fi "run(&cmd,def_all_cpus,%s);" outchan;
    if alloc_def_all_cpus then begin
      O.oi "if (def_all_cpus != cmd.aff_cpus) cpus_free(def_all_cpus);"
    end ;
    O.oi "return  EXIT_SUCCESS;" ;
    O.o "}" ;
    ()

  let dump doc test =
(* Minimal type environemnt *)
    let env = build_env test in
    dump_header test ;
    dump_read_timebase () ;
    dump_threads test ;
    let cpys = dump_def_ctx env test in
    dump_cond_fun env test ;
    dump_defs_outs doc env test ;
    dump_reinit test cpys ;
    dump_check_globals env test ;
    dump_templates env doc.Name.name test ;
    dump_zyva doc cpys env test ;
    if do_vp then dump_report doc env test ;
    dump_run doc env test ;
    dump_main doc env test
end
    