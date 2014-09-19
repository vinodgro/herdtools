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
  val noccs : int
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

(*************)
(* Utilities *)
(*************)

  module U = SkelUtil.Make(P)(A)(T)

  let find_addr_type a env = U.find_type (A.Location_global a) env

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
    O.f "#define NOCCS %i" Cfg.noccs ;
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
  O.f "#define NSTEPS %i" nsteps ;
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

  let dump_addr_idx s = sprintf "_idx_%s" s

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
        O.fi "%s %s;"
          (if CType.is_ptr t then "int" else CType.dump t)
          (dump_loc_tag loc))
      locs ;
    O.o "} log_t;" ;
    O.o "" ;
    if U.ptr_in_outs env test then begin
      List.iteri
        (fun k (a,_) ->
          O.f "static const int %s = %i;"
            (dump_addr_idx a) (k+1))
        test.T.globals ;
      O.o "" ;
    (*  Translation to indices *)
      let dump_test (s,_) =
        O.fi "else if (v_addr == %s) return %s;"
          s (dump_addr_idx s) in
      O.f "static int idx_addr(void *v_addr%s) {"
        (String.concat ""
           (List.map
              (fun (a,_) -> sprintf ",void *%s" a)
              test.T.globals)) ;      
      O.oi "if (v_addr == NULL) { return 0;}" ;
      List.iter dump_test test.T.globals ;
      O.oi "else { fatal(\"???\"); return -1;}" ;
      O.o "}" ;
      O.o "" ;
(* Pretty-print indices *)
      let naddrs = List.length test.T.globals in
      O.f "static char *pretty_addr[%i] = {\"0\",%s};"
        (naddrs+1)
        (String.concat ""
           (List.map (fun (s,_) -> sprintf "\"%s\"," s) test.T.globals)) ;
      O.o "" ;
    end ;

    O.o "/* Dump of outcome */" ;
    O.o "static void pp_log(FILE *chan,log_t *p) {" ;
    let fmt = fmt_outcome env locs
    and args =
      A.LocSet.map_list
        (fun loc ->
          if U.is_ptr loc env then
            sprintf "pretty_addr[p->%s]" (dump_loc_tag loc)
          else
            sprintf "p->%s" (dump_loc_tag loc)) locs in
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
            | Symbolic s -> dump_addr_idx s
        end
        module Loc = struct
          type t = A.location
          let compare = A.location_compare
          let dump loc = sprintf "p->%s" (dump_loc_tag loc)
        end
      end)


  let dump_cond_fun env test =    
    let cond = test.T.condition in
    DC.fundef_onlog cond ;
    O.o "inline static int final_ok(log_t *p) {" ;
    O.fi
      "return %sfinal_cond(p);"
      (let open ConstrGen in
      match cond with
      | ExistsState _|NotExistsState _ -> ""
      | ForallStates _ -> "!") ;
    O.o "}" ;
    O.o ""

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

  let pvtag s = s ^"p"
  let pdtag i = sprintf "d%i" i
  let pctag (i,s) = sprintf "c_%i_%s" i s

  let get_param_vars test = match  test.T.globals with
  | [] -> []
  | _::xs -> xs

  let get_param_delays test  = Misc.interval 1 (T.get_nprocs test)

  let mk_get_param_pos test =
    let xs =  get_param_vars test in
    let _,m =
      List.fold_left
        (fun (i,m) (a,_) -> i+1,StringMap.add a (i+1) m)
        (0,StringMap.empty) xs in
    fun x -> StringMap.find x m

  let get_param_caches test =
    let r =
      List.map
        (fun (proc,(out,_)) ->
          List.map (fun a -> proc,a) (A.Out.get_addrs out))
        test.T.code in
    List.flatten r

  let dump_parameters env test =    
    let v_tags = List.map (fun (s,_) -> pvtag s) (get_param_vars test)
    and d_tags = List.map pdtag (get_param_delays test)
    and c_tags = List.map pctag (get_param_caches test) in
      
    O.o "/**************/" ;
    O.o "/* Parameters */" ;
    O.o "/**************/" ;
    O.o "" ;
    O.o "typedef enum { cflush, ctouch, cmax, } dir_t;" ;
    O.o "" ;
    O.o "typedef struct {" ;
    O.oi "int part;" ;
    O.fi "int %s;" (String.concat "," v_tags) ;
    O.fi "int %s;" (String.concat "," d_tags) ;
    O.fi "dir_t %s;" (String.concat "," c_tags) ;
    O.o "} param_t;" ;
    O.o "" ;
    let all_tags = "part"::v_tags@d_tags@c_tags in
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
    ObjUtil.insert_lib_file O.o "_instance.c" ;
    O.o "" ;
    ()

(*****************)
(* Run test code *)
(*****************)

let dump_run_thread
    env test get_param_pos global_env (proc,(out,(outregs,envVolatile))) =
  let my_regs = U.select_proc proc env in
  let addrs = A.Out.get_addrs out in
  O.fi "case %i: {" proc ;
  (* Delays *)
  O.oii "int _delay = DELTA_TB;" ;
  if proc <> 0 then
    O.fii "_delay += (_p->d%i - (NSTEPS-1)/2)*STEP;" proc ;
  (* Define locations *)
  let addrs0 =  List.map fst test.T.globals in
  List.iter
    (fun addr ->
      let t =  CType.dump (find_addr_type addr env) in
      try
        let pos = get_param_pos addr in
        O.fii "volatile %s *%s = _mem + LINESZ*_p->%s + %i;"
          t addr (pvtag addr) pos
      with Not_found ->
        O.fii "volatile %s *%s = _mem;" t addr)
    (if proc = 0 then addrs0 else addrs) ;
  (* Initialize them, if role is zero *)
  if proc = 0 then begin
    List.iter
      (fun (a,t) ->
        let v = A.find_in_state (A.Location_global a) test.T.init in
        let ins =
          U.do_store t (sprintf "*%s" a)
            (let open Constant in
            match v with
            | Concrete i -> sprintf "%i" i
            | Symbolic s ->
                let t2 = find_addr_type s env in
                if t=t2 then s else
                sprintf "(%s)%s" (CType.dump t) s) in
        O.fii "%s; cache_flush((void *)%s);" ins a)
      test.T.globals
  end ;
  (* And cache-instruct them *)
  O.oii "barrier_wait(_b);" ;
  List.iter (fun addr ->
    O.fii "if (_p->%s == ctouch) cache_touch((void *)%s);"
      (pctag (proc,addr)) addr ;
    O.fii "else cache_flush((void *)%s);" addr)
    addrs ;
  (* Synchronise *)
  if have_timebase then O.oii "_ctx->next_tb = read_timebase();" ;
  O.oii "barrier_wait(_b);" ;
  if have_timebase then begin
    O.oii "tb_t _tb0 = _ctx->next_tb;" ;
    O.oii "int _delta;" ;
    O.oii "do { _delta = read_timebase() - _tb0; } while (_delta < _delay);"
  end ;
  (* Dump code *)
  Lang.dump 
    O.out (Indent.as_string Indent.indent2)
    my_regs global_env envVolatile proc out ;
  let locs = U.get_final_globals test in
  O.oii "barrier_wait(_b);" ;
  if proc = 0 then begin
    A.LocSet.iter
      (fun loc ->
        let tag = dump_loc_tag loc in
        O.fii "%s = *%s;" (OutUtils.fmt_presi_index tag) tag)
      locs ;
    O.oii "hash_add(&_ctx->t,_log,_p,1);" ;
    O.oii "if (final_ok(_log)) ok = 1;"
  end ;
  O.oii "break; }" ;
  ()
  
let dump_run_def env test =
  O.o "/*************/" ;
  O.o "/* Test code */" ;
  O.o "/*************/" ;
  O.o "" ;
  O.o "inline static int do_run(thread_ctx_t *_c, param_t *_p) {" ;
  O.oi "int ok = 0;" ;
  O.oi "int _role = _c->role;" ;
  O.oi "if (_role < 0) return ok;" ;
  O.oi "ctx_t *_ctx = _c->ctx;" ;
  O.oi "int *_mem = _ctx->mem;" ;
  O.oi "sense_t *_b = &_ctx->b;" ;
  O.oi "log_t *_log = &_ctx->out;" ;
  O.o "" ;
  O.oi "barrier_wait(_b);" ;
  O.oi "switch (_role) {" ;
  let global_env = U.select_global env in
  List.iter
    (dump_run_thread env test (mk_get_param_pos test) global_env)
    test.T.code ;
  O.oi "}" ;  
  O.oi "return ok;" ;
  O.o "}" ;
  O.o ""

(********)
(* zyva *)
(********)

let dump_scan_def tname env test =
  O.o "/*******************/" ;
  O.o "/* Forked function */" ;
  O.o "/*******************/" ;
  O.o "" ;
  O.o "typedef struct {" ;
  O.oi "int id;" ;
  O.oi "global_t *g;" ;
  O.o "} scan_t;" ;
  O.o "" ;
  O.o "static void *scan(void *_a) {" ;
  O.oi "scan_t *a = (scan_t*)_a;" ;
  O.oi "int id = a->id;" ;
  O.oi "global_t *g = a->g;" ;
  O.oi "param_t p; " ;
  O.oi "thread_ctx_t c;" ;
  O.o "" ;
  O.oi "c.id = id;" ;
  O.oi
    (if Cfg.force_affinity then
      sprintf
      "force_one_affinity(id,AVAIL,g->verbose,\"%s\");"
        tname
    else
      "write_one_affinity(id);") ;
  O.oi "init_global(g,id);" ;
  O.oi "int nrun = 0;" ;
  O.oi "g->ok = 0;" ;
  O.oi "do {" ;
  O.oii "for (p.part = 0 ; p.part < SCANSZ ; p.part++) {" ;
  O.oiii "set_role(g,&c,p.part);" ;
  (* Enumerate parameters *)
  let rec loop_delays i = function
    | [] ->
        O.ox i "if (do_run(&c,&p)) (void)__sync_add_and_fetch(&g->ok,1);" ;
        ()
    | d::ds ->
        let tag = pdtag d in
        O.fx i "for (p.%s = 0 ; p.%s < NSTEPS ; p.%s++)" tag tag tag ;
        loop_delays (Indent.tab i) ds in
  let rec loop_caches i = function
    | [] -> loop_delays i (get_param_delays test)
    | c::cs ->
        let tag = pctag c in
        O.fx i "for (p.%s = 0 ; p.%s < cmax ; p.%s++)" tag tag tag ;
        loop_caches (Indent.tab i) cs in
  let rec loop_vars i = function
    | [] -> loop_caches i (get_param_caches test)
    | (x,_)::xs ->
        let tag = pvtag x in
        O.fx i "for (p.%s = 0 ; p.%s < NVARS ; p.%s++)" tag tag tag ;
        loop_vars (Indent.tab i) xs in
  loop_vars Indent.indent3 (get_param_vars test) ;
  O.oii "}" ;
  O.oii "barrier_wait(&g->gb);" ;
  O.oii "if (++nrun >= g->nruns) break;" ;
  O.oi "} while (g->ok < g->noccs);" ;
  O.oi "return NULL;" ;
  O.o "}" ;
  O.o ""

(********)
(* Main *)
(********)

let dump_main_def env test =
  ObjUtil.insert_lib_file O.o "_main.c" ;
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
    dump_run_def env test ;
    dump_scan_def doc.Name.name env test ;
    dump_main_def env test ;
    ()
    
end
