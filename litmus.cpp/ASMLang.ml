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

open Constant
open ConstrGen
open Speedcheck
open Speedcheck
open Preload
open Barrier
open Printf

module Make (Cfg : Language.Config) (O : Indent.S) (U : Language.Utils) = struct

  module T = U.T
  module A = T.A
  module C = T.C

  open U

  let dump_test env tname test =
    O.f "/***************/" ;
    O.f "/* Litmus code */" ;
    O.f "/***************/" ;
    O.f "" ;
    O.f "typedef struct {" ;
    O.fi "int th_id; /* I am running on this thread */" ;
    if do_affinity then O.fi "int *cpu; /* On this cpu */" ;
    if do_timebase && U.have_timebase then
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
          if do_staticNpl then begin
            match Cfg.preload with
            | Preload.StaticNPL Preload.One ->
                iter
                  (fun f loc ->
                    O.fx iloop "%s(%s);" f loc)
            | Preload.StaticNPL Preload.Two ->
                iter
                  (fun f loc ->
                    O.fx iloop "if (rand_bit(&(_a->seed))) %s(%s);" f loc)
            | _ -> assert false
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
            let rec aux = function
            | `PPCGen
            | `PPC ->
                O.fx iloop "asm __volatile__ (\"isync\" : : : \"memory\");"
            | `ARM ->
                O.fx iloop "asm __volatile__ (\"isb\" : : : \"memory\");"
            | `X86 -> ()
            | `C -> aux (Lazy.force Cfg.carch :> Archs.t)
            in
            aux A.arch
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
end
