(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf
open Code

module Make(V:Constant.S) =
  struct
    include AArch64Base
    include MachAtom
    module V = V

(**********)
(* Fences *)
(**********)

    type fence = barrier

    let is_isync = function
      | ISB -> true
      | _ -> false

    let compare_fence = barrier_compare

    let strong = DMB (SY,FULL)

    let pp_fence f = do_pp_barrier "." f

    let sig_of_domain = function
      | NSH -> 0
      | ISH -> 1
      | OSH -> 2
      | SY -> 3

    let sig_of_type = function
      | LD -> 0
      | ST -> 1
      | FULL -> 2

    let sig_of_options d t = 3 * sig_of_domain d + sig_of_type t

    let sig_of_fence = function
      | DSB (d,t) -> Char.chr (128 + 2 * (sig_of_options d t))  
      | DMB (d,t) -> Char.chr (128 + 2 * (sig_of_options d t)+1)
      | ISB -> 'I'

    let fold_cumul_fences f k = do_fold_dmb_dsb f k

    let fold_all_fences f k = fold_barrier f k

    let fold_some_fences f k =
      let k = f ISB k  in
      let k = f (DMB (SY,FULL)) k in
      let k = f (DMB (SY,ST)) k in
      let k = f (DMB (SY,LD)) k in
      k

    let orders f d1 d2 = match f,d1,d2 with
    | ISB,_,_ -> false
    | (DSB (_,FULL)|DMB (_,FULL)),_,_ -> true
    | (DSB (_,ST)|DMB (_,ST)),W,W -> true
    | (DSB (_,ST)|DMB (_,ST)),_,_ -> false
    | (DSB (_,LD)|DMB (_,LD)),R,(W|R) -> true
    | (DSB (_,LD)|DMB (_,LD)),_,_ -> false


(********)
(* Deps *)
(********)
    include Dep

    let pp_dp = function
      | ADDR -> "Addr"
      | DATA -> "Data"
      | CTRL -> "Ctrl"
      | CTRLISYNC -> "CtrlIsbc"

    include
        ArchExtra.Make
        (struct
          type arch_reg = reg

          let is_symbolic = function
            | Symbolic_reg _ -> true
            | _ -> false

          let pp_reg = pp_reg
          let free_registers = allowed_for_symb
        end)
  end
