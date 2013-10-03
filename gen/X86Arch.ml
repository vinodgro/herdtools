(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

module Make(V:Constant.S) =
  struct
    include X86Base
    module V = V

(**********)
(* Fences *)
(**********)

    type fence = MFence

    let compare_fence = compare

    let strong = MFence

    let pp_fence = function
      | MFence -> "MFence"

    let sig_of_fence = function
      | MFence -> 'M'

    let fold_all_fences f r = f MFence r
    let fold_cumul_fences f r = f MFence r
    let fold_some_fences f r =  f MFence r

    let orders f d1 d2 = match f,d1,d2 with
    | MFence,_,_ -> true

(********)
(* Deps *)
(********)

    type dp

    let pp_dp _ = assert false

    let sig_of_dp _ = assert false

    let fold_dpr _f r =  r
    let fold_dpw _f r =  r

    let ddr_default = None
    let ddw_default = None
    let ctrlr_default = None
    let ctrlw_default = None

    let is_ctrlr _ = assert false
    let fst_dp _ = assert false
    let sequence_dp _ _ = assert false

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
