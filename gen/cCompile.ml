(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* 'C' compiler *)

module type Config = sig
  include Top.Config
  include Cycle.Config
end

module Make(O:Config) : Builder.S
= struct
  module A = struct

(* Fences, to be completed *)
    type fence = SC

    let compare_fence = Pervasives.compare

    let strong = SC

    let pp_fence = function
      | SC -> "SC"

    let sig_of_fence = function
      | SC -> 'D'

    let fold_cumul_fences f k = f SC k
    let fold_all_fences = fold_cumul_fences
    let fold_some_fences = fold_cumul_fences

    let orders f d1 d2 = true

(* No dependencies *)
    type dp

    let pp_dp _ = assert false
    let sig_of_dp _ = assert false
    let fold_dpr  f k = k
    let fold_dpw f k = k

    let ddr_default = None
    let ddw_default = None
    let ctrlr_default = None
    let ctrlw_default = None

    let is_ctrlr _ = assert false
    let fst_dp _ = assert false
    let sequence_dp _ _ = assert false
  end

  module E = Edge.Make(A)
  module R = Relax.Make(A)(E)
  module C = Cycle.Make(O)(E)

end
