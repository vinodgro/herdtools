(**************************************************************************)
(*                                  DIY                                   *)
(*                                                                        *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.          *)
(* Shaked Flur, Susmit Sarkar, Peter Sewell, University of Cambridge, UK. *)
(*                                                                        *)
(*  Copyright 2015 Institut National de Recherche en Informatique et en   *)
(*  Automatique and the authors. All rights reserved.                     *)
(*  This file is distributed  under the terms of the Lesser GNU General   *)
(*  Public License.                                                       *)
(**************************************************************************)

module type Config = sig
  val word : Word.t
  val memory : Memory.t
  val morearch : MoreArch.t
  val cautious : bool
end

module Make(V:Constant.S)(C:Config) =
  struct
    module A = AArch64Arch.Make(C)(V)
    open A
    open A.Out
    open Printf

(* No addresses in code *)
    let extract_addrs _ins = StringSet.empty

    let stable_regs _ins = A.RegSet.empty

(************************)
(* Template compilation *)
(************************)
    let input_reg sf i =
      match sf with
      | Set32 -> sprintf "^wi%d" i
      | Set64 -> sprintf "^i%d" i

    let output_reg sf i =
      match sf with
      | Set32 -> sprintf "^wo%d" i
      | Set64 -> sprintf "^o%d" i

    let do_compile_ins tr_lab (ins : A.instruction) k =
      match ins with
      (* Generated fix-point instructions *)
      (* #include "src_aarch64_hgen/compile.hgen" *)

    let compile_ins tr_lab ins = do_compile_ins tr_lab ins

    let emit_loop k = k
      (* FIXME:
      let lbl1 = Label.next_label "L" in
      let lbl2 = Label.next_label "L" in
      cmpi loop_idx 0::
      b no_tr lbl2::
      emit_lbl lbl1::
      k@
      [ decr loop_idx 1;
        emit_lbl lbl2 ;
        bcc no_tr NE lbl1; ]
      *)

    (* FIXME: do we need these?
    let branch_neq r i lab k = cmpi r i::bcc no_tr NE lab::k
    let branch_eq r i lab k = cmpi r i::bcc no_tr EQ lab::k

    let signaling_write _i _k = Warn.fatal "no signaling write for ARM"
    *)

    let emit_tb_wait _ = Warn.fatal "no time base for ARM"


  end
