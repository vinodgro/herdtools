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
  val asmcomment : string option
end

module Make(V:Constant.S)(C:Config) =
  struct
    module A = AArch64GenArch.Make(C)(V)
    open A
    open A.Out
    open Printf

(* No addresses in code *)
    let extract_addrs _ins = StringSet.empty

    let stable_regs _ins = A.RegSet.empty

(************************)
(* Template compilation *)
(************************)
    let remove_zrsp regs =
        List.filter
          (fun reg ->
              match reg with
              | X ZR | W ZR | X SP | W SP -> false
              | X _ | W _ -> true)
          regs

    let inst_regs_to_regs regs = List.map (function | X reg -> reg | W reg -> reg) regs

    let pp_input reg i =
      begin match reg with
      | X (Ireg _) -> sprintf "^i%d" i
      | W (Ireg _) -> sprintf "^wi%d" i
      end

    let pp_output reg i =
      begin match reg with
      | X (Ireg _) -> sprintf "^o%d" i
      | W (Ireg _) -> sprintf "^wo%d" i
      end

    let get_pps inputs outputs =
      let rec find_index p l e =
        begin match l with
        | [] -> None
        | e' :: l' -> if  e' = e then Some p else find_index (p + 1) l' e
        end
      in
      let find_output = find_index 0 outputs in
      let find_input = find_index 0 inputs in

      let pp_reg _sf reg =
        begin match reg with
        | X ZR -> "XZR"
        | W ZR -> "WZR"
        | X SP -> "SP"
        | W SP -> "WSP"
        | X (Ireg _)
        | W (Ireg _) ->
            begin match find_output reg with
            | Some i -> pp_output reg i
            | None ->
                begin match find_input reg with
                | Some i -> pp_input reg i
                | None ->
                  Printf.printf "\ninputs: ";
                  List.iter (fun reg -> Printf.printf ", %s" (AArch64GenBase.pp_regzr Set64 reg)) inputs;
                  Printf.printf "\noutputs: ";
                  List.iter (fun reg -> Printf.printf ", %s" (AArch64GenBase.pp_regzr Set64 reg)) inputs;
                  Printf.printf "\n*** %s ***\n" (AArch64GenBase.pp_regzr Set64 reg);
                  assert false
                end
            end
        end
      in

      (pp_reg, pp_reg, fun sf _ -> pp_reg sf)


(*    let input_reg sf i =
      match sf with
      | Set32 -> sprintf "^wi%d" i 
      | Set64 -> sprintf "^i%d" i

    let input_regzrbyext sf extend_type i =
      match extend_type with
      | ExtendType_UXTX | ExtendType_SXTX -> input_reg sf i
      | _ -> input_reg Set32 i

    let output_reg sf i =
      match sf with
      | Set32 -> sprintf "^wo%d" i
      | Set64 -> sprintf "^o%d" i*)


    let do_compile_ins tr_lab (ins : A.instruction) k =
      let (outputs, inputs) =
        begin match ins with
        (* #include "src_aarch64_hgen/regs_out_in.hgen" *)
        end
      in
      let outputs = remove_zrsp outputs in
      let inputs = remove_zrsp inputs in
      let (pp_regzr, pp_regsp, pp_regzrbyext) = get_pps inputs outputs in

      let c_ins =
        { empty_ins with
          memo = instruction_printer pp_regzr pp_regsp pp_regzrbyext ins;
          inputs = inst_regs_to_regs inputs;
          outputs = inst_regs_to_regs outputs;
        }
      in
      c_ins::k

(*      let c_ins =
        begin match ins with
        (* Generated fix-point instructions *)
        (* #include "src_aarch64_hgen/compile.hgen" *)
        end
      in
      c_ins::k*)

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
