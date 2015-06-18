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
    open CType
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

    let inst_reg_to_reg = function | X reg -> reg | W reg -> reg

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

    let get_pps tr_lab inputs outputs =
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
                  (*Printf.printf "\ninputs: ";
                  List.iter (fun reg -> Printf.printf ", %s" (AArch64GenBase.pp_regzr Set64 reg)) inputs;
                  Printf.printf "\noutputs: ";
                  List.iter (fun reg -> Printf.printf ", %s" (AArch64GenBase.pp_regzr Set64 reg)) inputs;
                  Printf.printf "\n*** %s ***\n" (AArch64GenBase.pp_regzr Set64 reg);*)
                  assert false
                end
            end
        end
      in

      let pp_label label = A.Out.dump_label (tr_lab label) in

      (pp_reg, pp_reg, (fun sf _ -> (pp_reg sf)), pp_label)

    let get_reg_env voidstars outputs inputs =
      (* assuming outputs and inputs are disjoint *)
      let simple_regs = List.filter (fun e -> not (List.mem e voidstars)) (outputs @ inputs) in
      (List.map (fun reg -> (inst_reg_to_reg reg, voidstar)) voidstars) @
      (List.map (function X reg -> (reg, quad) | W reg -> (reg, word)) simple_regs)

    let do_compile_ins tr_lab (ins : A.instruction) k =
      let (outputs, inputs, voidstars, branch) =
        begin match ins with
        (* #include "src_aarch64_hgen/regs_out_in.hgen" *)
        | `AArch64BranchImmediate_label (_branch_type, label) ->
            ([], [], [], [Branch label])
        | `AArch64BranchConditional_label (label, _condition) ->
            ([], [], [], [Next; Branch label])
        | `AArch64CompareAndBranch_label (t, _datasize, _iszero, label) ->
            ([], [t], [], [Next; Branch label])
        | `AArch64TestBitAndBranch_label (t, _datasize, _bit_pos, _bit_val, label) ->
            ([], [t], [], [Next; Branch label])
        end
      in
      let outputs = remove_zrsp outputs in
      let inputs = remove_zrsp inputs in
      let voidstars = remove_zrsp voidstars in
      let (pp_regzr, pp_regsp, pp_regzrbyext, pp_label) = get_pps tr_lab inputs outputs in

      let c_ins =
        { empty_ins with
          memo = instruction_printer pp_regzr pp_regsp pp_regzrbyext pp_label ins;
          inputs = List.map inst_reg_to_reg inputs;
          outputs = List.map inst_reg_to_reg outputs;
          reg_env = get_reg_env voidstars outputs inputs;
          branch = branch;
        }
      in
      c_ins::k

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
