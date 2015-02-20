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

(** Define registers, barriers, and instructions for AArch64 *)

open Printf

(* Who am i ? *)
let arch = `AArch64

(* #include "./src_aarch64_hgen/types.hgen" *)

let big_in_range (n : Big_int.big_int) (min : int) (max : int) =
  let big_min = Big_int.big_int_of_int min in
  let big_max = Big_int.big_int_of_int max in
  (Big_int.le_big_int big_min n) && (Big_int.le_big_int n big_max)
  
(*************)
(* Registers *)
(*************)

type ireg =
  | GPR0 | GPR1 | GPR2 | GPR3
  | GPR4 | GPR5 | GPR6 | GPR7
  | GPR8 | GPR9 | GPR10 | GPR11
  | GPR12 | GPR13 | GPR14 | GPR15
  | GPR16 | GPR17 | GPR18 | GPR19
  | GPR20 | GPR21 | GPR22 | GPR23
  | GPR24 | GPR25 | GPR26 | GPR27
  | GPR28 | GPR29 | GPR30

(*type reg =
  | Xreg of ireg (* 64bit general purpose registers *)
  | XZR          (* 64bit zero register *)
  | SP           (* 64bit stack pointer *)
  | Wreg of ireg (* 32bit general purpose registers (lower half of xreg) *)
  | WZR          (* 32bit zero register *)
  | WSP          (* 32bit stack pointer (lower half of SP) *)
(*   | PC (* FIXME: do we need this *) *)
  | Symbolic_reg of string
(* Internal regs *)
  | Internal of int*)

type reg =
  | Ireg of ireg
  | ZR
  | SP
(*   | PC (* FIXME: do we need this *) *)
  | Symbolic_reg of string
(* Internal regs *)
  | Internal of int

type inst_reg =
  | X of reg
  | W of reg

(* FIXME: do we need these *)
let base =  Internal 0
and max_idx = Internal 1
and idx = Internal 2
and ephemeral = Internal 3
let loop_idx = Internal 4

(* let pc = PC	   *)

(*let regs =
  [
   Xreg GPR0,  "X0"  ; Xreg GPR1,  "X1"  ; Xreg GPR2,  "X2"  ; Xreg GPR3,  "X3"  ;
   Xreg GPR4,  "X4"  ; Xreg GPR5,  "X5"  ; Xreg GPR6,  "X6"  ; Xreg GPR7,  "X7"  ;
   Xreg GPR8,  "X8"  ; Xreg GPR9,  "X9"  ; Xreg GPR10, "X10" ; Xreg GPR11, "X11" ;
   Xreg GPR12, "X12" ; Xreg GPR13, "X13" ; Xreg GPR14, "X14" ; Xreg GPR15, "X15" ;
   Xreg GPR16, "X16" ; Xreg GPR17, "X17" ; Xreg GPR18, "X18" ; Xreg GPR19, "X19" ;
   Xreg GPR20, "X20" ; Xreg GPR21, "X21" ; Xreg GPR22, "X22" ; Xreg GPR23, "X23" ;
   Xreg GPR24, "X24" ; Xreg GPR25, "X25" ; Xreg GPR26, "X26" ; Xreg GPR27, "X27" ;
   Xreg GPR28, "X28" ; Xreg GPR29, "X29" ; Xreg GPR30, "X30" ;
   XZR, "XZR" ; SP, "SP" ;
   Wreg GPR0,  "X0"  ; Wreg GPR1,  "X1"  ; Wreg GPR2,  "X2"  ; Wreg GPR3,  "X3"  ;
   Wreg GPR4,  "X4"  ; Wreg GPR5,  "X5"  ; Wreg GPR6,  "X6"  ; Wreg GPR7,  "X7"  ;
   Wreg GPR8,  "X8"  ; Wreg GPR9,  "X9"  ; Wreg GPR10, "X10" ; Wreg GPR11, "X11" ;
   Wreg GPR12, "X12" ; Wreg GPR13, "X13" ; Wreg GPR14, "X14" ; Wreg GPR15, "X15" ;
   Wreg GPR16, "X16" ; Wreg GPR17, "X17" ; Wreg GPR18, "X18" ; Wreg GPR19, "X19" ;
   Wreg GPR20, "X20" ; Wreg GPR21, "X21" ; Wreg GPR22, "X22" ; Wreg GPR23, "X23" ;
   Wreg GPR24, "X24" ; Wreg GPR25, "X25" ; Wreg GPR26, "X26" ; Wreg GPR27, "X27" ;
   Wreg GPR28, "X28" ; Wreg GPR29, "X29" ; Wreg GPR30, "X30" ;
   WZR, "WZR" ; WSP, "WSP" ;
 ]*)
let regs =
  [
    Ireg GPR0,  "X0"  ; Ireg GPR1,  "X1"  ; Ireg GPR2,  "X2"  ; Ireg GPR3,  "X3"  ;
    Ireg GPR4,  "X4"  ; Ireg GPR5,  "X5"  ; Ireg GPR6,  "X6"  ; Ireg GPR7,  "X7"  ;
    Ireg GPR8,  "X8"  ; Ireg GPR9,  "X9"  ; Ireg GPR10, "X10" ; Ireg GPR11, "X11" ;
    Ireg GPR12, "X12" ; Ireg GPR13, "X13" ; Ireg GPR14, "X14" ; Ireg GPR15, "X15" ;
    Ireg GPR16, "X16" ; Ireg GPR17, "X17" ; Ireg GPR18, "X18" ; Ireg GPR19, "X19" ;
    Ireg GPR20, "X20" ; Ireg GPR21, "X21" ; Ireg GPR22, "X22" ; Ireg GPR23, "X23" ;
    Ireg GPR24, "X24" ; Ireg GPR25, "X25" ; Ireg GPR26, "X26" ; Ireg GPR27, "X27" ;
    Ireg GPR28, "X28" ; Ireg GPR29, "X29" ; Ireg GPR30, "X30" ;
    ZR, "XZR" ; SP, "SP" ;
  ]

let wregs =
  [
    Ireg GPR0,  "W0"  ; Ireg GPR1,  "W1"  ; Ireg GPR2,  "W2"  ; Ireg GPR3,  "W3"  ;
    Ireg GPR4,  "W4"  ; Ireg GPR5,  "W5"  ; Ireg GPR6,  "W6"  ; Ireg GPR7,  "W7"  ;
    Ireg GPR8,  "W8"  ; Ireg GPR9,  "W9"  ; Ireg GPR10, "W10" ; Ireg GPR11, "W11" ;
    Ireg GPR12, "W12" ; Ireg GPR13, "W13" ; Ireg GPR14, "W14" ; Ireg GPR15, "W15" ;
    Ireg GPR16, "W16" ; Ireg GPR17, "W17" ; Ireg GPR18, "W18" ; Ireg GPR19, "W19" ;
    Ireg GPR20, "W20" ; Ireg GPR21, "W21" ; Ireg GPR22, "W22" ; Ireg GPR23, "W23" ;
    Ireg GPR24, "W24" ; Ireg GPR25, "W25" ; Ireg GPR26, "W26" ; Ireg GPR27, "W27" ;
    Ireg GPR28, "W28" ; Ireg GPR29, "W29" ; Ireg GPR30, "W30" ;
    ZR, "WZR" ; SP, "WSP" ;
  ]

let parse_list =
  List.map (fun (r,s) -> s,r) regs

let parse_wlist =
  List.map (fun (r,s) -> s,r) wregs

let parse_reg s =
  try Some (List.assoc s parse_list)
  with Not_found -> None

let parse_wreg s =
  try Some (List.assoc s parse_wlist)
  with Not_found -> None

let pp_reg r =
  match r with
  | Symbolic_reg r -> "%" ^ r
  | Internal i -> Printf.sprintf "i%i" i
  | _ -> try List.assoc r regs with Not_found -> assert false


let reg_compare = Pervasives.compare

let is_zero_reg r = (r = X ZR || r = W ZR)
let is_sp_reg r = (r = X SP || r = W SP)

let ireg_to_int r =
  match r with
  | GPR0 -> 0   | GPR1 -> 1   | GPR2 -> 2   | GPR3 -> 3   | GPR4 -> 4   | GPR5 -> 5   | GPR6 -> 6   | GPR7 -> 7
  | GPR8 -> 8   | GPR9 -> 9   | GPR10 -> 10 | GPR11 -> 11 | GPR12 -> 12 | GPR13 -> 13 | GPR14 -> 14 | GPR15 -> 15
  | GPR16 -> 16 | GPR17 -> 17 | GPR18 -> 18 | GPR19 -> 19 | GPR20 -> 20 | GPR21 -> 21 | GPR22 -> 22 | GPR23 -> 23
  | GPR24 -> 24 | GPR25 -> 25 | GPR26 -> 26 | GPR27 -> 27 | GPR28 -> 28 | GPR29 -> 29 | GPR30 -> 30

let ireg_of_int i =
  match i with
  | 0 -> GPR0   | 1 -> GPR1   | 2 -> GPR2   | 3 -> GPR3   | 4 -> GPR4   | 5 -> GPR5   | 6 -> GPR6   | 7 -> GPR7
  | 8 -> GPR8   | 9 -> GPR9   | 10 -> GPR10 | 11 -> GPR11 | 12 -> GPR12 | 13 -> GPR13 | 14 -> GPR14 | 15 -> GPR15
  | 16 -> GPR16 | 17 -> GPR17 | 18 -> GPR18 | 19 -> GPR19 | 20 -> GPR20 | 21 -> GPR21 | 22 -> GPR22 | 23 -> GPR23
  | 24 -> GPR24 | 25 -> GPR25 | 26 -> GPR26 | 27 -> GPR27 | 28 -> GPR28 | 29 -> GPR29 | 30 -> GPR30

let inst_reg_to_int r =
  match r with
  | X (Ireg r') | W (Ireg r') -> ireg_to_int r'
  | X ZR | X SP | W ZR | W SP -> 31
  (* TODO: do we need to handle the other regs? *)

(*let xreg_of_int i =
  match i with
  | 0 -> Xreg GPR0   | 1 -> Xreg GPR1   | 2 -> Xreg GPR2   | 3 -> Xreg GPR3   | 4 -> Xreg GPR4   | 5 -> Xreg GPR5   | 6 -> Xreg GPR6   | 7 -> Xreg GPR7
  | 8 -> Xreg GPR8   | 9 -> Xreg GPR9   | 10 -> Xreg GPR10 | 11 -> Xreg GPR11 | 12 -> Xreg GPR12 | 13 -> Xreg GPR13 | 14 -> Xreg GPR14 | 15 -> Xreg GPR15
  | 16 -> Xreg GPR16 | 17 -> Xreg GPR17 | 18 -> Xreg GPR18 | 19 -> Xreg GPR19 | 20 -> Xreg GPR20 | 21 -> Xreg GPR21 | 22 -> Xreg GPR22 | 23 -> Xreg GPR23
  | 24 -> Xreg GPR24 | 25 -> Xreg GPR25 | 26 -> Xreg GPR26 | 27 -> Xreg GPR27 | 28 -> Xreg GPR28 | 29 -> Xreg GPR29 | 30 -> Xreg GPR30*)



(************)
(* Barriers *)
(************)

type barrier =
  | DMB of mBReqDomain*mBReqTypes
  | DSB of mBReqDomain*mBReqTypes
  | ISB

let all_kinds_of_barriers =
  [
    DMB (MBReqDomain_FullSystem, MBReqTypes_All);
    DMB (MBReqDomain_FullSystem, MBReqTypes_Reads);
    DMB (MBReqDomain_FullSystem, MBReqTypes_Writes);
    DSB (MBReqDomain_FullSystem, MBReqTypes_All);
    DSB (MBReqDomain_FullSystem, MBReqTypes_Reads);
    DSB (MBReqDomain_FullSystem, MBReqTypes_Writes);
    ISB;
  ]

let pp_option domain types =
  match (domain,types) with
  | (MBReqDomain_OuterShareable, MBReqTypes_Reads)  -> "OSHLD"
  | (MBReqDomain_OuterShareable, MBReqTypes_Writes) -> "OSHST"
  | (MBReqDomain_OuterShareable, MBReqTypes_All)    -> "OSH"
  | (MBReqDomain_Nonshareable,   MBReqTypes_Reads)  -> "NSHLD"
  | (MBReqDomain_Nonshareable,   MBReqTypes_Writes) -> "NSHST"
  | (MBReqDomain_Nonshareable,   MBReqTypes_All)    -> "NSH"
  | (MBReqDomain_InnerShareable, MBReqTypes_Reads)  -> "ISHLD"
  | (MBReqDomain_InnerShareable, MBReqTypes_Writes) -> "ISHST"
  | (MBReqDomain_InnerShareable, MBReqTypes_All)    -> "ISH"
  | (MBReqDomain_FullSystem,     MBReqTypes_Reads)  -> "LD"
  | (MBReqDomain_FullSystem,     MBReqTypes_Writes) -> "ST"
  | (MBReqDomain_FullSystem,     MBReqTypes_All)    -> "SY"

let pp_barrier b =
  match b with
  | DMB(domain,types) -> "DMB " ^ (pp_option domain types)
  | DSB(domain,types) -> "DSB " ^ (pp_option domain types)
  | ISB -> "ISB"

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

type label = Label.t

type instruction =
  [
    (* FIXME: the following line should import the ast generated by sail *)
    (* #include "./src_aarch64_hgen/ast.hgen" *)

    (* Branch *)
    (* these instructions take a 'label' instead of offset (bit64) *)
    | `AArch64BranchImmediate_label of branchType*label (* _branch_type,offset *)
    | `AArch64BranchConditional_label of label*bit4 (* offset,condition *)
  ]


(*** TODO: maybe put these in a different file? ***)
(*** pretty aux functions ***)

let pp_imm imm = sprintf "#%d" imm
let pp_big_imm = Big_int.string_of_big_int

let pp_label page offset = pp_big_imm offset (* FIXME: make this right *)

let pp_withflags inst setflags =
  if setflags then (inst ^ "S")
  else inst

let pp_regzr sf r = (*pp_reg r*)
  match r with
  | X ZR -> "XZR"
  | X (Ireg i) -> sprintf "X%d" (ireg_to_int i)
  | X (Symbolic_reg s) -> "X" ^ s
  | W ZR -> "WZR"
  | W (Ireg i) -> sprintf "W%d" (ireg_to_int i)
  | W (Symbolic_reg s) -> "W" ^ s

let pp_regsp sf r = (*pp_reg r*)
  match r with
  | X SP -> "SP"
  | X (Ireg i) -> sprintf "X%d" (ireg_to_int i)
  | X (Symbolic_reg s) -> "X" ^ s
  | W SP -> "WSP"
  | W (Ireg i) -> sprintf "W%d" (ireg_to_int i)
  | W (Symbolic_reg s) -> "W" ^ s

let pp_regzrbyext extend_type r = (*pp_reg r*)
  (*match extend_type with
  | ExtendType_UXTX | ExtendType_SXTX -> pp_regzr Set64 r
  | _ -> pp_regzr Set32 r*)
  match r with
  | X ZR -> "XZR"
  | X (Ireg i) -> sprintf "X%d" (ireg_to_int i)
  | X (Symbolic_reg s) -> "X" ^ s
  | W ZR -> "WZR"
  | W (Ireg i) -> sprintf "W%d" (ireg_to_int i)
  | W (Symbolic_reg s) -> "W" ^ s

let pp_regext ext =
  match ext with
  | ExtendType_UXTB -> "UXTB"
  | ExtendType_UXTH -> "UXTH"
  | ExtendType_UXTW -> "UXTW"
  | ExtendType_UXTX -> "UXTX"
  | ExtendType_SXTB -> "SXTB"
  | ExtendType_SXTH -> "SXTH"
  | ExtendType_SXTW -> "SXTW"
  | ExtendType_SXTX -> "SXTX"

let pp_shift shift =
  match shift with
  | ShiftType_LSL -> "LSL"
  | ShiftType_LSR -> "LSR"
  | ShiftType_ASR -> "ASR"
  | ShiftType_ROR -> "ROR"

let pp_shiftop shifttype =
  match shifttype with
  | ShiftType_ASR -> "ASR"
  | ShiftType_LSL -> "LSL"
  | ShiftType_LSR -> "LSR"
  | ShiftType_ROR -> "ROR"

let pp_ifnz amount =
  if amount = 0 then "" else (" " ^ (pp_imm amount))

let pp_addsub sub_op =
  pp_withflags (if sub_op then "SUB" else "ADD")

let pp_logop op setflags invert =
  match (op,invert) with
  | (LogicalOp_AND,false) -> if setflags then "ANDS" else "AND"
  | (LogicalOp_AND,true)     -> if setflags then "BICS" else "BIC"
  | (LogicalOp_EOR,false) -> "EOR" (*assert (not setflags)*)
  | (LogicalOp_EOR,true)     -> "EON" (*assert (not setflags)*)
  | (LogicalOp_ORR,false) -> "ORR" (*assert (not setflags)*)
  | (LogicalOp_ORR,true)     -> "ORN" (*assert (not setflags)*)

let pp_cond cond =
  match cond with
  | 0b0000 -> "EQ"
  | 0b0001 -> "NE"
  | 0b0010 -> "CS"
  | 0b0011 -> "CC"
  | 0b0100 -> "MI"
  | 0b0101 -> "PL"
  | 0b0110 -> "VS"
  | 0b0111 -> "VC"
  | 0b1000 -> "HI"
  | 0b1001 -> "LS"
  | 0b1010 -> "GE"
  | 0b1011 -> "LT"
  | 0b1100 -> "GT"
  | 0b1101 -> "LE"
  | 0b1110 -> "AL"
  | 0b1111 -> "NV"

let pp_bfm inzero extend =
  match (inzero,extend) with
  | (false,false) -> "BFM"
  | (true,true)   -> "SBFM"
  | (true,false)  -> "UBFM"

let pp_branchimmediate branch_type =
  match branch_type with
  | BranchType_JMP -> "B"
  | BranchType_CALL -> "BL"

let pp_branchregister branch_type =
  match branch_type with
  | BranchType_JMP -> "BR"
  | BranchType_CALL -> "BLR"
  | BranchType_RET -> "RET"

let pp_countop opcode =
  match opcode with
  | CountOp_CLS -> "CLS"
  | CountOp_CLZ -> "CLZ"

let pp_crc sz crc32c =
  let size =
    match sz with
    | 0b00 -> "B"
    | 0b01 -> "H"
    | 0b10 -> "W"
    | 0b11 -> "X"
    in
  "CRC" ^ (if crc32c then "C" else "") ^ size

let pp_csel else_inv else_inc =
  match (else_inv,else_inc) with
  | (false,false) -> "CSEL"
  | (false,true)  -> "CSINC"
  | (true,false)  -> "CSINV"
  | (true,true)   -> "CSNEG"

let pp_barr op =
  match op with
  | MemBarrierOp_DSB -> "DSB"
  | MemBarrierOp_DMB -> "DMB"
  | MemBarrierOp_ISB -> "ISB"

let pp_barroption domain types =
  if domain = MBReqDomain_FullSystem && types = MBReqTypes_All then "SY"
  else
  let pref =
    match domain with
    | MBReqDomain_OuterShareable -> "OSH"
    | MBReqDomain_Nonshareable ->   "NSH"
    | MBReqDomain_InnerShareable -> "ISH"
    | MBReqDomain_FullSystem ->     "" in
  let suff =
    match types with
    | MBReqTypes_Reads ->  "LD"
    | MBReqTypes_Writes -> "ST"
    | MBReqTypes_All ->    "" in
  pref ^ suff

let pp_ldaxstlxp memop acctype excl pair datasize =
  (match memop with
  | MemOp_STORE -> "ST" ^ (if acctype = AccType_ORDERED then "L" else "")
  | MemOp_LOAD ->  "LD" ^ (if acctype = AccType_ORDERED then "A" else ""))
  ^
  (if excl then "X" else "")
  ^
  (if pair then "P" else
  match datasize with
  | 32 | 64 -> "R"
  | 8 ->       "RB"
  | 16 ->      "RH")

let pp_movwide opcode =
  match opcode with
  | MoveWideOp_N -> "MOVN"
  | MoveWideOp_Z -> "MOVZ"
  | MoveWideOp_K -> "MOVK"

let pp_maddsubl sub_op unsigned =
  (if unsigned then "U" else "S") ^ "M" ^ (pp_addsub sub_op false) ^ "L"

let moveWidePreferred sf imm = false (* FIXME: implement the function from arm arm *)
let bFXPreferred sf opc1 imms immr = false (* FIXME: implement the function from arm arm *)

let pp_prfop prfop =
  match prfop with
  | 0b00000 -> "PLDL1KEEP"
  | 0b00001 -> "PLDL1STRM"
  | 0b00010 -> "PLDL2KEEP"
  | 0b00011 -> "PLDL2STRM"
  | 0b00100 -> "PLDL3KEEP"
  | 0b00101 -> "PLDL3STRM"
  (**)
  | 0b01000 -> "PLIL1KEEP"
  | 0b01001 -> "PLIL1STRM"
  | 0b01010 -> "PLIL2KEEP"
  | 0b01011 -> "PLIL2STRM"
  | 0b01100 -> "PLIL3KEEP"
  | 0b01101 -> "PLIL3STRM"
  (**)
  | 0b10000 -> "PSTL1KEEP"
  | 0b10001 -> "PSTL1STRM"
  | 0b10010 -> "PSTL2KEEP"
  | 0b10011 -> "PSTL2STRM"
  | 0b10100 -> "PSTL3KEEP"
  | 0b10101 -> "PSTL3STRM"
  | _ -> (pp_imm prfop)

let pp_reverse sf op =
  match op with
  | RevOp_RBIT -> "RBIT"
  | RevOp_REV64 -> "REV"
  | RevOp_REV16 -> "REV16"
  | RevOp_REV32 -> if sf = Set32 then "REV" else "REV32"

(*** TODO: end ***)

let do_pp_instruction i =
  match i with
  (* #include "./src_aarch64_hgen/pretty.hgen" *)
  | _ -> failwith "unrecognised instruction"


let pp_instruction _m i = do_pp_instruction i

let dump_instruction = do_pp_instruction

(****************************)
(* Symbolic registers stuff *)
(****************************)

(*let allowed_for_symb =
  [ Xreg GPR0  ; Xreg GPR1  ; Xreg GPR2  ; Xreg GPR3  ;
    Xreg GPR4  ; Xreg GPR5  ; Xreg GPR6  ; Xreg GPR7  ;
    Xreg GPR8  ; Xreg GPR9  ; Xreg GPR10 ; Xreg GPR11 ;
    Xreg GPR12 ; Xreg GPR13 ; Xreg GPR14 ; Xreg GPR15 ;
    Xreg GPR16 ; Xreg GPR17 ; Xreg GPR18 ; Xreg GPR19 ;
    Xreg GPR20 ; Xreg GPR21 ; Xreg GPR22 ; Xreg GPR23 ;
    Xreg GPR24 ; Xreg GPR25 ; Xreg GPR26 ; Xreg GPR27 ;
    Xreg GPR28 ; Xreg GPR29 ; Xreg GPR30 ;
    Wreg GPR0  ; Wreg GPR1  ; Wreg GPR2  ; Wreg GPR3  ;
    Wreg GPR4  ; Wreg GPR5  ; Wreg GPR6  ; Wreg GPR7  ;
    Wreg GPR8  ; Wreg GPR9  ; Wreg GPR10 ; Wreg GPR11 ;
    Wreg GPR12 ; Wreg GPR13 ; Wreg GPR14 ; Wreg GPR15 ;
    Wreg GPR16 ; Wreg GPR17 ; Wreg GPR18 ; Wreg GPR19 ;
    Wreg GPR20 ; Wreg GPR21 ; Wreg GPR22 ; Wreg GPR23 ;
    Wreg GPR24 ; Wreg GPR25 ; Wreg GPR26 ; Wreg GPR27 ;
    Wreg GPR28 ; Wreg GPR29 ; Wreg GPR30 ]*)
let allowed_for_symb =
  [ Ireg GPR0  ; Ireg GPR1  ; Ireg GPR2  ; Ireg GPR3  ;
    Ireg GPR4  ; Ireg GPR5  ; Ireg GPR6  ; Ireg GPR7  ;
    Ireg GPR8  ; Ireg GPR9  ; Ireg GPR10 ; Ireg GPR11 ;
    Ireg GPR12 ; Ireg GPR13 ; Ireg GPR14 ; Ireg GPR15 ;
    Ireg GPR16 ; Ireg GPR17 ; Ireg GPR18 ; Ireg GPR19 ;
    Ireg GPR20 ; Ireg GPR21 ; Ireg GPR22 ; Ireg GPR23 ;
    Ireg GPR24 ; Ireg GPR25 ; Ireg GPR26 ; Ireg GPR27 ;
    Ireg GPR28 ; Ireg GPR29 ; Ireg GPR30 ]

let fold_regs (f_reg,f_sreg) =
  (* Let us have a functional style... *)
  let fold_reg reg (y_reg,y_sreg) =
    match reg with
    | X (Symbolic_reg s)
    | W (Symbolic_reg s) -> y_reg, f_sreg s y_sreg
(*     | Xreg _ | XZR | SP | Wreg _ | WZR | WSP -> f_reg reg y_reg, y_sreg *)
    | X ((Ireg _) as reg') | X (ZR as reg') | X (SP as reg')
    | W ((Ireg _) as reg') | W (ZR as reg') | W (SP as reg') -> f_reg reg' y_reg, y_sreg
    | _ -> y_reg, y_sreg in

  fun (y_reg,y_sreg as c) ins ->
    match ins with
    (* #include "./src_aarch64_hgen/fold.hgen" *)
    | _ -> c

(* Map over symbolic regs *)
let map_regs f_reg f_symb =
  let map_reg reg =
    match reg with
    | X (Symbolic_reg s) -> X (f_symb s)
    | W (Symbolic_reg s) -> W (f_symb s)
    (*| Xreg _ | XZR | SP | Wreg _ | WZR | WSP -> f_reg reg*)
    | X ((Ireg _) as reg') | X (ZR as reg') | X (SP as reg') -> X (f_reg reg')
    | W ((Ireg _) as reg') | W (ZR as reg') | W (SP as reg') -> W (f_reg reg')
    | _ -> reg in

  fun ins ->
    match ins with
    (* #include "./src_aarch64_hgen/map.hgen" *)
    | _ -> ins

(* No addresses burried in ARM code *)
let fold_addrs _f c _ins = c

let map_addrs _f ins = ins

let norm_ins ins = ins

(* Instruction continuation *)
let get_next = function
  | _ -> [Label.Next]

include Pseudo.Make
  (struct
      type ins = instruction
      type reg_arg = reg

      let get_naccesses = function
        (* number of memory accesses *)
        (* XXX this should be guessable from pseudocode *)
        | _ ->  failwith "shouldn't need this for litmus"

      let fold_labels k f = function
        | _ -> k

      let map_labels f = function
        | ins -> ins

  end)

let get_macro _name = raise Not_found

(* #include "./src_aarch64_hgen/token_types.hgen" *)

(*** TODO: maybe put these in a different file? ***)
(*** parser aux functions ***)

(* check_bits: returns true iff any bit that is not between lsb and
               lsb+count-1 has the value 0 *)
let check_bits (n : Big_int.big_int) (lsb : int) (count : int) =
  Big_int.eq_big_int
    n
    (Big_int.shift_left_big_int (Big_int.extract_big_int n lsb count) lsb)

let iskbituimm k imm = 0 <= imm && imm < (1 lsl k)
let big_iskbituimm k imm = check_bits imm 0 k
let iskbitsimm k imm = -(1 lsl (k-1)) <= imm && imm < (1 lsl (k-1))

(* invert: inverts the least significant bit *)
let invert n = if n mod 2 = 0 then (n+1) else (n-1)

(* TODO: this is a function from ARM ARM, we have a better implementation
         in Sail and we should use it instead of this *)
let decodeBitMasks regSize (immN : int) (imms : int) (immr : int) (immediate : bool) =
  assert (regSize = 32 || regSize = 64) ;

  (* Compute log2 of element size *)
  (* 2^len must be in range [2, regSize] *)
  let len : int =
    if immN = 1 then 6
    else if 0b100000 land imms = 0 then 5
    else if 0b10000 land imms = 0 then 4
    else if 0b1000 land imms = 0 then 3
    else if 0b100 land imms = 0 then 2
    else if 0b10 land imms = 0 then 1
    else if 0b1 land imms = 0 then 0
    else -1 in

  if len < 1 then failwith "reserved value" else
  if not (regSize >= (1 lsl len)) then failwith "assert failed" else

  (* Determine S, R and S - R parameters *)
  let levels : int = (1 lsl len) - 1 in

  (* For logical immediates an all-ones value of S is reserved *)
  (* since it would generate a useless all-ones result (many times) *)
  if immediate && ((imms land levels) = levels) then failwith "reserved value" else

  let _S : int = imms land levels in
  let _R : int = immr land levels in

  let diff : int = _S - _R in

  let esize : int = 1 lsl len in

  let d : int = diff land levels in
  let welem : Big_int.big_int = Big_int.pred_big_int (Big_int.shift_left_big_int Big_int.unit_big_int (_S+1)) in
  let telem : Big_int.big_int = Big_int.pred_big_int (Big_int.shift_left_big_int Big_int.unit_big_int (d+1)) in

  let ror (n : Big_int.big_int) (k : int) =
    let k = k mod esize in
    if k = 0 then n else
    let d = Big_int.extract_big_int n 0 k in
    Big_int.or_big_int
      (Big_int.shift_left_big_int d (esize-k))
      (Big_int.shift_right_big_int n k) in

  let rec replicate (n : Big_int.big_int) k =
    if k = 1 then n else
    let d = replicate n (k-1) in
    Big_int.or_big_int n (Big_int.shift_left_big_int d esize) in

  let wmask : Big_int.big_int = replicate (ror welem _R) (regSize / esize) in
  let tmask : Big_int.big_int = replicate telem (regSize / esize) in
  (wmask, tmask)

(* encodeBitMasks: return Some (N,imms,immr) if exist such values that
                   decode to 'imm' for 'regSize' (32/64) bits datasize. Otherwise
                   return None. *)
(*** FIXME: ocaml int is 31/63bit, what do we use instead? ***)
let encodeBitMasks regSize (imm : Big_int.big_int) =
  if imm = Big_int.zero_big_int then None
  else if imm = Big_int.pred_big_int (Big_int.shift_left_big_int Big_int.unit_big_int regSize) then None
  else

  let ror (n : Big_int.big_int) (k : int) =
    let k = k mod regSize in
    if k = 0 then n else
    let d = Big_int.extract_big_int n 0 k in
    Big_int.or_big_int
      (Big_int.shift_left_big_int d (regSize-k))
      (Big_int.shift_right_big_int n k) in

  let rec next b n =
    if Big_int.extract_big_int n 0 1 = b then 0
    else 1 + next b (Big_int.shift_right_big_int n 1) in

  let pref0 = next Big_int.unit_big_int imm in (* the length of 0s prefix of imm *)
  let (e,ones,rotate) =
    if pref0 = 0 then
      let pref1 = next Big_int.zero_big_int imm in (* the length of 1s prefix of imm *)
      let run0 = next Big_int.unit_big_int (ror imm pref1) in (* the length of the first 0s run *)
      let run1 = next Big_int.zero_big_int (ror imm (pref1 + run0)) in  (* the length of the first non-prefix 1s run *)
      (run0+run1, run1, run1-pref1)
    else
      let run1 = next Big_int.zero_big_int (ror imm pref0) in (* the length of the first 1s run *)
      let run0 = next Big_int.unit_big_int (ror imm (pref0 + run1)) in  (* the length of the first non-prefix 0s run *)
      (run0+run1, run1, run0+run1-pref0) in

  let (_N,imms,immr) =
    if e = 64 then (1, ones-1, rotate)
    else (0, (lnot (e*2 - 1)) lor (ones-1), rotate) in

  match decodeBitMasks regSize _N imms immr true with
  | (wmask,_) when wmask = imm -> Some (_N,imms,immr)
  | _ -> None


