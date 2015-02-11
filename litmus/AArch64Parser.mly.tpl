/**************************************************************************/
/*                                  DIY                                   */
/*                                                                        */
/* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.          */
/* Shaked Flur, Susmit Sarkar, Peter Sewell, University of Cambridge, UK. */
/*                                                                        */
/*  Copyright 2015 Institut National de Recherche en Informatique et en   */
/*  Automatique and the authors. All rights reserved.                     */
/*  This file is distributed  under the terms of the Lesser GNU General   */
/*  Public License.                                                       */
/**************************************************************************/

%{
module AArch64 = AArch64Base
open AArch64

let issp r = match r with X SP | W SP -> true | _ -> false
let iszr r = match r with X ZR | W ZR -> true | _ -> false
let isregzr r = not (issp r)
let isregsp r = not (iszr r)

let error_registers = failwith
let error_arg = failwith
let error_not_instruction txt = failwith "%s is not an instruction" txt

%}

%token EOF

%token <AArch64Base.inst_reg> ARCH_XREG
%token <AArch64Base.inst_reg> ARCH_WREG

%token <AArch64Base.inst_reg> SYMB_XREG
%token <AArch64Base.inst_reg> SYMB_WREG
%token <int> NUM
%token <Big_int.big_int> BIG_NUM
%token <string> NAME
%token <int> PROC

%token SEMI COMMA PIPE COLON LBRK RBRK EXCL

/* #include "./src_aarch64_hgen/tokens.hgen" */

%type <int list * (AArch64Base.pseudo) list list * MiscParser.gpu_data option> main
%start  main

%nonassoc SEMI
%%

main:
| semi_opt proc_list iol_list EOF { $2,$3,None }

semi_opt:
| { () }
| SEMI { () }

proc_list:
| PROC SEMI {[$1]}
| PROC PIPE proc_list { $1::$3 }

iol_list :
| instr_option_list SEMI {[$1]}
| instr_option_list SEMI iol_list {$1::$3}

instr_option_list :
| instr_option {[$1]}
| instr_option PIPE instr_option_list {$1::$3}

instr_option :
|            { Nop }
| NAME COLON instr_option { Label ($1,$3) }
| instr      { Instruction $1 }

instr:
/* Generated fixed-point instructions */
/* #include "./src_aarch64_hgen/parser.hgen" */
/* instructions */
| B NAME
    { `AArch64BranchImmediate_label ($1._branch_type,$2) }
| BCOND NAME
    { `AArch64BranchConditional_label ($2,$1.condition) }

wreg:
| ARCH_WREG { $1 }
| SYMB_WREG { $1 }

xreg:
| ARCH_XREG { $1 }
| SYMB_XREG { $1 }

imm:
| NUM { $1 }

big_imm:
| imm { Big_int.big_int_of_int $1 }
| BIG_NUM { $1 }

