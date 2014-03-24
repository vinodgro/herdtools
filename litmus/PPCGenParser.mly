/*********************************************************************/
/*                        Memevents                                  */
/*                                                                   */
/* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     */
/* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         */
/*                                                                   */
/*  Copyright 2010 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

%{
module PPCGen = PPCGenBase
open PPCGen
%}

%token EOF
%token <PPCGenBase.reg> ARCH_REG
%token <string> SYMB_REG
%token <int> NUM
%token <string> NAME
%token <int> PROC

%token SEMI COMMA PIPE COLON LPAR RPAR

%token <int> CRK
%token <int> CRBIT
%token PLUS TIMES

/* #include "generated/tokens.gen" */

%token B
%token BL
%token DCBF
%token EIEIO
%token ISYNC
%token LWARX
%token STWCX
%token SYNC

%token CRNAND
%token CRAND

%token BEQ
%token BGE
%token BGT
%token BLE
%token BLR
%token BLT
%token BNE
%token BNG
%token BNL
%token CMPW
%token CMPWI
%token LI
%token LWSYNC
%token MFLR
%token MR
%token MTLR
%token SUB
%token SUBDOT
%token SUBI

%token COMMENT
%token <string> STRING

%type <int list * (PPCGenBase.pseudo) list list * (ScopeTree.scope_tree option * MemSpaceMap.mem_space_map * LocationKindMap.lk_map) > main 
%start  main

%nonassoc SEMI
%%

main:
| semi_opt proc_list iol_list_opt EOF { $2,$3,(None,[],[]) }

semi_opt:
| { () }
| SEMI { () }

proc_list:
| PROC SEMI
    {[$1]}

| PROC PIPE proc_list  { $1::$3 }

iol_list_opt :
| { [] }
| iol_list { $1 }

iol_list :
|  instr_option_list SEMI
    {[$1]}
|  instr_option_list SEMI iol_list {$1::$3}

instr_option_list :
  | instr_option
      {[$1]}
  | instr_option PIPE instr_option_list 
      {$1::$3}

instr_option :
|            { Nop }
| NAME COLON instr_option { Label ($1,$3) }
| NAME LPAR reg_list RPAR
             { Macro ($1,$3) }
| instr      { Instruction $1}

reg_list :
| { [] }
| reg { [$1] }
| reg COMMA reg_list { $1 :: $3 }

instr:
  /* Generated fixed-point instructions */
  /* #include "generated/parser.gen" */

  | B NAME
    { Pb $2 }
  | BL NAME
    { Pbl $2 }
  | DCBF reg COMMA reg
    { Pdcbf ($2,$4) }
  | EIEIO
    { Peieio }
  | ISYNC
    { Pisync }
  | LWARX  reg COMMA reg COMMA reg
    { Plwarx ($2,$4,$6)}
  | STWCX reg COMMA reg COMMA reg
    { Pstwcx ($2,$4,$6) }
  | SYNC
    { Psync }
  | CRNAND crbit COMMA crbit COMMA crbit
    { Pcrnand($2, $4, $6) }
  | CRAND crbit COMMA crbit COMMA crbit
    { Pcrand($2, $4, $6) }

  /* Extended mnemonics */
  | BEQ NAME
    { Pbcc (Eq,$2) }
  | BGE NAME
    { Pbcc (Ge,$2) }
  | BGT NAME
    { Pbcc (Gt,$2) }
  | BLE NAME
    { Pbcc (Le,$2) }
  | BLT NAME
    { Pbcc (Lt,$2) }
  | BNE NAME
    { Pbcc (Ne,$2) }
  | BNG NAME
    { Pbcc (Le,$2) }
  | BNL NAME
    { Pbcc (Ge,$2) }
  | BLR
    { Pblr }
  | CMPW crindex COMMA reg COMMA reg
    { Pcmp ($2,0,$4,$6) }
  | CMPW reg COMMA reg
    { Pcmp (0,0,$2,$4) }
  | CMPWI crindex COMMA reg COMMA k
    { Pcmpi ($2,0,$4,$6) }
  | CMPWI reg COMMA k
    { Pcmpi (0,0,$2,$4) }
  | LI reg COMMA k
    { Paddi ($2, Ireg GPR0,$4) }
  | LWSYNC
    { Plwsync }
  | MFLR reg
    { Pmfspr ($2, 8)}
  | MR reg COMMA reg
    { Por (DontSetCR0,$2,$4,$4) }
  | MTLR reg
    { Pmtspr (8,$2) }
  | SUB reg COMMA reg COMMA reg
    { Psubf (DontSetSOOV,DontSetCR0,$2,$6,$4) }
  | SUBDOT reg COMMA reg COMMA reg
    { Psubf (DontSetSOOV,SetCR0,$2,$6,$4) }
  | SUBI reg COMMA reg COMMA k
    { Paddi ($2,$4, 0 - $6) }

  | COMMENT STRING
    { Pcomment $2 }

k:
| NUM  { $1 }

crindex:
| NUM  { $1 }
| CRK  { $1 }

crmask:
| NUM  { $1 }
| CRK  { 1 lsl (7-$1) } /* FXM field */

crbitnumber:
| NUM  { $1 }
| CRBIT { $1 }

crbit:
| k TIMES crindex PLUS crbitnumber {
    if ($1 <> 4) then failwith "crindex should be multiplied by 4.";
    $1 * $3 + $5
    }
| crbitnumber { $1 }

reg:
| SYMB_REG { Symbolic_reg $1 }
| ARCH_REG { $1 }
