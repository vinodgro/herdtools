/*Much copied from the PPC parser*/

%{
module CPP11 = CPP11Base
open Constant
open CPP11
%}

%token EOF
%token <CPP11Base.reg> ARCH_REG
%token <int> NUM
%token <string> NAME
%token <int> PROC
%token SEMI COMMA PIPE COLON LPAR RPAR EQ DOT 
%token <CPP11Base.mem_order> MEMORDER

/* Instruction tokens */

%token LD ST FENCE LOCK UNLOCK SCAS WCAS

%type <int list * (CPP11Base.pseudo) list list * (ScopeTree.scope_tree option * MemSpaceMap.mem_space_map option)> main 
%start  main

%nonassoc SEMI
%%

main:
| semi_opt proc_list iol_list EOF { $2,$3,(None,None) }
| semi_opt proc_list EOF          { $2,[],(None,None) }

semi_opt:
|      { () }
| SEMI { () }

proc_list:
| PROC SEMI           { [$1] }
| PROC PIPE proc_list { $1::$3 }

iol_list :
|  instr_option_list SEMI          { [$1] }
|  instr_option_list SEMI iol_list { $1::$3 }

instr_option_list :
| instr_option                        { [$1] }
| instr_option PIPE instr_option_list { $1::$3 }

instr_option :
|            { Nop }
| instr      { Instruction $1}

instr:
  | reg EQ loc DOT LD LPAR MEMORDER RPAR
    {Pload ($3,$1,$7)}
  | loc DOT ST LPAR store_op COMMA MEMORDER RPAR
    {Pstore ($1,$5,$7)}
  | loc EQ store_op
    {Pstore ($1,$3,NA)}
  | reg EQ loc
    {Pload ($3,$1,NA)}
  | FENCE LPAR MEMORDER RPAR
    {Pfence ($3)}
  | LOCK LPAR loc RPAR
    {Plock ($3)}
  | UNLOCK LPAR loc RPAR
    {Punlock ($3)}
  | WCAS LPAR loc COMMA loc COMMA store_op COMMA MEMORDER COMMA MEMORDER RPAR
    {Pcas ($3,$5,$7,$9,$11,false)}
  | SCAS LPAR loc COMMA loc COMMA store_op COMMA MEMORDER COMMA MEMORDER RPAR
    {Pcas ($3,$5,$7,$9,$11,true)}

store_op :
| NUM { Concrete $1 }

reg:
| ARCH_REG { $1 }

loc:
| NAME { Symbolic $1 }

