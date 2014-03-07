/*Much copied from the C++11 parser*/

%{
module OpenCL = OpenCLBase
open Constant
open OpenCL
%}

%token EOF
%token <OpenCLBase.reg> ARCH_REG
%token <int> NUM
%token <string> NAME
%token <int> PROC
%token SEMI COMMA PIPE COLON LPAR RPAR EQ DOT 
%token <OpenCLBase.mem_order> MEMORDER
%token <OpenCLBase.mem_scope> MEMSCOPE

/* Instruction tokens */

%token LD ST FENCE

%type <int list * (OpenCLBase.pseudo) list list> main 
%start  main

%nonassoc SEMI
%%

main:
| semi_opt proc_list iol_list EOF { $2,$3 }
| semi_opt proc_list EOF          { $2,[] }

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
  | reg EQ loc DOT LD LPAR MEMORDER COMMA MEMSCOPE RPAR
    {Pload ($3,$1,$7,$9)}
  | loc DOT ST LPAR store_op COMMA MEMORDER COMMA MEMSCOPE RPAR
    {Pstore ($1,$5,$7,$9)}
  | loc EQ store_op
    {Pstore ($1,$3,NA,S_workitem)}
  | reg EQ loc
    {Pload ($3,$1,NA,S_workitem)}
  | FENCE LPAR MEMORDER COMMA MEMSCOPE RPAR
    {Pfence ($3,$5)}

store_op :
| NUM { Concrete $1 }

reg:
| ARCH_REG { $1 }

loc:
| NAME { Symbolic $1 }

