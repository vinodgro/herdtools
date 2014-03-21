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
%token <OpenCLBase.gpu_memory_space> MEMREGION

/* Instruction tokens */

%token LD ST FENCE

%type <int list * (OpenCLBase.pseudo) list list * (ScopeTree.scope_tree option * MemSpaceMap.mem_space_map)> main 
%start  main

%nonassoc SEMI

%token SCOPETREE DEVICE KERNEL CTA WARP THREAD COMMA PTX_REG_DEC 

%type <ScopeTree.scope_tree option * MemSpaceMap.mem_space_map> scopes_and_memory_map

%%

main:
| semi_opt proc_list iol_list scopes_and_memory_map EOF { $2,$3,$4 }

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
  | FENCE LPAR MEMREGION COMMA MEMORDER COMMA MEMSCOPE RPAR
    {Pfence ($3,$5,$7)}

store_op :
| NUM { Concrete $1 }

reg:
| ARCH_REG { $1 }

loc:
| NAME { Symbolic $1 }

/* 
   Parsing a simple S expression that needs to have a certain value.
*/

scopes_and_memory_map : 
| SCOPETREE scope_tree memory_map 
   { Some $2, $3 }

scope_tree :
|  device_list {$1}

device_list :
|  device { [$1] }
|  device device_list { [$1]@$2 }

device:
|  LPAR DEVICE kernel_list RPAR {$3}

kernel_list :
|  kernel { [$1] }
|  kernel kernel_list { [$1]@$2 }
|  cta_list {List.map (fun x -> [x]) $1}

kernel:
|  LPAR KERNEL cta_list RPAR {$3}

cta_list:
|  cta { [$1] }
|  cta cta_list { [$1]@$2 }
|  warp_list {List.map (fun x -> [x]) $1}

cta:
| LPAR CTA warp_list RPAR {$3}

warp_list:
|  warp { [$1] }
|  warp warp_list { [$1]@$2 }
|  thread_list {List.map (fun x -> [x]) $1}

warp:
| LPAR WARP thread_list RPAR { $3 }

thread_list:
|  thread { [$1] }
|  thread thread_list { [$1]@$2 }

thread:
| PROC {$1}

memory_map:
| memory_map_list { $1 }
|                 { [] }

memory_map_list:
| memory_map_atom { [$1] }
| memory_map_atom COMMA memory_map_list { [$1]@$3 }

memory_map_atom:
| NAME COLON MEMREGION { ($1,$3) }
| NAME COLON MEMREGION { ($1,$3) }
