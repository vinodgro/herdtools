%{
open Constant
open CBase
%}

%token EOF
%token LK
%token <CBase.reg> ARCH_REG
%token <int> NUM
%token <string> NAME
%token <string> ATOMIC_NAME
%token <int> PROC
%token SEMI COMMA PIPE COLON LPAR RPAR EQ DOT LBRACE RBRACE STAR
%token WHILE IF ELSE 
%token VOLATILE UNSIGNED SIGNED ATOMIC LONG DOUBLE BOOL INT
%token <CBase.mem_order> MEMORDER
%token <CBase.location_kind> LOCATIONKIND

/* Instruction tokens */

%token LD ST FENCE LOCK UNLOCK SCAS WCAS

%type <LocationKindMap.lk_map> lk_map
%type <(int * CBase.pseudo list) list * MiscParser.gpu_data option> main 
%type <(CBase.pseudo list) CAst.test list> procs
%start  main

%%

main: 
| procs lk_map EOF 
  { let proc_list, param_map = 
    List.fold_right (fun p (proc_list, param_map) -> 
        let proc_list = (p.CAst.proc,p.CAst.body) :: proc_list in
        let param_map = (p.CAst.proc,p.CAst.params) :: param_map in
        (proc_list, param_map)) $1 ([],[]) in
    let additional = { 
      MiscParser.empty_gpu with 
      MiscParser.lk_map = $2; 
      MiscParser.param_map = param_map} in
     (proc_list, Some additional) }

procs:
|   { [] }
| PROC LPAR params RPAR LBRACE instr_list RBRACE procs
    { {CAst.proc = $1; CAst.params = $3; CAst.body = $6} :: $8 } 

instr_list:
|            { [] }
| instr instr_list 
             { $1 :: $2 }

instr:
| basic_instr SEMI     
             { Instruction $1} 
| WHILE LPAR basic_instr RPAR LBRACE instr_list RBRACE 
             { Loop ($3,$6) }
| IF LPAR basic_instr RPAR LBRACE instr_list RBRACE ELSE LBRACE instr_list RBRACE
             { Choice ($3,$6,$10) }

basic_instr:
  | store_op
    {Pexpr_const $1}
  | loc
    {Pexpr_const $1}
  | reg
    {Pexpr_reg $1}
  | optional_type reg EQ LD LPAR loc COMMA MEMORDER RPAR
    {Pload ($6,$2,$8)}
  | ST LPAR loc COMMA store_op COMMA MEMORDER RPAR
    {Pstore ($3,$5,$7)}
  | STAR loc EQ store_op
    {Pstore ($2,$4,NA)}
  | optional_type reg EQ STAR loc
    {Pload ($5,$2,NA)}
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

optional_type:
|      { () }
| atyp { () }

store_op :
| NUM { Concrete $1 }

reg:
| ARCH_REG { $1 }

loc:
| NAME { Symbolic $1 }

one_or_more_params:
| param { [$1] }
| param COMMA one_or_more_params { $1 :: $3 }

params:
|                    { [] }
| one_or_more_params { $1 }

param:
| ty NAME
    { {CAst.param_ty = $1; volatile = false; param_name = $2} }
| VOLATILE ty NAME
    { {CAst.param_ty = $2; volatile = true; param_name = $3} }

ty:
| atyp STAR { RunType.Ty $1 }
/*| atyp STAR STAR { RunType.Pointer $1 }*/

atyp:
| typ { $1 }
| ATOMIC typ { "_Atomic " ^ $2 }

typ:
| ATOMIC_NAME { $1 }
| ty_attr INT { $1 ^ "int" }
| ty_attr LONG { $1 ^ "long" }
| ty_attr DOUBLE { $1 ^ "double" }
| ty_attr LONG LONG { $1 ^ "long long" }
| ty_attr LONG DOUBLE { $1 ^ "long double" }
| BOOL { "_Bool" }

ty_attr:
| { "" }
| UNSIGNED { "unsigned " }
| SIGNED { "signed " }

lk_map:
| LK lk_list { $2 }
|         { [] }

lk_list:
| lk { [$1] }
| lk COMMA lk_list { $1 :: $3 }

lk:
| NAME COLON LOCATIONKIND { ($1,$3) }

