%{
open Constant
open CPP11Base
%}

%token EOF
%token LK
%token <CPP11Base.reg> ARCH_REG
%token <int> CONSTANT
%token <string> IDENTIFIER
%token <string> ATOMIC_NAME
%token <int> PROC
%token SEMI COMMA PIPE COLON LPAR RPAR EQ EQ_OP DOT LBRACE RBRACE STAR
%token WHILE IF ELSE

%nonassoc LOWER_THAN_ELSE /* This fixes the dangling-else problem */
%nonassoc ELSE
 
%token UNSIGNED SIGNED ATOMIC LONG DOUBLE BOOL INT VOID FLOAT CHAR SHORT
%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CONST VOLATILE 
%token <CPP11Base.mem_order> MEMORDER
%token <CPP11Base.location_kind> LOCATIONKIND

/* Instruction tokens */

%token LD LD_EXPLICIT ST ST_EXPLICIT FENCE LOCK UNLOCK SCAS WCAS

%type <LocationKindMap.lk_map> lk_map
%type <(int * CPP11Base.pseudo list) list * MiscParser.gpu_data option> main 
%type <(CPP11Base.pseudo list) CAst.test list> translation_unit
%start  main

%%

main: 
| translation_unit lk_map EOF 
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

primary_expression:
| ARCH_REG { Eregister $1 }
| CONSTANT { Econstant (Concrete $1) }
| LPAR expression RPAR { $2 }

postfix_expression:
| primary_expression 
  { $1 }
| ST LPAR loc COMMA assignment_expression RPAR
  { Estore ($3, $5, CPP11Base.SC) }
| ST_EXPLICIT LPAR loc COMMA assignment_expression COMMA MEMORDER RPAR
  { Estore ($3, $5, $7) }
| LD LPAR loc RPAR
  { Eload ($3, CPP11Base.SC) }
| LD_EXPLICIT LPAR loc COMMA MEMORDER RPAR
  { Eload ($3, $5) }
| FENCE LPAR MEMORDER RPAR
  { Efence ($3) }
| LOCK LPAR loc RPAR
  { Elock ($3) }
| UNLOCK LPAR loc RPAR
  { Eunlock ($3) }
| WCAS LPAR loc COMMA loc COMMA assignment_expression COMMA MEMORDER COMMA MEMORDER RPAR
  { Ecas ($3,$5,$7,$9,$11,false) }
| SCAS LPAR loc COMMA loc COMMA assignment_expression COMMA MEMORDER COMMA MEMORDER RPAR
  { Ecas ($3,$5,$7,$9,$11,true) }

unary_expression:
| postfix_expression 
  { $1 }
| pointer loc
  { Eload ($2, CPP11Base.NA) }

cast_expression:
| unary_expression { $1 }

multiplicative_expression:
| cast_expression { $1 }

additive_expression:
| multiplicative_expression { $1 }

shift_expression:
| additive_expression { $1 }

relational_expression:
| shift_expression { $1 }

equality_expression:
| relational_expression 
  { $1 }
| equality_expression EQ_OP relational_expression 
  { Eeq ($1,$3) }

and_expression:
| equality_expression { $1 }

exclusive_or_expression:
| and_expression { $1 }

inclusive_or_expression: 
| exclusive_or_expression { $1 }

logical_and_expression: 
| inclusive_or_expression { $1 }

logical_or_expression:
| logical_and_expression { $1 }

conditional_expression:
| logical_or_expression { $1 }

assignment_expression:
| conditional_expression 
  { $1 }
| reg assignment_operator assignment_expression
  { Eassign ($1, $3) }
| pointer loc assignment_operator assignment_expression
  { Estore ($2, $4, CPP11Base.NA) }

assignment_operator:
| EQ { () }

expression:
| assignment_expression { $1 }
/* TODO:
| expression COMMA assignment_expression
*/

declaration:
| declaration_specifiers SEMI
  { Pblock [] }
| declaration_specifiers init_declarator_list SEMI
  { Pblock $2 }

declaration_specifiers:
| storage_class_specifier 
  { $1 }
| storage_class_specifier declaration_specifiers
  { let (ty1, vol1) = $1 in
    let (ty2, vol2) = $2 in
    (ty1 ^ " " ^ ty2, vol1 || vol2) }
| type_specifier 
  { $1 }
| type_specifier declaration_specifiers
  { let (ty1, vol1) = $1 in
    let (ty2, vol2) = $2 in
    (ty1 ^ " " ^ ty2, vol1 || vol2) }
| type_qualifier
  { $1 }
| type_qualifier declaration_specifiers
  { let (ty1, vol1) = $1 in
    let (ty2, vol2) = $2 in
    (ty1 ^ " " ^ ty2, vol1 || vol2) }

init_declarator_list:
| init_declarator
  { [$1] }
| init_declarator_list COMMA init_declarator
  { $1 @ [$3] }

init_declarator:
| reg
  { Pblock [] }
| reg EQ initialiser 
  { Pexpr (Eassign ($1, $3)) }

storage_class_specifier:
| TYPEDEF { ("typedef", false) }
| EXTERN { ("extern", false) }
| STATIC { ("static", false) }
| AUTO { ("auto", false) }
| REGISTER { ("register", false) }

type_specifier:
| VOID { ("void", false) }
| CHAR { ("char", false) }
| SHORT { ("short", false) }
| INT { ("int", false) }
| LONG { ("long", false) }
| FLOAT { ("float", false) }
| DOUBLE { ("double", false) }
| SIGNED { ("signed", false) }
| UNSIGNED { ("unsigned", false) }
| ATOMIC_NAME { ($1, false) } 

type_qualifier:
| CONST { ("const", false) }
| VOLATILE { ("volatile", true) }

declarator:
| pointer IDENTIFIER { (true, $2) }
| IDENTIFIER { (false, $1) }

pointer:
| STAR { () }

parameter_type_list:
| parameter_list { $1 }

parameter_list:
| parameter_declaration
  { [$1] }
| parameter_list COMMA parameter_declaration
  { $1 @ [$3] }

parameter_declaration:
| declaration_specifiers declarator
  { let (ty, vol) = $1 in
    let (ptr, identifier) = $2 in
    let ty = if ptr then RunType.Pointer ty else RunType.Ty ty in
    { CAst.param_ty = ty; 
      CAst.volatile = vol; 
      CAst.param_name = identifier } }

initialiser:
| assignment_expression 
  { $1 }

statement:
| compound_statement
  { Pblock $1 }
| expression_statement
  { $1 }
| selection_statement
  { $1 }
| iteration_statement
  { $1 }

compound_statement:
| LBRACE RBRACE
  { [] }
| LBRACE statement_list RBRACE
  { $2 }
| LBRACE declaration_list RBRACE
  { $2 }
| LBRACE declaration_list statement_list RBRACE
  { $2 @ $3 }

declaration_list:
| declaration
  { [$1] }
| declaration_list declaration
  { $1 @ [$2] }

statement_list:
| statement
  { [$1] }
| statement_list statement
  { $1 @ [$2] }

expression_statement:
| SEMI
  { Pblock [] }
| expression SEMI
  { Pexpr $1 }

selection_statement:
| IF LPAR expression RPAR statement %prec LOWER_THAN_ELSE
  { Pif ($3, $5, Pblock []) }
| IF LPAR expression RPAR statement ELSE statement
  { Pif ($3, $5, $7) }

iteration_statement:
| WHILE LPAR expression RPAR statement
  { Pwhile($3,$5) }

translation_unit:
| external_declaration
  { [$1] }
| translation_unit external_declaration 
  { $1 @ [$2] }

external_declaration:
| function_definition { $1 }

function_definition:
| declaration_specifiers PROC LPAR parameter_type_list RPAR declaration_list compound_statement
  { { CAst.proc = $2; 
      CAst.params = $4; 
      CAst.body = List.map (fun ins -> Instruction ins) ($6 @ $7) } }
| declaration_specifiers PROC LPAR parameter_type_list RPAR compound_statement
  { { CAst.proc = $2; 
      CAst.params = $4; 
      CAst.body = List.map (fun ins -> Instruction ins) $6 } }
| PROC LPAR parameter_type_list RPAR declaration_list compound_statement
  { { CAst.proc = $1; 
      CAst.params = $3; 
      CAst.body = List.map (fun ins -> Instruction ins) ($5 @ $6) } }
| PROC LPAR parameter_type_list RPAR compound_statement
  { { CAst.proc = $1; 
      CAst.params = $3; 
      CAst.body = List.map (fun ins -> Instruction ins) $5 } }


reg:
| ARCH_REG { $1 }

loc:
| IDENTIFIER { Symbolic $1 }

lk_map:
| LK lk_list { $2 }
|         { [] }

lk_list:
| lk { [$1] }
| lk COMMA lk_list { $1 :: $3 }

lk:
| IDENTIFIER COLON LOCATIONKIND { ($1,$3) }

