%{
open Constant
open CPP11Base
%}

%token EOF
%token LK
%token <CPP11Base.reg> ARCH_REG
%token <int> CONSTANT
%token <string> IDENTIFIER
%token <string> ATOMIC_TYPE
%token <int> PROC
%token SEMI COMMA PIPE COLON LPAR RPAR EQ EQ_OP DOT LBRACE RBRACE STAR
%token WHILE IF ELSE

%nonassoc LOWER_THAN_ELSE /* This fixes the dangling-else problem */
%nonassoc ELSE
 
%token UNSIGNED SIGNED ATOMIC LONG DOUBLE BOOL INT VOID FLOAT CHAR SHORT
%token MUTEX 
%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CONST VOLATILE 
%token <CPP11Base.mem_order> MEMORDER

/* Instruction tokens */

%token LD LD_EXPLICIT ST ST_EXPLICIT FENCE LOCK UNLOCK SCAS WCAS

%type <(int * CPP11Base.pseudo list) list * MiscParser.gpu_data option> main 
%type <(CPP11Base.pseudo list) CPP11Ast.test list> translation_unit
%start main

%%

main: 
| translation_unit EOF 
  { let proc_list, param_map = 
      List.fold_right (fun p (proc_list, param_map) -> 
        let proc_list = (p.CPP11Ast.proc,p.CPP11Ast.body) :: proc_list in
        let param_map = p.CPP11Ast.params @ param_map in
        (proc_list, param_map)) $1 ([], [])  
    in
    let additional = 
      { MiscParser.empty_gpu with 
        MiscParser.param_map = param_map; } 
    in
    (proc_list, Some additional) }

primary_expression:
| ARCH_REG 
  { Eregister $1 }
| CONSTANT 
  { Econstant (Concrete $1) }
| LPAR expression RPAR 
  { Eparen $2 }

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
| ARCH_REG assignment_operator assignment_expression
  { Eassign ($1, $3) }
| pointer loc assignment_operator assignment_expression
  { Estore ($2, $4, CPP11Base.NA) }

assignment_operator:
| EQ { () }

expression:
| assignment_expression { $1 }
| expression COMMA assignment_expression { Ecomma ($1,$3) }

declaration:
| declaration_specifiers SEMI
  { Pblock [] }
| declaration_specifiers init_declarator_list SEMI
  { Pblock $2 }

declaration_specifiers:
| storage_class_specifier 
  { $1 }
| storage_class_specifier declaration_specifiers
  { $1 ^ " " ^ $2 }
| type_specifier 
  { $1 }
| type_specifier declaration_specifiers
  { $1 ^ " " ^ $2 }
| type_qualifier
  { $1 }
| type_qualifier declaration_specifiers
  { $1 ^ " " ^ $2 }

init_declarator_list:
| init_declarator
  { [$1] }
| init_declarator_list COMMA init_declarator
  { $1 @ [$3] }

init_declarator:
| ARCH_REG
  { Pblock [] }
| ARCH_REG EQ initialiser 
  { Pexpr (Eassign ($1, $3)) }

storage_class_specifier:
| TYPEDEF { "typedef" }
| EXTERN { "extern" }
| STATIC { "static" }
| AUTO { "auto" }
| REGISTER { "register" }

type_specifier:
| VOID { "void" }
| CHAR { "char" }
| SHORT { "short" }
| INT { "int" }
| LONG { "long" }
| FLOAT { "float" }
| DOUBLE { "double" }
| SIGNED { "signed" }
| UNSIGNED { "unsigned" }
| MUTEX { "mutex" }
| ATOMIC { "_Atomic" }
| ATOMIC_TYPE { $1 } 

type_qualifier:
| CONST { "const" }
| VOLATILE { "volatile" }

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
  { let ty = $1 in
    let (ptr, identifier) = $2 in
    let vol = 
      try 
        let _ = Str.search_forward (Str.regexp "volatile") ty 0 in 
        true 
      with Not_found -> false
    in
    let full_ty = if ptr then RunType.Pointer ty else RunType.Ty ty in
    { CPP11Ast.param_ty = full_ty; 
      CPP11Ast.volatile = vol; 
      CPP11Ast.param_name = identifier } }

initialiser:
| assignment_expression 
  { $1 }

statement:
| declaration /* (* Added to allow mid-block declarations *) */
  { $1 }
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
| declaration_specifiers PROC LPAR parameter_type_list RPAR compound_statement
  { { CPP11Ast.proc = $2; 
      CPP11Ast.params = $4; 
      CPP11Ast.body = List.map (fun ins -> Instruction ins) $6 } }
| PROC LPAR parameter_type_list RPAR compound_statement
  { { CPP11Ast.proc = $1; 
      CPP11Ast.params = $3; 
      CPP11Ast.body = List.map (fun ins -> Instruction ins) $5 } }

loc:
| IDENTIFIER { Symbolic $1 }
