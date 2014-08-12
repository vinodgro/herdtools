/*********************************************************************/
/*                        Herd                                       */
/*                                                                   */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                   */
/* Jade Alglave, University College London, UK.                      */
/*                                                                   */
/*  Copyright 2013 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/


%{
open AST

let as_op op = function
  | Op (op0,es) when op0 = op -> es
  | e -> [e]

let do_op op e1 e2 =
  let es1 = as_op op e1
  and es2 = as_op op e2 in
  Op (op,es1@es2)

let pp () =
  let open Lexing in
  let start = Parsing.symbol_start_pos ()
  and fin = symbol_end () in
  let pos = start.pos_cnum in
  let len = fin - pos in
  {pos;len}


%}
%token EOF
%token <string> VAR
%token <string> STRING
%token <string> LATEX
%token LPAR RPAR LBRAC RBRAC
%token EMPTY EMPTY_SET UNDERSCORE
%token WITHCO WITHOUTCO WITHINIT WITHOUTINIT
/* Access direction */
%token MM  MR  MW WM WW WR RM RW RR INT EXT NOID
/* Plain/Atomic */
%token AA AP PA PP
%token SEMI UNION INTER COMMA DIFF
%token STAR PLUS OPT INV COMP NOT HAT TWO
%token LET REC SET RLN AND ACYCLIC IRREFLEXIVE TESTEMPTY EQUAL SHOW UNSHOW AS FUN IN
%token REQUIRES
%token ARROW
%type <AST.t> main
%start main

/* Precedences */
%right UNION
%right SEMI
%left DIFF
%right INTER
%nonassoc STAR PLUS OPT INV COMP NOT
%%

main:
| VAR options ins_list EOF { $2,$1,$3 }
| STRING options ins_list EOF { $2,$1,$3 }

options:
| WITHCO options { ModelOption.set_enumco true $2 }
| WITHOUTCO options { ModelOption.set_enumco false $2 }
| WITHINIT options { ModelOption.set_init true $2 }
| WITHOUTINIT options { ModelOption.set_init false $2 }
|    { ModelOption.default }

ins_list:
| { [] }
| ins ins_list { $1 :: $2 }

ins:
| LET pat_bind_list { Let $2 }
| LET REC bind_list { Rec $3 }
| test_type test exp optional_name { Test(pp (),$2,$3,$4,$1) }
| SHOW exp AS VAR { ShowAs ($2, $4) }
| SHOW var_list { Show $2 }
| UNSHOW var_list { UnShow $2 }
| LATEX { Latex $1 }

test_type:
|          { Provides }
| REQUIRES { Requires }

optional_name:
|        { None }
| AS VAR { Some $2 }

test:
| ACYCLIC { Acyclic }
| IRREFLEXIVE { Irreflexive }
| TESTEMPTY { TestEmpty }

var_list:
| VAR { [$1] }
| VAR commaopt var_list { $1 :: $3 }

commaopt:
| COMMA { () }
|       { () }
    
bind:
| VAR EQUAL exp { ($1,$3) }

bind_list:
| bind { [$1] }
| bind AND bind_list { $1 :: $3 }

pat_bind:
| bind { $1 }
| VAR LPAR formals RPAR EQUAL exp { ($1,Fun ($3,$6)) }

pat_bind_list:
| pat_bind { [$1] }
| pat_bind AND pat_bind_list { $1 :: $3 }


formals:
|          { [] }
| formalsN { $1 }

formalsN:
| VAR                { [$1] }
| VAR COMMA formalsN { $1 :: $3 }

exp:
| LET pat_bind_list IN exp { Bind ($2,$4) }
| LET REC bind_list IN exp { BindRec ($3,$5) }
| FUN LPAR formals RPAR ARROW exp { Fun ($3,$6) }
| base { $1 }

base:
| EMPTY { Konst (Empty RLN) }
| EMPTY_SET { Konst (Empty SET) }
| UNDERSCORE { Var "_" }
| select LPAR exp RPAR { Op1 ($1,$3) }
|  exp0 { $1 }
| base STAR base {Op (Cartesian, [$1; $3])}
| LBRAC exp RBRAC {Op1(Set_to_rln,$2)}
| base STAR { Op1(Star,$1) }
| base PLUS { Op1(Plus,$1) }
| base OPT { Op1(Opt,$1) }
| base HAT INV { Op1(Inv,$1) }
| base HAT TWO { Op1(Square,$1) }
| base SEMI base { do_op Seq $1 $3 }
| base UNION base { do_op (Union) $1 $3 }
| base DIFF base { do_op (Diff) $1 $3 }
| COMP base { Op1 (Comp RLN, $2) }
| NOT base { Op1 (Comp SET, $2) }
| base INTER base { do_op (Inter) $1 $3 }
| LPAR exp RPAR { $2 }

exp0:
| VAR                 { Var $1 }
| exp0 LPAR args RPAR { App ($1,$3) }


args:
| exp            { [ $1 ] }
| exp COMMA args { $1 :: $3 }

select:
| MM { Select (WriteRead,WriteRead) }
| MW { Select (WriteRead,Write) }
| MR { Select (WriteRead,Read) }
| WM { Select (Write,WriteRead) }
| WW { Select (Write,Write) }
| WR { Select (Write,Read) }
| RM { Select (Read,WriteRead) }
| RW { Select (Read,Write) }
| RR { Select (Read,Read) }
/* Atomic/Plain */
| AA { Select (Atomic,Atomic) }
| AP { Select (Atomic,Plain) }
| PA { Select (Plain,Atomic) }
| PP { Select (Plain,Plain) }
/* additional filters */
| EXT { Ext }
| INT { Int }
| NOID { NoId }
