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
%token LPAR RPAR
%token EMPTY
/* Access direction */
%token MM  MR  MW WM WW WR RM RW RR
/* Plain/Atomic */
%token AA AP PA PP
%token SEMI UNION INTER COMMA DIFF
%token STAR PLUS OPT
%token LET REC AND ACYCLIC IRREFLEXIVE TESTEMPTY EQUAL SHOW UNSHOW AS
%type <AST.t> main
%start main

/* Precedences */
%right UNION
%right SEMI
%left DIFF
%right INTER
%nonassoc STAR PLUS OPT
%%

main:
| VAR ins_list EOF { $1,$2 }
| STRING ins_list EOF { $1,$2 }

ins_list:
| { [] }
| ins ins_list { $1 :: $2 }

ins:
| LET bind_list { Let $2 }
| LET REC bind_list { Rec $3 }
| test exp AS VAR { Test(pp (),$1,$2,Some $4) }
| test exp  { Test(pp (),$1,$2,None) }
| SHOW exp AS VAR { ShowAs ($2, $4) }
| SHOW var_list { Show $2 }
| UNSHOW var_list { UnShow $2 }

test:
| ACYCLIC { Acyclic }
| IRREFLEXIVE { Irreflexive }
| TESTEMPTY { TestEmpty }

var_list:
|  { [] }
| VAR commaopt var_list { $1 :: $3 }

commaopt:
| COMMA { () }
|       { () }
    
bind_list:
| bind { [$1] }
| bind AND bind_list { $1 :: $3 }

bind:
| VAR EQUAL exp { ($1,$3) }

exp:
| EMPTY { Konst Empty }
| select LPAR exp RPAR { Op1 ($1,$3) }
| VAR { Var $1 }
| exp STAR { Op1(Star,$1) }
| exp PLUS { Op1(Plus,$1) }
| exp OPT { Op1(Opt,$1) }
| exp SEMI exp { do_op Seq $1 $3 }
| exp UNION exp { do_op Union $1 $3 }
| exp DIFF exp { do_op Diff $1 $3 }
| exp INTER exp { do_op Inter $1 $3 }
| LPAR exp RPAR { $2 }

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

