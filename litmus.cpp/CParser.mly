/*********************************************************************/
/*                          Litmus                                   */
/*                                                                   */
/*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     */
/*                                                                   */
/*  Copyright 2010 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

%token COMMA STAR INT
%token LPAREN RPAREN
%token <int> PROC
%token <string> BODY
%token <string> NAME

%start main
%type <CAst.t list> main

%%

main:
| PROC LPAREN params RPAREN BODY main
    { {CAst.proc = $1; params = $3; body = $5} :: $6 }

params:
| { [] }
| ty NAME { [{CAst.param_ty = $1; param_name = $2}] }
| ty NAME COMMA params { {CAst.param_ty = $1; param_name = $2} :: $4 }

ty:
| INT STAR NAME { CAst.Int_ptr }
