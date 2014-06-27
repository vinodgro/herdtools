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

%{
open CType
%}
%token EOF COMMA STAR VOLATILE UNSIGNED SIGNED ATOMIC LONG DOUBLE BOOL
%token LPAREN RPAREN
%token <int> PROC
%token <string> BODY
%token <string> ATOMIC_NAME
%token <string> NAME

%start main
%type <string CAst.t list> main

%%

main:
| EOF { [] }
| BODY main { CAst.Global $1 :: $2 }
| PROC LPAREN params RPAREN BODY main
    { CAst.Test {CAst.proc = $1; params = $3; body = $5} :: $6 }

/*
params:
| { [] }
| ty NAME
    { [{CAst.param_ty = $1; volatile = false; param_name = $2}] }
| VOLATILE ty NAME
    { [{CAst.param_ty = $2; volatile = true; param_name = $3}] }
| ty NAME COMMA params
    { {CAst.param_ty = $1; volatile = false; param_name = $2} :: $4 }
| VOLATILE ty NAME COMMA params
    { {CAst.param_ty = $2; volatile = true; param_name = $3} :: $5 }
*/
params:
| { [] }
| bind { [ $1 ] }
| bind COMMA params { $1 :: $3 }
 
bind:
| toptyp NAME { {CAst.param_ty = $1; param_name = $2} }

toptyp:
| typ STAR { $1 }

typ:
| typ STAR { Pointer $1 } 
| typ VOLATILE { Volatile $1 } 
| ATOMIC base { Atomic $2 }
| VOLATILE base0 { Volatile $2 }
| base { $1 }

base0:
| ATOMIC_NAME { Atomic (Base $1) }
| ty_attr NAME { Base ($1 ^ $2) }
| ty_attr LONG { Base ($1 ^ "long") }
| ty_attr DOUBLE { Base ($1 ^ "double") }
| ty_attr LONG LONG { Base ($1 ^ "long long") }
| ty_attr LONG DOUBLE { Base ($1 ^ "long double") }
| BOOL { Base ("_Bool") }

base:
| base0 { $1 }
| LPAREN typ RPAREN { $2 }

ty_attr:
| { "" }
| UNSIGNED { "unsigned " }
| SIGNED { "signed " }
