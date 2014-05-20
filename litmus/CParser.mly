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

%token EOF COMMA STAR VOLATILE UNSIGNED SIGNED ATOMIC LONG DOUBLE BOOL
%token LPAREN RPAREN
%token <int> PROC
%token <string> BODY
%token <string> ATOMIC_NAME
%token <string> NAME

%start main
%type <(string CAst.t) list> main

%%

main:
| EOF { [] }
| BODY main { CAst.Global $1 :: $2 }
| PROC LPAREN params RPAREN BODY main
    { CAst.Test {MiscParser.proc = $1; params = $3; body = $5} :: $6 }

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

ty:
| atyp STAR { RunType.Ty $1 }
| atyp STAR STAR { RunType.Pointer $1 }

atyp:
| typ { $1 }
| ATOMIC typ { "_Atomic " ^ $2 }

typ:
| ATOMIC_NAME { $1 }
| ty_attr NAME { $1 ^ $2 }
| ty_attr LONG { $1 ^ "long" }
| ty_attr DOUBLE { $1 ^ "double" }
| ty_attr LONG LONG { $1 ^ "long long" }
| ty_attr LONG DOUBLE { $1 ^ "long double" }
| BOOL { "_Bool" }

ty_attr:
| { "" }
| UNSIGNED { "unsigned " }
| SIGNED { "signed " }
