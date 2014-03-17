/*********************************************************************/
/*                        Memevents                                  */
/*                                                                   */
/* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     */
/* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         */
/*                                                                   */
/*  Copyright 2010 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

%{
open ScopeTree
%}

%token EOF
%token <string> NAME
%token <int> PROC
%token LPAR RPAR COLON
%token SCOPETREE GLOBAL SHARED DEVICE KERNEL CTA WARP THREAD COMMA PTX_REG_DEC 

%type <ScopeTree.scope_tree * ScopeTree.mem_space_map> scopes_and_memory_map
%start scopes_and_memory_map
%%


scopes_and_memory_map : 
| SCOPETREE scope_tree memory_map {Scope_tree($2), $3}

/* 
   Parsing a simple S expression that needs to have a certain value.
*/

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
| memory_map_list { Mem_space_map($1) }
|                 {ScopeTree.No_mem_space_map}

memory_map_list:
| memory_map_atom { [$1] }
| memory_map_atom COMMA memory_map_list { [$1]@$3 }

memory_map_atom:
| NAME COLON GLOBAL { ($1,ScopeTree.Global) }
| NAME COLON SHARED { ($1,ScopeTree.Shared) }
