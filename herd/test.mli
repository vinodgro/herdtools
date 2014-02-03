(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

type ('prog,'nice_prog,'start,'state,'constr, 'loc) t =
    {
     arch : Archs.t ; 
     name : Name.t ;
     info : MiscParser.info ;
     program : 'prog ;
     nice_prog : 'nice_prog ;
     start_points : 'start ;
     init_state : 'state ;
     cond : 'constr ;
     flocs : 'loc list ;
     scope_tree : MiscParser.scope_tree ;
     mem_map : MiscParser.mem_space_map ;
   }

val simple_name :
    ('prog,'nice_prog,'start,'state,'constr, 'loc) t -> string
val readable_name :
    ('prog,'nice_prog,'start,'state,'constr, 'loc) t -> string
val very_readable_name :
    ('prog,'nice_prog,'start,'state,'constr, 'loc) t -> string
val basename :
    ('prog,'nice_prog,'start,'state,'constr, 'loc) t -> string


module Make(A:Arch.S) : sig

  type result =
      (A.program,
       A.nice_prog,
       A.start_points,
       A.state,
       A.constr, 
       A.location
      ) t
       
  val build : Name.t -> A.pseudo MiscParser.t -> result

  val find_our_constraint : result -> A.constr

end
