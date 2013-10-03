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

type ('prog,'nice_prog,'start,'state,'constr,'loc) t =
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
   }

(* Name and nothing else *)
let simple_name test = test.name.Name.name

(* human-readable test name/filename combination *)
let readable_name test =  test.name.Name.name

(* and just the first part of that, for use in latex index *)
let very_readable_name test =  test.name.Name.name

(* Name from filename *)
let basename test = Filename.chop_extension (Filename.basename test.name.Name.file)

module Make(A:Arch.S) =
  struct

    type result =
        (A.program, A.nice_prog, A.start_points,
         A.state, A.constr, A.location) t

(* Symb register allocation is external, since litmus needs it *)
   module Alloc = SymbReg.Make(A)
(* Code loader is external, since litmus tests need it too *)
    module Load = Loader.Make(A) 

    let build name t =
      let t = Alloc.allocate_regs t in
      let
          {MiscParser.init = init ;
           info = info ;
           prog = nice_prog ;
           condition = final ; 
           locations = locs ;
	 } = t in

      let prog,starts = Load.load nice_prog in
      {
       arch = A.arch ;
       name = name ;
       info = info ;
       program = prog ;
       nice_prog = nice_prog ;
       start_points = starts ;
       init_state = A.build_state init ;
       cond = final ;
       flocs = List.map fst locs ;
     }


    let find_our_constraint test = test.cond 

end
