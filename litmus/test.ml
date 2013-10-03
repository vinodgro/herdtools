(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type S = sig
  module A : Arch.S
  module C : Constr.S with module A = A

  type src =
    ((A.location * Constant.v) list, (int * A.pseudo list) list,
          C.constr, A.location)
         MiscParser.result

  type 'a type_env = ('a * RunType.t) list
  type t =
    { init : A.state ;
      info : MiscParser.info ;
      code : (int * (A.Out.t * A.reg type_env)) list ;
      condition : C.constr ;
      globals : string type_env ;
      flocs : A.location list ;
      src : src ; }

  val find_our_constraint : t -> C.constr
  val get_nprocs : t -> int
end

    

module Make(A:Arch.S) : S with module A = A =
struct
  module A  = A
  module C = Constr.Make(A)

  type 'a type_env = ('a * RunType.t) list
  type src =
    ((A.location * Constant.v) list, (int * A.pseudo list) list,
          C.constr, A.location)
         MiscParser.result
  type t =
    { init : A.state ;
      info : MiscParser.info ;
      code : (int * (A.Out.t * A.reg type_env)) list ;
      condition : C.constr ;
      globals : string type_env ;
      flocs : A.location list ;
      src : src ; }

  let find_our_constraint test = test.condition

  let get_nprocs t = List.length t.code
end
