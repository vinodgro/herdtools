(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Values of the model interpreter *)

module type I = sig
  type event_rel
  type event_set
end

module type S = sig
  type event_rel
  type event_set
    type v =
    | Rel of event_rel
    | Set of event_set
    | Clo of closure
    | Proc of procedure
  and env = v Lazy.t StringMap.t
  and closure =
    { clo_args : AST.var list ;
      clo_env : env ;
      clo_body : AST.exp; }
  and procedure = {
      proc_args : AST.var list;
      proc_env : env;
      proc_body : AST.ins list; }
end

module Make(I:I) = struct
  type event_rel = I.event_rel
  type event_set = I.event_set

  type v =
    | Rel of event_rel
    | Set of event_set
    | Clo of closure
    | Proc of procedure
  and env = v Lazy.t StringMap.t
  and closure =
    { clo_args : AST.var list ;
      clo_env : env ;
      clo_body : AST.exp; }
  and procedure = {
      proc_args : AST.var list;
      proc_env : env;
      proc_body : AST.ins list; }

end

