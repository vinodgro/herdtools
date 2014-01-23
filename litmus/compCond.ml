(*********************************************************************)
(*                        Diy                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

module type X = sig
  type t
  val compare : t -> t -> int
  val dump : t -> string
end

module type I = sig
  module C : Constr.S
  module V : X with type t = Constant.v
  module Loc : X with type  t = C.A.location
end

module Make (O:Indent.S) (I:CompCondUtils.I) :
    sig
      val fundef :
          (I.Loc.t -> string) -> (* For types *)
            (I.Loc.t,I.V.t) ConstrGen.cond -> unit 
      val funcall :
          I.C.constr ->
            (I.Loc.t -> string) -> (string -> string) -> string
    end = struct
      open ConstrGen

      module S = Switch.Make(O)(I)

      let dump  =
        let rec dump_prop p = match p with
        | Atom (LV (loc,v)) ->          
            O.fprintf "%s == %s" (I.Loc.dump loc) (I.V.dump v)
        | Atom (LL (loc1,loc2)) ->
            O.fprintf"%s == %s" (I.Loc.dump loc1) (I.Loc.dump loc2)
        | Not p ->
            O.output "!(" ;
            dump_prop p ;
            O.output ")"
        | Or [] -> O.output "0"
        | Or [p] -> dump_prop p
        | Or (p::ps) ->
            O.output "(" ;
            dump_prop p ;
            O.output ") || (" ;
            dump_prop (Or ps) ;
            O.output ")" ;
            ()
        | And [] -> O.output "1"
        | And [p] -> dump_prop p
        | And (p::ps) ->
            O.output "(" ;
            dump_prop p ;
            O.output ") && (" ;
            dump_prop (And ps) ;
            O.output ")" ;
            ()
        | Implies (p1,p2) ->
            O.output "!(" ;
            dump_prop p1 ;
            O.output ") || (" ;
            dump_prop p2 ;
            O.output ")" ;
            () in
        dump_prop

      let funname = "final_cond"
      let funname_other = "final_cond_bis"

      let fundef_other p formals =
        let x = S.compile p in
        O.f "inline static int %s(%s) {" funname_other formals ;
        S.dump Indent.indent x ;
        O.o "}" ;
        O.o ""

      let fundef find_type cond =
        let locs = I.C.locations cond in
        let plocs =
          List.map
            (fun loc ->
              let t = find_type loc in
              Printf.sprintf "%s %s" t (I.Loc.dump loc))
            locs in
        let vals = I.C.location_values cond in
        let pvals =
          List.map
            (fun loc -> Printf.sprintf
                "void *%s" (I.V.dump (Constant.Symbolic loc))) vals in
        let formals = String.concat "," (plocs@pvals) in          
        let p = ConstrGen.prop_of cond in
        let other =
          try fundef_other p formals ; true
          with Switch.Cannot -> false in
        O.f "inline static int %s(%s) {" funname formals ;
        O.fprintf "%sint cond = " (Indent.as_string Indent.indent) ;
        dump p ;
        O.output ";\n" ;
        if other then begin
          O.fi "int cond_bis = %s(%s);"
            funname_other
            (String.concat "," (List.map I.Loc.dump locs)) ;
          O.oi "assert (cond == cond_bis);"
        end ;
        O.oi "return cond;" ;
        O.o "}" ;
        O.o ""


      let funcall cond dump_loc dump_val =
        let locs = I.C.locations cond in
        let plocs = List.map dump_loc locs in
        let vals = I.C.location_values cond in
        let pvals = List.map dump_val vals in
        Printf.sprintf "%s(%s)" funname (String.concat "," (plocs@pvals))


    end
