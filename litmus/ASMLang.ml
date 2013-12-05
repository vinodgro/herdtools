(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module Make(Tmpl:Template.S) = struct

  open Printf
  open Tmpl
  open Tmpl.Reexport

  let dump chan indent env proc t =
    let rec dump_ins k ts = match ts with
    | [] -> ()
    | t::ts ->
        begin match t.label with
        | Some _ ->
            fprintf chan "\"%c_litmus_P%i_%i\\n\"\n"  A.comment proc k
        | None ->
            fprintf chan "\"%c_litmus_P%i_%i\\n%s\"\n"
              A.comment proc k
              (if t.comment then "" else "\\t")
        end ;
        fprintf chan "\"%s\\n\"\n" (to_string t) ;
(*
        fprintf chan "\"%-20s%c_litmus_P%i_%i\\n\\t\"\n"
          (to_string t) A.comment proc k ;
*)
        dump_ins (k+1) ts in
    let trashed = trashed_regs t in
    before_dump chan indent env proc t trashed;
    fprintf chan "asm __volatile__ (\n" ;
    fprintf chan "\"\\n\"\n" ;
    fprintf chan "\"%cSTART _litmus_P%i\\n\"\n" A.comment proc ;
    begin match t.code with
    | [] -> fprintf chan "\"\"\n"
    | code -> dump_ins 0 code
    end ;
    fprintf chan "\"%cEND_litmus\\n\\t\"\n" A.comment ;
    dump_outputs chan proc t trashed ;
    dump_inputs chan t trashed ;
    dump_clobbers chan t  ;
    fprintf chan ");\n" ;
    after_dump chan indent proc t;
    ()

end
