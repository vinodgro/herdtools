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

  let dump chan indent env _ _ proc t =
    let rec dump_ins k ts = match ts with
    | [] -> ()
    | t::ts ->
        begin match t.Tmpl.label with
        | Some _ ->
            fprintf chan "\"%c_litmus_P%i_%i\\n\"\n" Tmpl.comment proc k
        | None ->
            fprintf chan "\"%c_litmus_P%i_%i\\n%s\"\n"
              Tmpl.comment proc k
              (if t.Tmpl.comment then "" else "\\t")
        end ;
        fprintf chan "\"%s\\n\"\n" (Tmpl.to_string t) ;
(*
        fprintf chan "\"%-20s%c_litmus_P%i_%i\\n\\t\"\n"
          (to_string t) A.comment proc k ;
*)
        dump_ins (k+1) ts in
    let trashed = Tmpl.trashed_regs t in
    Tmpl.before_dump chan indent env proc t trashed;
    fprintf chan "asm __volatile__ (\n" ;
    fprintf chan "\"\\n\"\n" ;
    fprintf chan "\"%cSTART _litmus_P%i\\n\"\n" Tmpl.comment proc ;
    begin match t.Tmpl.code with
    | [] -> fprintf chan "\"\"\n"
    | code -> dump_ins 0 code
    end ;
    fprintf chan "\"%cEND_litmus\\n\\t\"\n" Tmpl.comment ;
    Tmpl.dump_outputs chan proc t trashed ;
    Tmpl.dump_inputs chan t trashed ;
    Tmpl.dump_clobbers chan t  ;
    fprintf chan ");\n" ;
    Tmpl.after_dump chan indent proc t;
    ()

end
