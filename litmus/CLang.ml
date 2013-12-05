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

  let dump chan indent env proc t =
    let out x = Printf.fprintf chan x in
    let dump_ins x =
      out "%s\n" (Tmpl.Reexport.to_string x)
    in
    out "{\n";
    let trashed = Tmpl.Reexport.trashed_regs t in
    Tmpl.Reexport.before_dump chan indent env proc t trashed;
    List.iter dump_ins t.Tmpl.code;
    Tmpl.after_dump chan indent proc t;
    out "}\n"

end
