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

  let dump chan indent env globEnv envVolatile proc t =
    let out x = Printf.fprintf chan x in
    out "%sdo {\n" indent;
    begin
      let indent = "  " ^ indent in
      let dump_input x =
        let x = Tmpl.fmt_reg x in
        let volatile = List.exists (Misc.string_eq x) envVolatile in
        let ty =
          let f t = function
            | true -> "volatile " ^ RunType.dump t ^ "*"
            | false -> RunType.dump t ^ "*"
          in
          try
            f (List.assoc x globEnv) volatile
          with
          | Not_found -> assert false
        in
        match Tmpl.memory with
        | Memory.Direct ->
            out "%s%s %s = (%s)&_a->%s[_i];\n" indent ty x ty x
        | Memory.Indirect ->
            out "%s%s %s = (%s)_a->%s[_i];\n" indent ty x ty x
      in
      let dump_output x =
        let outname = Tmpl.compile_out_reg proc x in
        out "%s%s = %s;\n" indent outname (Tmpl.fmt_reg x)
      in
      let print_start = out "%sasm __volatile__ (\"%cSTART _litmus_P%i\\n\" ::: \"memory\");\n" in
      let print_end = out "%sasm __volatile__ (\"%cEND _litmus_P%i\\n\" ::: \"memory\");\n" in
      let dump_ins x =
        List.iter dump_input x.Tmpl.inputs;
        print_start indent Tmpl.comment proc;
        out "%s\n" (Tmpl.to_string x);
        print_end indent Tmpl.comment proc;
        List.iter dump_output x.Tmpl.outputs
      in
      List.iter dump_ins t.Tmpl.code;
    end;
    out "%s} while(0);\n" indent

end
