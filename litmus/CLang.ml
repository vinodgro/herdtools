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

  type arch_reg = Tmpl.arch_reg
  type t = Tmpl.t

  let dump_start chan indent proc =
    fprintf chan
      "%sasm __volatile__ (\"%cSTART _litmus_P%i\\n\" ::: \"memory\");\n"
      indent Tmpl.comment proc

  let dump_end chan indent proc =
    fprintf chan
      "%sasm __volatile__ (\"%cEND _litmus_P%i\\n\" ::: \"memory\");\n"
      indent Tmpl.comment proc

  let dump_global_def globEnv envVolatile x =
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
    ty,x

  let dump_output_def env proc x =
    let outname = Tmpl.dump_out_reg proc x
    and ty =
      try List.assoc x env with Not_found -> assert false in
    sprintf "%s*" (RunType.dump ty),outname
    
  let dump_fun chan env globEnv envVolatile proc t =
    let out x = fprintf chan x in
    match t.Tmpl.code with
    | [t] ->
        let input_defs =
          List.map (dump_global_def globEnv envVolatile) t.Tmpl.inputs
        and output_defs =
          List.map (dump_output_def env proc) t.Tmpl.outputs in
        let defs = input_defs@output_defs in
        let params =
          String.concat ","
            (List.map
               (fun (ty,v) -> sprintf "%s %s" ty v)
               defs) in
        (* Function prototype  *)
        LangUtils.dump_code_def chan proc params ;
        (* body *)
        dump_start chan "  " proc ;
        out "%s\n" (Tmpl.to_string t) ;
        dump_end chan "  " proc ;
        (* output parameters *)
        List.iter
          (fun reg ->
            out "  *%s = %s;\n"
              (Tmpl.dump_out_reg proc reg)
              (Tmpl.fmt_reg reg))
          t.Tmpl.outputs ;
        out "}\n"
    | _ -> assert false
    
  let dump_call chan indent env globEnv envVolatile proc t =
    match t.Tmpl.code with
    | [t] ->
        let global_args =
          List.map
            (fun x ->
              let _ty,x = dump_global_def globEnv envVolatile x in
              sprintf "&_a->%s[_i]" x)
            t.Tmpl.inputs
        and out_args =
          List.map
            (fun x -> sprintf "&%s" (Tmpl.compile_out_reg proc x))
            t.Tmpl.outputs in
        let args = String.concat "," (global_args@out_args) in
        LangUtils.dump_code_call chan indent proc args
    | _ -> assert false

  let dump chan indent env globEnv envVolatile proc t =
    let out x = fprintf chan x in
    out "%sdo {\n" indent;
    begin
      let indent = "  " ^ indent in
      let dump_input x =
        let ty,x = dump_global_def globEnv envVolatile x in
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
      let print_start = dump_start chan in
      let print_end = dump_end chan in
      let dump_ins x =
        List.iter dump_input x.Tmpl.inputs;
        print_start indent proc;
        out "%s\n" (Tmpl.to_string x);
        print_end indent proc;
        List.iter dump_output x.Tmpl.outputs
      in
      List.iter dump_ins t.Tmpl.code;
    end;
    out "%s} while(0);\n" indent

end
