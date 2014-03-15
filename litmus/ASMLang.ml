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

module type I = sig
  include Template.I

(* Initial value of internal registers *)
  val internal_init : arch_reg -> (string * string) option
(* gcc assembly template register class *)
  val reg_class : arch_reg -> string
end

module Make(A:I)(Tmpl:Template.S with type arch_reg = A.arch_reg) = struct

  type arch_reg = Tmpl.arch_reg
  type t = Tmpl.t

  open Printf

  module RegSet =
    MySet.Make
      (struct
        type t = Tmpl.arch_reg
        let compare = A.reg_compare
      end)


  let all_regs t =
    let all_ins ins =
      RegSet.union (RegSet.of_list (ins.Tmpl.inputs@ins.Tmpl.outputs)) in
    List.fold_right all_ins t.Tmpl.code  (RegSet.of_list t.Tmpl.final)

  let trashed_regs t =
    let trashed_ins ins = RegSet.union (RegSet.of_list ins.Tmpl.outputs) in
    let all_trashed =
      List.fold_right trashed_ins t.Tmpl.code RegSet.empty in
    RegSet.diff all_trashed (RegSet.of_list t.Tmpl.final)

  let dump_clobbers chan _t =
    fprintf chan ":%s\n"
      (String.concat ","
         (List.map (fun s -> sprintf "\"%s\"" s)
            ("cc"::"memory"::
             List.map A.reg_to_string A.forbidden_regs)))

  let copy_name s = sprintf "_tmp_%s" s

  let tag_reg_def reg = sprintf "[%s]" (Tmpl.tag_reg reg)

  let dump_inputs chan t trashed =
    let all = all_regs t in
    let in_outputs = RegSet.union trashed  (RegSet.of_list t.Tmpl.final) in
(*
    eprintf "Outputs in In: %a\n"
      (fun chan rs -> RegSet.pp chan "," pp_reg rs)
      in_outputs ;
*)
    let dump_pair reg v =
      let dump_v = (* catch those addresses that are saved in a variable *)
        if Tmpl.cautious then match Tmpl.memory with
        | Memory.Indirect ->
            (fun v -> match v with
            | Constant.Symbolic _ -> copy_name (Tmpl.tag_reg reg)
            | Constant.Concrete _ -> Tmpl.dump_v v)
        | Memory.Direct -> Tmpl.dump_v
        else Tmpl.dump_v in
      if RegSet.mem reg in_outputs then begin
        match A.internal_init reg with
        | None -> sprintf "\"%s\" (%s)" (tag_reg_def reg) (dump_v v)
        | Some (s,_) -> sprintf "\"%s\" (%s)" (tag_reg_def reg) s
      end else match A.internal_init reg with
      | None ->
          sprintf "%s \"r\" (%s)" (tag_reg_def reg) (dump_v v)
      | Some (s,_) ->
          sprintf "%s \"r\" (%s)" (tag_reg_def reg) s in

    (* Input from state *)
    let ins =
      List.map
        (fun (reg,v) -> dump_pair reg v)
        t.Tmpl.init in
    (* All other inputs, apparently needed to get gcc to
       allocate registers avoiding all registers in template *)
    let rem =
      RegSet.diff
        all
        (List.fold_right
           (fun (reg,_) -> RegSet.add reg) t.Tmpl.init in_outputs) in
    let rem =
      RegSet.fold
        (fun reg k ->
          let v = Constant.Concrete 0 in
          dump_pair reg v::k)
        rem [] in

    fprintf chan ":%s\n" (String.concat "," (ins@rem))

  let dump_trashed_reg reg =
    sprintf "trashed_%s"
      (Tmpl.clean_reg (A.reg_to_string reg))

  let dump_outputs chan proc t trashed =
    let outs =
      String.concat ","
        (List.map
           (match Tmpl.memory with
           | Memory.Direct ->
               (fun (_,a) -> sprintf "[%s] \"=m\" (_a->%s[_i])" a a)
           | Memory.Indirect ->
               (fun (_,a) -> sprintf "[%s] \"=m\" (*_a->%s[_i])" a a)
           )
           t.Tmpl.addrs
         @List.map
             (fun reg ->
               if Tmpl.cautious then
                 sprintf "%s \"%s\" (%s)"
                 (tag_reg_def reg)
                 (A.reg_class reg)
                 (copy_name (Tmpl.dump_out_reg proc reg))
               else
                 sprintf "%s \"%s\" (%s)"
                 (tag_reg_def reg)
                 (A.reg_class reg)
                 (Tmpl.compile_out_reg proc reg))
             t.Tmpl.final
         @RegSet.fold
             (fun reg k ->
               sprintf "%s \"%s\" (%s)"
                 (tag_reg_def reg)
                 (A.reg_class reg)
                 (dump_trashed_reg reg)::k)
             trashed []) in
    fprintf chan ":%s\n" outs

  let dump_copies chan indent env proc t =
(*
    List.iter
      (fun (_,a) ->
        fprintf chan "%sint *%s = %s;\n" indent (copy_name a)
        (match O.memory with
        | Memory.Direct -> sprintf "&%s[_i]" a
        | Memory.Indirect -> sprintf "%s[_i]" a) ;
        fprintf chan "%smbar();\n" indent)
      t.addrs ;
*)
    List.iter
      (fun reg ->
        fprintf chan "%s%s %s = %s;\n" indent
          (Tmpl.dump_type env reg)
          (copy_name (Tmpl.dump_out_reg proc reg))
          (Tmpl.compile_out_reg proc reg) ;
        fprintf chan "%smcautious();\n" indent)
      t.Tmpl.final ;
    begin match Tmpl.memory with
    | Memory.Indirect ->
        List.iter
          (fun (reg,v) -> match v with
          | Constant.Symbolic a ->
              fprintf chan "%svoid *%s = %s;\n" indent
                (copy_name (Tmpl.tag_reg reg))
                (Tmpl.dump_v v) ;
              fprintf chan "%s_a->%s[_i] = %s;\n" indent
                (Tmpl.addr_cpy_name a proc)  (copy_name (Tmpl.tag_reg reg)) ;
              fprintf chan "%smcautious();\n" indent
          | Constant.Concrete _ -> ())
          t.Tmpl.init
    | Memory.Direct -> ()
    end ;
    ()

  let dump_save_copies chan indent proc t =
    List.iter
      (fun reg ->
        fprintf chan "%smcautious();\n" indent ;
        fprintf chan "%s%s = %s;\n" indent
          (Tmpl.compile_out_reg proc reg)
          (copy_name (Tmpl.dump_out_reg proc reg)))
      t.Tmpl.final ;
    ()

  let after_dump chan indent proc t =
    if Tmpl.cautious then begin
      dump_save_copies chan indent proc t
    end

  let before_dump chan indent env proc t trashed =
    RegSet.iter
      (fun reg ->
        let ty = match A.internal_init reg with
        | Some (_,ty) -> ty
        | None -> "int" in
        fprintf chan "%s%s %s;\n"
          indent ty (dump_trashed_reg reg))
      trashed ;
    if Tmpl.cautious then begin
      dump_copies chan indent env proc t
    end

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
    let trashed = trashed_regs t in
    before_dump chan indent env proc t trashed;
    fprintf chan "asm __volatile__ (\n" ;
    fprintf chan "\"\\n\"\n" ;
    fprintf chan "\"%cSTART _litmus_P%i\\n\"\n" Tmpl.comment proc ;
    begin match t.Tmpl.code with
    | [] -> fprintf chan "\"\"\n"
    | code -> dump_ins 0 code
    end ;
    fprintf chan "\"%cEND_litmus\\n\\t\"\n" Tmpl.comment ;
    dump_outputs chan proc t trashed ;
    dump_inputs chan t trashed ;
    dump_clobbers chan t  ;
    fprintf chan ");\n" ;
    after_dump chan indent proc t;
    ()

end
