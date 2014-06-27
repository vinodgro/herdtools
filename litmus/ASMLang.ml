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

module type Config = sig
  val memory : Memory.t
  val cautious : bool
end

module type I = sig
  include Template.I
(* Forbidden registers *)
  val forbidden_regs :  arch_reg list
(* Initial value of internal registers *)
  val internal_init : arch_reg -> (string * string) option
(* gcc assembly template register class *)
  val reg_class : arch_reg -> string
end

module Make
    (O:Config)(A:I)(Tmpl:Template.S with type arch_reg = A.arch_reg) = struct

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

  let dump_inputs compile_val chan t trashed =
    let all = all_regs t in
    let in_outputs = RegSet.union trashed  (RegSet.of_list t.Tmpl.final) in
(*
    eprintf "Outputs in In: %a\n"
      (fun chan rs -> RegSet.pp chan "," pp_reg rs)
      in_outputs ;
*)
    let dump_pair reg v =
      let dump_v = (* catch those addresses that are saved in a variable *)
        if O.cautious then match O.memory with
         | Memory.Indirect ->
            (fun v -> match v with
            | Constant.Symbolic _ -> copy_name (Tmpl.tag_reg reg)
            | Constant.Concrete _ -> Tmpl.dump_v v)
        | Memory.Direct -> Tmpl.dump_v
        else compile_val  in
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

  let compile_addr_inline x = sprintf "_a->%s[_i]" x
  and compile_addr_fun x = sprintf "*%s" x 

  let dump_outputs compile_addr compile_out_reg chan proc t trashed =
    let outs =
      String.concat ","
        (List.map
           (match O.memory with
           | Memory.Direct ->
               (fun a -> sprintf "[%s] \"=m\" (%s)" a (compile_addr a))
           | Memory.Indirect ->
               (fun a -> sprintf "[%s] \"=m\" (*%s)" a (compile_addr a)))
           t.Tmpl.addrs
         @List.map
             (fun reg ->
               if O.cautious then
                 sprintf "%s \"%s\" (%s)"
                 (tag_reg_def reg)
                 (A.reg_class reg)
                 (copy_name (Tmpl.dump_out_reg proc reg))
               else
                 sprintf "%s \"%s\" (%s)"
                 (tag_reg_def reg)
                 (A.reg_class reg)
                 (compile_out_reg proc reg))
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
    begin match O.memory with
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
    if O.cautious then begin
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
    if O.cautious then begin
      dump_copies chan indent env proc t
    end

  let do_dump compile_val compile_addr compile_out_reg chan indent env proc t =
    let rec dump_ins k ts = match ts with
    | [] -> ()
    | t::ts ->
        begin match t.Tmpl.label with
        | Some _ ->
            fprintf chan "\"%c_litmus_P%i_%i\\n\"\n" A.comment proc k
        | None ->
            fprintf chan "\"%c_litmus_P%i_%i\\n%s\"\n"
              A.comment proc k
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
    fprintf chan "\"%s\\n\"\n" (LangUtils.start_comment A.comment proc) ;
    begin match t.Tmpl.code with
    | [] -> fprintf chan "\"\"\n"
    | code -> dump_ins 0 code
    end ;
    fprintf chan "\"%s\\n\\t\"\n" (LangUtils.end_comment A.comment proc) ;
    dump_outputs compile_addr compile_out_reg chan proc t trashed ;
    dump_inputs compile_val chan t trashed ;
    dump_clobbers chan t  ;
    fprintf chan ");\n" ;
    after_dump chan indent proc t;
    ()

  let dump chan indent env globEnv volatileEnv proc t =
    do_dump 
      Tmpl.dump_v compile_addr_inline Tmpl.compile_out_reg
      chan indent env proc t

  let compile_val_fun = match O.memory with
  | Memory.Direct ->
      (fun v -> match v with
      | Constant.Symbolic s -> sprintf "%s" s
      | Constant.Concrete _ -> Tmpl.dump_v v)
  | Memory.Indirect ->
      (fun v -> match v with
      | Constant.Symbolic s -> sprintf "*%s" s
      | Constant.Concrete _ -> Tmpl.dump_v v)

  let dump_fun chan env globEnv volatileEnv proc t =
    let addrs = Tmpl.get_addrs t in
    let addrs =
      List.map
        (fun x ->
          let ty =
            try List.assoc x globEnv
            with Not_found -> assert false in
          match O.memory with
          | Memory.Direct ->  
              sprintf "%s *%s" (CType.dump ty) x
          | Memory.Indirect -> 
              sprintf "%s **%s" (CType.dump ty) x)
        addrs in
    let outs =
      List.map
        (fun x ->
          let ty =
            try List.assoc x env
            with Not_found -> assert false in
          let x = Tmpl.dump_out_reg proc x in
          sprintf "%s *%s" (CType.dump ty) x) t.Tmpl.final in          
    let params =  String.concat "," (addrs@outs) in
    LangUtils.dump_code_def chan proc params ;
    do_dump
      compile_val_fun
      compile_addr_fun
      (fun p r  -> sprintf "*%s" (Tmpl.dump_out_reg p r))
      chan "  " env proc t ;
    fprintf chan "}\n\n" ;
    ()

  let compile_addr_call x = sprintf "&_a->%s[_i]" x
  let compile_out_reg_call proc reg = 
    sprintf "&%s" (Tmpl.compile_out_reg proc reg)

  let dump_call chan indent env globEnv volatileEnv proc t =
    let addrs = List.map compile_addr_call (Tmpl.get_addrs t)
    and outs = List.map (compile_out_reg_call proc) t.Tmpl.final in
    let args = String.concat "," (addrs@outs) in
    LangUtils.dump_code_call chan indent proc args

end
