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

module type I = sig
  type arch_reg
  val arch : Archs.t
  val forbidden_regs : arch_reg list
  val reg_compare : arch_reg -> arch_reg -> int
  val reg_to_string : arch_reg -> string
(* Initial value of internal registers *)
  val internal_init : arch_reg -> (string * string) option
(* gcc assembly template register class *)
  val reg_class : arch_reg -> string
(* gas line comment char *)
  val comment : char
end


exception Error of string

module type Config = sig
  val memory : Memory.t
  val cautious : bool
end

module type S = sig

  type arch_reg

  type flow = Next | Branch of string
  type ins =
      { memo:string ; inputs:arch_reg list ;  outputs:arch_reg list;
        (* Jumps *)
        label:string option ;  branch : flow list ;
        (* A la ARM conditional execution *)
        cond: bool ;
        comment: bool; }

  val empty_ins : ins

  type t = {
      init : (arch_reg * Constant.v) list ;
      addrs : (int * string) list ;
      final : arch_reg list ;
      code : ins list;
    }
  val get_addrs : t -> string list
  val fmt_reg : arch_reg -> string
  val dump_label : string -> string
  val dump_out_reg : int -> arch_reg -> string
  val dump_v : Constant.v -> string
  val addr_cpy_name : string -> int -> string

  val after_dump : out_channel -> string -> int -> t -> unit

  (* TODO: Remove this ugly module *)
  module Reexport : sig
    module O : Config
    module A : I with type arch_reg = arch_reg
    module V : Constant.S

    module RegSet : MySet.S with type elt = A.arch_reg

    val to_string : ins -> string
    val compile_out_reg : int -> arch_reg -> string
    val dump_type : ('a * RunType.t) list -> 'a -> string
    val trashed_regs : t -> RegSet.t
    val dump_trashed_reg : A.arch_reg -> string
    val dump_copies : out_channel -> string -> (A.arch_reg * RunType.t) list -> int -> t -> unit
    val dump_outputs : out_channel -> int -> t -> RegSet.t -> unit
    val dump_inputs : out_channel -> t -> RegSet.t -> unit
    val dump_clobbers : out_channel -> 'a -> unit
    val dump_save_copies : out_channel -> string -> int -> t -> unit

    val before_dump : out_channel -> string -> (arch_reg * RunType.t) list -> int -> t -> RegSet.t -> unit
  end

end

module Make(O:Config) (A:I) (V:Constant.S): S
  with type arch_reg = A.arch_reg =
struct
  open Printf
  open Constant
  open Memory

  type arch_reg = A.arch_reg

  type flow = Next | Branch of string
  type ins =
      { memo:string ; inputs:arch_reg list ;  outputs:arch_reg list;
        (* Jumps *)
        label:string option ;  branch : flow list ;
        cond:bool ;
        comment:bool;}

  let empty_ins =
    { memo="" ; inputs=[]; outputs=[];
      label=None; branch=[Next]; cond=false; comment=false;}

  type t = {
      init : (arch_reg * Constant.v) list ;
      addrs : (int * string) list ;
      final : arch_reg list ;
      code : ins list;
    }


  let get_addrs { init=init; addrs=addrs; _ } =
    let set =
      StringSet.union
        (StringSet.of_list (List.map (fun (_,a) -> a) addrs))
        (StringSet.of_list
           (List.fold_left
              (fun k (_,v) ->
                match v with Symbolic s -> s::k
                | Concrete _ -> k)
              [] init)) in
    StringSet.elements set


  exception Internal of string
  let internal msg = raise (Internal msg)

  exception Error of string
  let error msg = raise (Error msg)


  module RegSet =
    MySet.Make
      (struct
        type t = A.arch_reg
        let compare = A.reg_compare
      end)


  let escape_percent s =
    Misc.map_string
      (fun c -> match c with
      | '%' -> "%%"
      | _ -> String.make 1 c)
      s

  let pp_reg r = escape_percent (A.reg_to_string r)
  let fmt_reg = pp_reg

  let dump_label lbl = lbl

  let clean_reg s =
    Misc.map_string
      (fun c -> match c with
      | '%' -> ""
      | _  -> String.make 1 c)
      s

  let tag_reg reg = clean_reg (A.reg_to_string reg)

  let tag_reg_ref reg = sprintf "%%[%s]" (tag_reg reg)
  and tag_reg_def reg = sprintf "[%s]" (tag_reg reg)

  let dump_out_reg proc reg =
    sprintf "out_%i_%s"
      proc
      (clean_reg (A.reg_to_string reg))

  let dump_trashed_reg reg =
    sprintf "trashed_%s"
      (clean_reg (A.reg_to_string reg))

  let compile_out_reg proc reg =
    sprintf "%s[_i]" (dump_out_reg proc reg)


  let get_reg k rs =
    try List.nth rs k
    with _ ->
      internal
        (sprintf "get_reg %i in {%s}"
           k (String.concat ","
                (List.map pp_reg rs)))

  let escape_percent s =
    let len = String.length s in
    let buff = Buffer.create 16 in
    let rec do_rec i =
      if i < len then begin
        begin match s.[i] with
        | '%' -> Buffer.add_string buff "%%"
        | c -> Buffer.add_char buff c
        end ;
        do_rec (i+1)
      end in
    do_rec 0 ; Buffer.contents buff

  let to_string t =

    let digit i =
      let c = Char.code t.memo.[i] in
      let n = c - Char.code '0' in
      if 0 <= n && n <= 2 then n
      else internal (sprintf "bad digit '%i'" n)

    and substring i j =
      try String.sub t.memo i (j-i)
      with _ -> internal (sprintf "substring %i-%i" i j)

    and look_escape i =
      try String.index_from t.memo i '^'
      with
      | Not_found -> raise Not_found
      | _ -> internal (sprintf "look_escape %i" i) in


    let b = Buffer.create 20 in
    let add = Buffer.add_string b in
    let len = String.length t.memo in

    let rec do_rec i =
      if i < len then
        try
          let j = look_escape i in
          add (substring i j) ;
          let n = digit (j+2) in
          begin match t.memo.[j+1] with
          | 'i' -> add (tag_reg_ref (get_reg n t.inputs))
          | 'o' -> add (tag_reg_ref (get_reg n t.outputs))
          | c -> internal (sprintf "bad escape '%c'" c)
          end ;
          do_rec (j+3)
        with Not_found -> add (substring i len) in
    try
      if t.comment then sprintf "%c%s" A.comment (escape_percent t.memo)
      else begin
        do_rec 0  ; Buffer.contents b
      end
    with Internal msg ->
      error (sprintf "memo: %s, error: %s" t.memo msg)

  let copy_name s = sprintf "_tmp_%s" s

  let dump_type env reg =
    try RunType.dump (List.assoc reg env) with
      | Not_found -> "int"


  let dump_addr a = match O.memory with
  | Direct -> sprintf "&_a->%s[_i]" a
  | Indirect -> sprintf "_a->%s[_i]" a

  let dump_v v = match v with
  | Concrete i -> sprintf "%i" i
  | Symbolic a -> dump_addr a

  let addr_cpy_name s p = sprintf "_addr_%s_%i" s p

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
          (dump_type env reg)
          (copy_name (dump_out_reg proc reg))
          (compile_out_reg proc reg) ;
        fprintf chan "%smcautious();\n" indent)
      t.final ;
    begin match O.memory with
    | Indirect ->
        List.iter
          (fun (reg,v) -> match v with
          | Symbolic a ->
              fprintf chan "%svoid *%s = %s;\n" indent
                (copy_name (tag_reg reg))
                (dump_v v) ;
              fprintf chan "%s_a->%s[_i] = %s;\n" indent
                (addr_cpy_name a proc)  (copy_name (tag_reg reg)) ;
              fprintf chan "%smcautious();\n" indent
          | Concrete _ -> ())
          t.init
    | Direct -> ()
    end ;
    ()

  let dump_save_copies chan indent proc t =
    List.iter
      (fun reg ->
        fprintf chan "%smcautious();\n" indent ;
        fprintf chan "%s%s = %s;\n" indent
          (compile_out_reg proc reg)
          (copy_name (dump_out_reg proc reg)))
      t.final ;
    ()

  let dump_outputs chan proc t trashed =
    let outs =
      String.concat ","
        (List.map
           (match O.memory with
           | Direct ->
               (fun (_,a) -> sprintf "[%s] \"=m\" (_a->%s[_i])" a a)
           | Indirect ->
               (fun (_,a) -> sprintf "[%s] \"=m\" (*_a->%s[_i])" a a)
           )
           t.addrs
         @List.map
             (fun reg ->
               if O.cautious then
                 sprintf "%s \"%s\" (%s)"
                 (tag_reg_def reg)
                 (A.reg_class reg)
                 (copy_name (dump_out_reg proc reg))
               else
                 sprintf "%s \"%s\" (%s)"
                 (tag_reg_def reg)
                 (A.reg_class reg)
                 (compile_out_reg proc reg))
             t.final
         @RegSet.fold
             (fun reg k ->
               sprintf "%s \"%s\" (%s)"
                 (tag_reg_def reg)
                 (A.reg_class reg)
                 (dump_trashed_reg reg)::k)
             trashed []) in
    fprintf chan ":%s\n" outs

  let all_regs t =
    let all_ins ins =
      RegSet.union (RegSet.of_list (ins.inputs@ins.outputs)) in
    List.fold_right all_ins t.code  (RegSet.of_list t.final)

  let trashed_regs t =
    let trashed_ins ins = RegSet.union (RegSet.of_list ins.outputs) in
    let all_trashed =
      List.fold_right trashed_ins t.code RegSet.empty in
    RegSet.diff all_trashed (RegSet.of_list t.final)


  let dump_inputs chan t trashed =
    let all = all_regs t in
    let in_outputs = RegSet.union trashed  (RegSet.of_list t.final) in
(*
    eprintf "Outputs in In: %a\n"
      (fun chan rs -> RegSet.pp chan "," pp_reg rs)
      in_outputs ;
*)
    let dump_pair reg v =
      let dump_v = (* catch those addresses that are saved in a variable *)
        if O.cautious then match O.memory with
        | Indirect ->
            (fun v -> match v with
            | Symbolic _ -> copy_name (tag_reg reg)
            | Concrete _ -> dump_v v)
        | Direct -> dump_v
        else dump_v in
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
        t.init in
    (* All other inputs, apparently needed to get gcc to
       allocate registers avoiding all registers in template *)
    let rem =
      RegSet.diff
        all
        (List.fold_right
           (fun (reg,_) -> RegSet.add reg) t.init in_outputs) in
    let rem =
      RegSet.fold
        (fun reg k ->
          let v = Concrete 0 in
          dump_pair reg v::k)
        rem [] in

    fprintf chan ":%s\n" (String.concat "," (ins@rem))


  let dump_clobbers chan _t =
    fprintf chan ":%s\n"
      (String.concat ","
         (List.map (fun s -> sprintf "\"%s\"" s)
            ("cc"::"memory"::
             List.map A.reg_to_string A.forbidden_regs)))

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

  let after_dump chan indent proc t =
    if O.cautious then begin
      dump_save_copies chan indent proc t
    end


  (* TODO: Remove this ugly module *)
  module Reexport = struct
    module A = A
    module O = O
    module V = V

    module RegSet = RegSet

    let to_string = to_string
    let compile_out_reg = compile_out_reg
    let dump_type = dump_type
    let trashed_regs = trashed_regs
    let dump_trashed_reg = dump_trashed_reg
    let dump_copies = dump_copies
    let dump_outputs = dump_outputs
    let dump_inputs = dump_inputs
    let dump_clobbers = dump_clobbers
    let dump_save_copies = dump_save_copies

    let before_dump = before_dump
  end

end
