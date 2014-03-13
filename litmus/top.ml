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

(***********************************************)
(* Parse a source file for the needs of litmus *)
(* (And then compile test)                     *)
(***********************************************)

open Answer

module type CommonConfig = sig
  val verbose : int
  val limit : bool
  val timeloop : int
  val stride : int option
  val avail : int option
  val runs : int
  val size : int
  val isync : bool
  val speedcheck : Speedcheck.t
  val safer : Safer.t
  val cautious : bool
  val preload : Preload.t
  val memory : Memory.t
  val prealloc : bool
  val launch : Launch.t
  val barrier : Barrier.t
  val linkopt : string
  val logicalprocs : int list option
  val affinity : Affinity.t
  val targetos : TargetOS.t
  val is_out : bool
  val sleep : int
  val driver : Driver.t
  val crossrun : Crossrun.t
  val gcc : string
  val c11 : bool
  val c11_fence : bool
  val xy : bool
  val pldw : bool
  val morearch : MoreArch.t
  val carch : Archs.System.t option
  val syncconst : int
  val signaling : bool
  val numeric_labels : bool
  val kind : bool
  val force_affinity : bool
  val smtmode : Smt.t
  val smt : int
  val contiguous : bool
  val syncmacro : int option
  val collect : Collect.t
  val hexa : bool
  val verbose_barrier : bool
  val verbose_prelude : bool
  val check_kind : string -> ConstrGen.kind option
  val check_cond : string -> string option
  val cross : bool
  val tarname : string
  val hint : string option
  val no : string option
  val index : string option
end

module type TopConfig = sig
  include CommonConfig
  val check_name : string -> bool
  val check_rename : string -> string option
(* Arch dependent options *)
  val mkopt : Option.opt -> Option.opt
end

module type Config = sig
  include GenParser.Config
  include Compile.Config
  include Skel.Config
  include Run.Config
  val limit : bool
end

module Top (OT:TopConfig) (Tar:Tar.S) : sig
  val from_files : string list -> unit
end = struct

  module W = Warn.Make(OT)

  module Utils (O:Config) (A:Arch.S) (A':Arch.Base) (Lang:Language.S)
      (Pseudo:PseudoAbstract.S) =
    struct
      module T = Test.Make(A')(Pseudo)
      module R = Run.Make(O)(Tar)(T.D)
      module MS = Skel.Make(O)(Pseudo)(A')(T)

      let get_cycle t =
        let info = t.MiscParser.info in
        List.assoc "Cycle" info

      let cycle_ok avoid t =
        try
          let cy = get_cycle t in
          not (avoid cy)
        with Not_found -> true


      let hash_ok env tname hash =
        try
          let ohash = StringMap.find tname env in
          if String.compare hash.hash ohash.hash <> 0 then begin
            Warn.user_error "Unconsistent hashes for test %s, previous file %s"
              tname ohash.filename
          end else begin
            if  ohash.filename <> hash.filename then
              W.warn  "Duplicate occurrence of test %s (%s,%s)"
                tname ohash.filename hash.filename
            else
              W.warn "File %s is referenced more then once"
                ohash.filename
          end ;
          false
        with Not_found ->  true

      let change_hint hint name t =
        try
          let more_info = Hint.get hint name in
          let info =
            more_info @
            List.filter
              (fun (k,_) ->
                try
                  let _ = List.assoc k more_info in
                  false
                with Not_found -> true)
              t.MiscParser.info in
          { t with MiscParser.info = info; }
        with Not_found -> t

      let get_utils utils =
        match utils with
        | [] ->
            let module O = struct
              include O
              let arch = A'.arch
            end in
            let module Obj = ObjUtil.Make(O)(Tar) in
            Obj.dump ()
        | _ -> utils

      let dump source doc compiled =
        Misc.output_protect
          (fun chan ->
             let module Out =
               Indent.Make(struct let out = chan end) in
             let module S = MS(Out)(Lang) in
             S.dump doc compiled)
          (Tar.outname source)

      let avail = match O.avail with
        | Some n -> n
        | None -> 1000

      let hash name parsed =
        try
          let hash = List.assoc "Hash" parsed.MiscParser.info in
          { filename=name; hash=hash;}
        with Not_found -> assert false

      let compile
          parse count_procs compile allocate
          hint avoid_cycle utils cycles hash_env
          name in_chan out_chan splitted =
        try begin
          let parsed = parse in_chan splitted in
          let doc = splitted.Splitter.name in
          let tname = doc.Name.name in
          close_in in_chan ;
          let nprocs = count_procs parsed.MiscParser.prog in
          let hash = hash name parsed in
          if
            cycle_ok avoid_cycle parsed &&
            hash_ok hash_env tname hash &&
            (not O.limit || nprocs <= avail)
          then begin
            let hash_env = StringMap.add tname hash hash_env in
            let parsed = change_hint hint doc.Name.name parsed in
            let module Alloc = CSymbReg.Make(A') in
            let allocated = allocate parsed in
            let compiled = compile allocated in
            let source = MyName.outname name ".c" in
            dump source doc compiled;
            let utils = get_utils utils in
            R.run name out_chan doc allocated source ;
            Completed (A.arch,doc,source,utils,cycles,hash_env)
          end else begin
            Warn.warn_always
              "Test %s not performed" doc.Name.name ;
            Absent A.arch
          end
        end with e -> Interrupted (A.arch,e)
    end


  module Make
      (O:Config)
      (A:Arch.S)
      (L:GenParser.LexParse with type instruction = A.pseudo)
      (XXXComp : XXXCompile.S with module A = A)
      (Lang:Language.S) =
    struct
      module Pseudo = struct
        type code = int * A.pseudo list
        let rec fmt_io io = match io with
        | A.Nop -> ""
        | A.Instruction ins -> A.dump_instruction ins
        | A.Label (lbl,io) -> lbl ^ ": " ^ fmt_io io
        | A.Macro (f,regs) ->
            Printf.sprintf
              "%s(%s)"
              f
              (String.concat "," (List.map A.pp_reg regs))

        let dump_prog (p,is) = Printf.sprintf "P%i" p::List.map fmt_io is

        let dump_prog_lines prog =
          let pp = List.map dump_prog prog in
          let pp = Misc.lines_of_prog pp in
          List.map (Printf.sprintf "%s;") pp

        let print_prog chan prog =
          let pp = List.map dump_prog prog in
          Misc.pp_prog chan pp
      end

      module Utils = Utils(O)(A)(A)(Lang)(Pseudo)
      module P = GenParser.Make(O)(A) (L)
      module Comp = Compile.Make (O)(A)(A)(Utils.T) (XXXComp)

      let compile =
        let allocate parsed =
          let module Alloc = SymbReg.Make(A) in
          Alloc.allocate_regs parsed
        in
        Utils.compile P.parse List.length Comp.compile allocate
    end


  module Make'
      (O:Config)
      (A:Arch.S)
      (L:CGenParser.LexParse)
      (Lang:Language.S) =
    struct
      module A' = struct
        module V = A.V

        type reg = string

        module Internal = struct
          type arch_reg = reg
          let pp_reg x = x
          let reg_compare = String.compare

          type arch_global = string
          let maybev_to_global = function
            | Constant.Concrete i -> "addr_" ^ string_of_int i
            | Constant.Symbolic s -> s
          let pp_global x = x
          let global_compare = String.compare

          let comment = A.I.comment
          let reg_class _ = assert false (* Unused *)
          let internal_init _ = assert false (* Unused *)
          let reg_to_string x = x
          let forbidden_regs = []
          let arch = `C
        end

        include Location.Make(Internal)

        let parse_reg x = Some x
        let reg_compare = Internal.reg_compare

        type state = (location * V.v) list

        module Out = Template.Make(O)(Internal)(V)

        let arch = Internal.arch

        let rec find_in_state loc = function
          | [] -> V.intToV 0
          | (loc2,v)::rem ->
              if location_compare loc loc2 = 0 then v
              else find_in_state loc rem
        let pp_reg x = x
      end
      module Pseudo = struct
        type code = CAst.t
        let dump_prog cfun =
          let f = function
            | CAst.Test { CAst.params; body; proc = i } ->
                let string_of_ty ty = RunType.dump ty ^ "*" in
                let f {CAst.param_ty; param_name} =
                  Printf.sprintf "%s %s" (string_of_ty param_ty) param_name
                in
                let params = String.concat ", " (List.map f params) in
                Printf.sprintf "static void P%i(%s) {%s}\n" i params body
            | CAst.Global x -> Printf.sprintf "{%s}\n\n" x
          in
          [f cfun]

        let dump_prog_lines prog =
          let pp = List.map dump_prog prog in
          let pp = List.concat pp in
          List.map (Printf.sprintf "%s\n") pp

        let print_prog chan prog =
          let pp = dump_prog_lines prog in
          List.iter (Printf.fprintf chan "%s") pp
      end

      module Utils = Utils(O)(A)(A')(Lang)(Pseudo)
      module P = CGenParser.Make(O)(Pseudo)(A')(L)
      module Comp = CCompile.Make(O)(Utils.T)

      let rec count_procs = function
        | CAst.Test _::xs -> 1 + count_procs xs
        | CAst.Global _::xs -> count_procs xs
        | [] -> 0

      let compile =
        let allocate parsed =
          let module Alloc = CSymbReg.Make(A') in
          let allocated = Alloc.allocate_regs parsed in
          { allocated with MiscParser.prog = allocated.MiscParser.prog; }
        in
        Utils.compile P.parse count_procs Comp.compile allocate
    end


  let debuglexer =  OT.verbose > 2

  module LexConfig =
    struct
      let debug = debuglexer
      let check_rename = OT.check_rename
    end


  module SP = Splitter.Make(LexConfig)

  let from_chan hint avoid_cycle fst cycles
      hash_env name in_chan out_chan =
(* First split the input file in sections *)
    let { Splitter.arch=arch ; _ } as splitted =
      SP.split name in_chan in
    let tname = splitted.Splitter.name.Name.name in
    if OT.check_name tname then begin
(* Then call appropriate compiler, depending upon arch *)
      let module V = SymbConstant in
      let opt = OT.mkopt (Option.get_default arch) in
      let word = Option.get_word opt in
      let module ODep = struct
        let word = word
        let delay = Option.get_delay opt
        let gccopts = Option.get_gccopts opt
      end in
(* Compile configuration, must also be used to configure arch modules *)
      let module OC = struct
        let word = word
        let syncmacro =OT.syncmacro
        let syncconst = OT.syncconst
        let memory = OT.memory
        let morearch = OT.morearch
        let cautious = OT.cautious
      end in
      let module OX = struct
        include OT
        include ODep
        let debuglexer = debuglexer
        let carch = lazy (assert false)
      end in
      let get_cfg = function
      | `PPC, _ | `PPCGen, _ | `X86, _ | `ARM, _ -> Some (module OX : Config)
      | `C, Some given_carch ->
          let module OX = struct
            include OX
            let carch = lazy given_carch
          end in
          Some (module OX : Config)
      | `C, None -> None
      in
      match get_cfg (arch, OT.carch) with
      | Some cfg ->
          let module Cfg = (val cfg : Config) in
          let get_lang = function
          | `PPC | `PPCGen | `X86 | `ARM -> (module ASMLang.Make : Language.S)
          | `C -> (module CLang.Make : Language.S)
          in
          let module Lang = (val (get_lang arch) : Language.S) in
          let aux = function
          | `PPC ->
              let module Arch' = PPCArch.Make(OC)(V) in
              let module LexParse = struct
                type instruction = Arch'.pseudo
                type token = PPCParser.token
                module Lexer = PPCLexer.Make(LexConfig)
                let lexer = Lexer.token
                let parser = PPCParser.main
              end in
              let module Compile = PPCCompile.Make(V)(OC) in
              let module X = Make(Cfg)(Arch')(LexParse)(Compile)(Lang) in
              X.compile hint avoid_cycle fst cycles hash_env
                name in_chan out_chan splitted
          | `PPCGen ->
              let module Arch' = PPCGenArch.Make(OC)(V) in
              let module LexParse = struct
                type instruction = Arch'.pseudo
                type token = PPCGenParser.token
                module Lexer = PPCGenLexer.Make(LexConfig)
                let lexer = Lexer.token
                let parser = PPCGenParser.main
              end in
              let module Compile = PPCGenCompile.Make(V)(OC) in
              let module X = Make(Cfg)(Arch')(LexParse)(Compile)(Lang) in
              X.compile hint avoid_cycle fst cycles hash_env
                name in_chan out_chan splitted
          | `X86 ->
              let module Arch' = X86Arch.Make(OC)(V) in
              let module LexParse = struct
                type instruction = Arch'.pseudo
                type token = X86Parser.token
                module Lexer = X86Lexer.Make(LexConfig)
                let lexer = Lexer.token
                let parser = X86Parser.main
              end in
              let module Compile = X86Compile.Make(V)(OC) in
              let module X = Make(Cfg)(Arch')(LexParse)(Compile)(Lang) in
              X.compile hint avoid_cycle fst cycles hash_env
                name in_chan out_chan splitted
          | `ARM ->
              let module Arch' = ARMArch.Make(OC)(V) in
              let module LexParse = struct
                type instruction = Arch'.pseudo
                type token = ARMParser.token
                module Lexer = ARMLexer.Make(LexConfig)
                let lexer = Lexer.token
                let parser = ARMParser.main
              end in
              let module Compile = ARMCompile.Make(V)(OC) in
              let module X = Make(Cfg)(Arch')(LexParse)(Compile)(Lang) in
              X.compile hint avoid_cycle fst cycles hash_env
                name in_chan out_chan splitted
          | `C ->
              let module Arch' = (val (match Lazy.force Cfg.carch with
                | `PPC -> (module PPCArch.Make(OC)(V) : Arch.S)
                | `PPCGen -> (module PPCGenArch.Make(OC)(V) : Arch.S)
                | `X86 -> (module X86Arch.Make(OC)(V) : Arch.S)
                | `ARM -> (module ARMArch.Make(OC)(V) : Arch.S)
                ) : Arch.S)
              in
              let module LexParse = struct
                type token = CParser.token
                let lexer = CLexer.main
                let parser = CParser.main
              end in
              let module X = Make'(Cfg)(Arch')(LexParse)(Lang) in
              X.compile hint avoid_cycle fst cycles hash_env
                name in_chan out_chan splitted
          in
          aux arch
      | None ->
          W.warn "Test %s not performed because -carch is not given but required while using C arch" tname ;
          Absent arch
    end else begin
      W.warn "Test %s not performed" tname ;
      Absent arch
    end

  let from_file
      hint avoid_cycles fst cycles hash_env
      name out_chan =
    Misc.input_protect
      (fun in_chan ->
        from_chan hint avoid_cycles fst cycles hash_env
          name in_chan out_chan) name

(* Call generic tar builder/runner *)
  module DF = DumpRun.Make (OT)(Tar) (struct let from_file = from_file end)

  let from_files = DF.from_files
end
