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

  module Make
      (O:Config)
      (A:Arch.S)
      (L:GenParser.LexParse with type instruction = A.pseudo)
      (XXXComp : XXXCompile.S with module A = A)
      (Lang:Language.S) =
    struct
      module Pseudo = struct
        type pseudo = A.pseudo
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
      end

      module P = GenParser.Make(O)(A) (L)
      module T = Test.Make(A)(Pseudo)
      module Comp = Compile.Make (O)(A)(T) (XXXComp)
      module MS = Skel.Make(O)(struct type pseudo = A.pseudo end)(T)
      module R = Run.Make(O)(Tar)(T.D)

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
            W.warn  "Duplicate occurrence of test %s (%s,%s)"
              tname ohash.filename hash.filename
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

      let compile
          hint avoid_cycle utils cycles hash_env
          name in_chan out_chan splitted  =
        try begin
          let parsed = P.parse in_chan splitted in
          let doc = splitted.Splitter.name in
          let tname = doc.Name.name in
          close_in in_chan ;
          let nprocs = List.length parsed.MiscParser.prog
          and avail = match O.avail with
          | Some n -> n
          | None -> 1000
          and hash =
            try
              let hash = List.assoc "Hash" parsed.MiscParser.info in
              { filename=name; hash=hash;}
            with Not_found -> assert false in
          if
            cycle_ok avoid_cycle parsed &&
            hash_ok hash_env tname hash &&
            (not O.limit || nprocs <= avail)
          then begin
            let hash_env = StringMap.add tname hash hash_env in
            let parsed = change_hint hint doc.Name.name parsed in
            let module Alloc = SymbReg.Make(A) in
            let allocated = Alloc.allocate_regs parsed in
            let compiled = Comp.compile allocated in
            let source = MyName.outname name ".c" in
            let () =
              Misc.output_protect
                (fun chan ->
                  let module Out =
                    Indent.Make(struct let out = chan end) in
                  let module S = MS(Out)(Lang) in
                  S.dump doc compiled)
                (Tar.outname source) in
            let utils =
              match utils with
              | [] ->
                  let module Obj = ObjUtil.Make(O)(Tar) in
                  Obj.dump ()
              | _ -> utils  in
            R.run name out_chan doc allocated source ;
            Completed (A.arch,doc,source,utils,cycles,hash_env)
          end else begin
            W.warn
              "Test %s not performed" doc.Name.name ;
            Absent A.arch
          end
        end with e -> Interrupted (A.arch,e)
    end


  module Make'
      (O:Config)
      (A:Arch.S)
      (L:CGenParser.LexParse with type instruction = CAst.param list * CAst.body)
      (Lang:Language.S) =
    struct
      module P = CGenParser.Make(O)(L)
      module Pseudo = struct
        type pseudo = L.instruction
        let dump_prog (i, l) =
          let f (params, body) =
            let string_of_ty = function
            | CAst.Int_ptr -> "int*"
            in
            let f {CAst.param_ty; param_name} =
              Printf.sprintf "%s %s" (string_of_ty param_ty) param_name
            in
            let params = String.concat ", " (List.map f params) in
            Printf.sprintf "static void P%i(%s) {\n%s\n}\n" i params body
          in
          Printf.sprintf "P%i" i :: List.map f l
      end
      module T = Test.Make(A)(Pseudo)
      module Comp = CCompile.Make(O)(T)
      module MS = Skel.Make(O)(struct type pseudo = P.pseudo end)(T)
      module R = Run.Make(O)(Tar)(T.D)

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
            W.warn  "Duplicate occurrence of test %s (%s,%s)"
              tname ohash.filename hash.filename
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

      let compile
          hint avoid_cycle utils cycles hash_env
          name in_chan out_chan splitted  =
        try begin
          let parsed = P.parse in_chan splitted in
          let doc = splitted.Splitter.name in
          let tname = doc.Name.name in
          close_in in_chan ;
          let nprocs = List.length parsed.MiscParser.prog
          and avail = match O.avail with
          | Some n -> n
          | None -> 1000
          and hash =
            try
              let hash = List.assoc "Hash" parsed.MiscParser.info in
              { filename=name; hash=hash;}
            with Not_found -> assert false in
          if
            cycle_ok avoid_cycle parsed &&
            hash_ok hash_env tname hash &&
            (not O.limit || nprocs <= avail)
          then begin
            let hash_env = StringMap.add tname hash hash_env in
            let parsed = change_hint hint doc.Name.name parsed in
            let module Alloc = CSymbReg.Make(A) in
            let allocated = Alloc.allocate_regs parsed in
            let compiled = Comp.compile allocated in
            let source = MyName.outname name ".c" in
            let () =
              Misc.output_protect
                (fun chan ->
                  let module Out =
                    Indent.Make(struct let out = chan end) in
                  let module S = MS(Out)(Lang) in
                  S.dump doc compiled)
                (Tar.outname source) in
            let utils =
              match utils with
              | [] ->
                  let module Obj = ObjUtil.Make(O)(Tar) in
                  Obj.dump ()
              | _ -> utils  in
            R.run name out_chan doc allocated source ;
            Completed (A.arch,doc,source,utils,cycles,hash_env)
          end else begin
            W.warn
              "Test %s not performed" doc.Name.name ;
            Absent A.arch
          end
        end with e -> Interrupted (A.arch,e)
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
          let rec aux = function
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
                type instruction = CAst.param list * CAst.body
                type token = CParser.token
                let lexer = CLexer.main
                let parser x y =
                  let f (procs, instrs) {CAst.proc; params; body} =
                    (proc :: procs, [params, body] :: instrs)
                  in
                  List.fold_left f ([], []) (CParser.main x y)
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
