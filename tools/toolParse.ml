(*********************************************************************)
(*                        DIY                                        *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(***************************************)
(* Apply a function (zyva) to one test *)
(***************************************)

open Archs

module Top
    (T:sig type t end) (* Return type, must be abstracted *)
    (B: functor(A:ArchBase.S)->
      (sig val zyva : Name.t -> A.pseudo MiscParser.t -> T.t end)) :
sig
  val from_file : string -> T.t
end = struct

  module Make
      (A:ArchBase.S) 
      (L:GenParser.LexParse with type instruction = A.pseudo) =
    struct
      module P = GenParser.Make(GenParser.DefaultConfig)(A)(L)
      module X = B(A)  


      let zyva chan splitted =
        let name = splitted.Splitter.name in
        let parsed = P.parse chan splitted in
        X.zyva name parsed
    end

  module LexConf = Splitter.Default

  let from_chan chan splitted = 
    match splitted.Splitter.arch with
    | PPC ->
        let module PPC = PPCBase in
        let module PPCLexParse = struct
	  type instruction = PPC.pseudo
	  type token = PPCParser.token

          module L = PPCLexer.Make(LexConf)
	  let lexer = L.token
	  let parser = PPCParser.main
        end in
        let module X = Make (PPC) (PPCLexParse) in
        X.zyva chan splitted
(*
  | PPCGen ->
  let module PPCGen = PPCGenArch.Make(V) in
  let module PPCGenLexParse = struct
  type instruction = PPCGen.pseudo
  type token = PPCGenParser.token

  let lexer = PPCGenLexer.token
  let parser = PPCGenParser.main
  end in
  let module X = Make (PPCGen) (PPCGenLexParse) in
  X.zyva chan splitted
 *)
    | X86 ->
        let module X86 = X86Base in
        let module X86LexParse = struct
	  type instruction = X86.pseudo
	  type token = X86Parser.token

          module L = X86Lexer.Make(LexConf)
	  let lexer = L.token
	  let parser = X86Parser.main
        end in
        let module X = Make (X86) (X86LexParse) in
        X.zyva chan splitted
    | ARM ->
        let module ARM = ARMBase in
        let module ARMLexParse = struct
	  type instruction = ARM.pseudo
	  type token = ARMParser.token

          module L = ARMLexer.Make(LexConf)
	  let lexer = L.token
	  let parser = ARMParser.main
        end in
        let module X = Make (ARM) (ARMLexParse) in
        X.zyva chan splitted
    | C -> Warn.fatal "No C arch in toolParse.ml"

  let from_file name =
    let module Y = ToolSplit.Top(LexConf)(T)
        (struct
          let zyva = from_chan
        end) in
    Y.from_file name

end
