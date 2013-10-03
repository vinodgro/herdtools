(*********************************************************************)
(*                       DIY                                         *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Pretty print tests *)

open Printf


module Top
    (O:
       sig
         val verbose : int
         val texmacros : bool
         val hexa : bool
         val outputdir : string option
         val mode : OutMode.t
       end) =
  struct

    module T = struct
      type t = unit
    end

    (* Text dump *)
    module Text(A:ArchBase.S) = struct

      module D =
        SimpleDumper.Make
          (struct
            module A = A

            let dump_loc = MiscParser.dump_location

            let dump_state_atom (loc,v) =
              sprintf "%s=%s" (dump_loc loc) (SymbConstant.pp_v v)

            type state = MiscParser.state
            let dump_state st =
              String.concat " "
                (List.map
                   (fun a -> sprintf "%s;" (dump_state_atom a))
                   st)

                
            type constr = MiscParser.constr
            let dump_atom a =
              let open ConstrGen in
              match a with
              | LV (loc,v) -> dump_state_atom (loc,v)
              | LL (loc1,loc2) ->
                  sprintf "%s=%s" (dump_loc loc1) (MiscParser.dump_rval loc2)

            let dump_constr = ConstrGen.constraints_to_string dump_atom

            type location = MiscParser.location
            let dump_location = dump_loc
                
          end)

      let zyva = match O.outputdir with
      | None -> D.dump_info stdout
      | Some d ->
          fun name parsed ->
            let fname = name.Name.file in
            let fname = Filename.basename fname in
            let fname = Filename.concat d fname in
            Misc.output_protect
              (fun chan -> D.dump_info chan name parsed)
              fname

    end

    module Latex(A:ArchBase.S) = struct
      module Arch = struct
        include A
        module V = struct
          include SymbConstant
          let maybevToV c = c
        end
        type location = 
          | Location_global of Constant.v
          | Location_reg of int * A.reg

        let maybev_to_location c = Location_global c

        let pp_location = function
          | Location_global c -> V.pp O.hexa c
          | Location_reg (i,r) -> sprintf "%i:%s" i (pp_reg r)

        let pp_rval = function
          | Location_global c -> sprintf "*%s" (V.pp O.hexa c)
          | Location_reg (i,r) -> sprintf "%i:%s" i (pp_reg r)
      end
      module M = PrettyProg.Make(O)(Arch)
      module Alloc = SymbReg.Make(Arch)
      let zyva name parsed =
        let parsed = Alloc.allocate_regs parsed in
        M.dump_prog name parsed

    end

    module Z =  ToolParse.Top(T)(Text)

    open OutMode
    let zyva = match O.mode with
    | Txt ->
        let module Z =  ToolParse.Top(T)(Text) in
        Z.from_file
    | LaTeX|HeVeA|HeVeANew ->
        let module Z =  ToolParse.Top(T)(Latex) in
        Z.from_file
        
  end

(***********************)
let args = ref []
let verbose = ref 0
let texmacros = ref false
let hexa = ref false
let outputdir = ref None
let mode = ref OutMode.LaTeX
let opts =
  [
   "-v",Arg.Unit (fun () -> incr verbose), " be verbose";
   "-texmacros", Arg.Bool (fun b -> texmacros := b),
   (sprintf "<bool> use latex macros in output, default %b" !texmacros);
   "-hexa", Arg.Bool (fun b -> hexa := b),
   (sprintf "<bool> hexadecimal output, default %b" !hexa);
   begin let module P = ParseTag.Make(OutMode) in
   P.parse "-mode" mode "output mode" end ;
   "-o", Arg.String (fun s -> outputdir := Some s),
   "<name>  all output in directory <name>";
 ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mprog"

let () =
  Arg.parse opts
    (fun s -> args := !args @ [s])
    (sprintf "Usage: %s [options]* [test]*" prog)


module X =
  Top
    (struct
      let verbose = !verbose
      let texmacros = !texmacros
      let hexa = !hexa
      let outputdir = !outputdir
      let mode = !mode
    end)

let () =
  Misc.iter_argv
    (fun fname ->
      try X.zyva fname with
      | Misc.Exit -> ()
      | Misc.Fatal msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 fname msg ;
          ()
      | e ->
          Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 fname ;
          raise e)
    !args

