(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


open Printf
open LogState

let verbose = ref 0
let logs = ref []

let options =
  [
  
  ("-q", Arg.Unit (fun _ -> verbose := -1),
   "<non-default> be silent");  
  ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
  ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mlog2cond"

let () =
  Arg.parse options
    (fun s -> logs := !logs @ [s])
    (sprintf "Usage %s [options]* [log]
log is a log file names.
Options are:" prog)

let verbose = !verbose
let log = match !logs with
| [log;] -> Some log
| [] -> None
| _ ->
    eprintf "%s takes one argument\n" prog ;
    exit 2

module Verbose = struct let verbose = verbose end

let do_rename name = name
let select_name = fun _ -> true

module LS = LogState.Make(Verbose)
module LL =
  LexLog.Make
    (struct
      let verbose = verbose
      let rename = do_rename
      let ok = select_name
    end)


let zyva log =
  let test = match log with
  | None -> LL.read_chan "stdin" stdin
  | Some log -> LL.read_name log in
  
(* Dumping of condition *)
  let pp_cond name bdss =
    let pp =
      List.map
        (fun bds ->
          String.concat " /\\ "
            (List.map
               (fun (loc,v) -> sprintf "%s=%s" loc v)
               bds))
        bdss in
    let pp = List.map (sprintf "(%s)") pp in
    let pp = String.concat " \\/ "  pp in
    sprintf "%s \"exists %s\"" name pp in

  let dump_test chan t =
    let bdss = LS.get_bindings t.states in
    match bdss with
    | [] -> ()
    | _ ->
        fprintf chan "%s\n" (pp_cond t.tname bdss) in

  let dump_log chan =  Array.iter (dump_test chan) in

  dump_log stdout test.tests ;
  flush stdout ;
  ()

let () =
  try zyva log
  with Misc.Fatal msg ->
    eprintf "Fatal error: %s\n%!" msg

