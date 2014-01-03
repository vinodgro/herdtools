(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

(* Helpers *)

let parse_km opt s =  match Misc.string_of_intkm s with
| Some x -> x
| None ->
    raise
      (Arg.Bad
         (sprintf
            "wrong argument '%s'; option '%s' expects an integer argument, possibly suffixed by k or m" s opt))

let argkm opt r usage =
  opt,Arg.String (fun s -> r := parse_km opt s),
  sprintf "<n[kKmM]?> %s, default %i" usage !r

let argkm_withfun opt set usage =
  opt,Arg.String (fun s -> set (parse_km opt s)),
  sprintf "<n[kKmM]?> %s" usage

let argstringo r = Arg.String (fun s -> r := Some s)

let arginto r = Arg.Int (fun s -> r := Some s)

let argstring_withfun opt f msg = opt,Arg.String f,msg

let argstring opt r msg =
  opt,Arg.String (fun b -> r := b),
  if String.length msg > 0 && msg.[0] = '<' then
    sprintf "%s, default %s" msg !r
  else
    sprintf "<s> %s, default %s" msg !r

let argint opt  r msg =
  opt,Arg.Int (fun b -> r := b),
  sprintf "<n> %s, default %i" msg !r

let argbool opt  r msg =
  opt,Arg.Bool (fun b -> r := b),
  sprintf "<bool> %s, default %b" msg !r

let argboolo opt  r msg =
  opt,Arg.Bool (fun b -> r := Some b),
  sprintf "<bool> %s" msg

(* verbose *)
let verbose = ref 0

(* Special *)
let tar = ref None
let cross = ref false

let set_tar b  = cross := false ; tar := Some b
let set_cross b  = cross := true ; tar := Some b
let is_out () = match !tar with
| Some _ -> true
| None -> false
and get_tar () = match !tar with
| Some s -> s
| None -> "**useless**"

let logicalprocs = ref None

let set_logicalprocs s =
  try
    logicalprocs := Some (LexSplit.ints s) ;
  with LexSplit.Error ->
    raise (Arg.Bad ("bad logical processors mapping: " ^s))

(* Direct references *)
let crossrun = ref Crossrun.No
let index = ref None
let hexa = ref false
let limit = ref false
let no = ref None
let hint = ref None
let avail = ref None
let size = ref 100000
let runs = ref 10
let barrier = ref Barrier.User
let verbose_barrier = ref false
let verbose_prelude = ref None
let driver = ref Driver.Shell
let launch = ref Launch.Changing
let memory = ref Memory.Direct
let contiguous = ref false
let stride = ref 0
let preload = ref Preload.RandomPL
let collect = ref Collect.After
let safer = ref Safer.Write
let cautious = ref false
let affinity = ref Affinity.No
let force_affinity = ref false
let smtmode = ref Smt.No
let smt = ref 2
let prealloc = ref false
let speedcheck = ref Speedcheck.NoSpeed
let gcc = ref "gcc"
let cxx = ref "g++"
let linkopt = ref ""
let targetos = ref TargetOS.Linux
let gas = ref None
let set_gas b = gas := Some b
let get_numeric_labels () = match !gas with
| Some b -> b
| None ->
    begin match !targetos with
    | TargetOS.AIX -> false
    | TargetOS.Linux|TargetOS.Mac -> true
    end
let timeloop = ref (-1)
let set_timeloop i = timeloop :=  i
let kind = ref true
let names = ref []
let rename = ref None
let kinds = ref []
let set_kinds s = kinds := !kinds @ [s]
let conds = ref []
let set_conds s = conds := !conds @ [s]
let sleep = ref 0
let isync = ref false
let syncconst = 128
let syncmacro = ref (-1)
let signaling = ref false
let xy = ref false
let morearch = ref MoreArch.No
let carch = ref None

(* Arch dependent options *)
type opt =
    { delay : int; gccopts : string ;
      word : Word.t ; }

let mod_config = ref (fun cfg -> cfg)

let x86opt =
  { delay = 2048; gccopts = "-fomit-frame-pointer -O2"; word = Word.WXX; }
let ppcopt =
  { delay = 1024; gccopts = "-O2"; word = Word.WXX; }
let armopt =
  { delay = 1024; gccopts = "-O2"; word = Word.WXX; }
let copt =
  { delay = 2048; gccopts = ""; word = Word.WXX; }
let cppopt =
  { delay = 2048; gccopts = ""; word = Word.WXX; }

let get_default arch = match arch with
| `X86 -> x86opt
| `PPCGen
| `PPC -> ppcopt
| `ARM -> armopt
| `C -> copt
| `Cpp -> cppopt

let replace_config f =
  let g = !mod_config in
  mod_config := (fun cfg -> f (g cfg))

let get_dependent () = !mod_config

let set_delay i = replace_config (fun o ->  { o with delay = i; })
let get_delay opt = opt.delay

let set_gccopts opts =  replace_config (fun o ->  { o with gccopts = opts; })
let get_gccopts opt = opt.gccopts

let set_word w = replace_config (fun o ->  { o with word = w; })
let get_word opt = opt.word

let set_carch x = carch := Some x

(* More *)

let pldw = ref true
