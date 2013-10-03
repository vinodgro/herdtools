(*********************************************************************)
(*                       DIY                                         *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Sort a list of litmus tests by increasing number of threads,
   or according to given order.
   Also erase duplicates identified from hashes *)

open Printf


module Top
    (Opt:
       sig
         val verbose : bool
         val duplicates : bool
         val reverse : bool
         val cost : string -> int
         val tnames : bool
       end) =
  struct

    module T = struct
      type t =
        { tname : string ;
          cost : int * int ;
          hash : string; }
    end

    module Make(A:ArchBase.S) = struct

      let default_cost pgm =
        let nprocs = List.length pgm in
        let nins =
          List.fold_left
            (fun k (_,code) -> k+A.get_naccesses code)
            0 pgm in
        nprocs,nins
          
      let zyva name parsed =
        { T.tname = name.Name.name ;
          cost = default_cost parsed.MiscParser.prog ;
          hash = MiscParser.get_hash  parsed; }
    end

    module Z = ToolParse.Top(T)(Make)

    type names = {fname:string; tname:string;}

    let do_test name k =
      try
        let {T.tname = tname;
             cost = c ;
             hash = h; } = Z.from_file name in
        let cx =
          try Opt.cost tname 
          with Not_found ->
            Warn.fatal "no cost for test %s" tname in
        let cst = (cx,c),h in
        ({fname=name; tname=tname;},cst)::k
      with
      | Misc.Exit -> k
      | Misc.Fatal msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 name msg ;
          k
      | e ->
          Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
          raise e

    let zyva tests =
      let xs = match tests with
      | [] -> Misc.fold_stdin do_test []
      | _  -> Misc.fold_argv do_test tests [] in

      let get_base f = Filename.chop_extension (Filename.basename f) in

      let bigname f =
        let b = get_base f in
        let len = String.length b in
        let rec check i =
          if i >= len then false
          else match b.[i] with
          | 'A'..'Z' -> true
          | _ -> check (i+1) in
        check 0 in


      let fname_compare f1 f2 =
        let f1 = f1.fname and f2 = f2.fname in
        match bigname f1,bigname f2 with
        | true,false -> -1
        | false,true -> 1
        | _,_ ->
            let x1 = String.length (get_base f1)
            and x2 =  String.length (get_base f2) in
            match Misc.int_compare x1 x2 with
            | 0 -> Misc.int_compare (String.length f1) (String.length f2)
            | r -> r in

      let get_min cmp = function
        | [] -> assert false
        | x::xs ->
            let rec  get_min_rec x = function
              | [] -> x
              | y::ys -> get_min_rec (if cmp x y < 0 then x else y) ys in
            get_min_rec x xs in


      let xs =
        let t = Hashtbl.create 17 in
        let see h f sz =
          try
            let _sz,fs = Hashtbl.find t h in
            assert (sz = _sz) ;
            Hashtbl.replace t h (sz,f::fs)
          with Not_found -> Hashtbl.add t h (sz,[f]) in
        List.iter (fun (f,(sz,h)) -> see h f sz) xs ;
        if Opt.verbose then begin
          Hashtbl.iter
            (fun _ (_,fs) -> match fs with
            | _::_::_ ->
                let fs = List.map (fun n -> n.fname) fs in
                eprintf "%s\n%!"
                  (String.concat " " fs)
            | _ -> ()) t      
        end ;
        if Opt.duplicates then
          Hashtbl.fold (fun _h (sz,fs) k ->
            List.fold_left
              (fun k f -> (f,sz)::k)
              k fs)
            t []
        else
          Hashtbl.fold
            (fun _h (sz,fs) k -> (get_min fname_compare fs,sz)::k) t [] in

      let do_pint_compare (i1,j1) (i2,j2) =
        match Misc.int_compare i1 i2 with
        | 0 -> Misc.int_compare j1 j2
        | r -> r in

      let do_tint_compare (i1,j1) (i2,j2) =
        match Misc.int_compare i1 i2 with
        | 0 -> do_pint_compare j1 j2
        | r -> r in

      let tint_compare l1 l2 =
        let r = do_tint_compare l1 l2 in
        if Opt.reverse then (0-r) else r in

      let xs = List.sort
          (fun (n1,l1) (n2,l2) ->
            match tint_compare l1 l2 with
            | 0 -> String.compare n1.tname n2.tname
            | r -> r) xs in

      let () =
        printf "#" ;
        for k = 0 to Array.length Sys.argv-1 do
          printf " %s" Sys.argv.(k)
        done ;
        printf "\n" ;
        List.iter
          (fun (n,(c1,(c2,c3))) ->
            let name = if Opt.tnames then n.tname else n.fname in
            if Opt.verbose then printf "#%i %i %i\n" c1 c2 c3;
            printf "%s\n" name)
          xs in
      ()

  end


let verbose = ref false
let duplicates = ref false
let arg = ref []
let orders = ref []
let reverse = ref false
let tnames = ref false
let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "msort"

let () =
  Arg.parse
    ["-v",Arg.Unit (fun () -> verbose := true), " be verbose";
     "-d",Arg.Unit (fun () -> duplicates := true)," keep duplicates";
     "-r",Arg.Unit (fun () -> reverse := true)," reverse sort";
     "-t",Arg.Unit (fun () -> tnames := true)," output test names";
     "-cost",
     Arg.String (fun s -> orders := !orders @ [s]),
     "<name> specify order file";]       
    (fun s -> arg := s :: !arg)
    (sprintf "Usage: %s [options]* [test]*" prog)

let tests = !arg

let parse_int s = try Some (int_of_string s) with _ -> None

module L = LexRename.Make(struct let verbose = if !verbose then 1 else 0 end)
let costs = L.read_from_files !orders parse_int

module X =
  Top
    (struct
      let verbose = !verbose
      let duplicates = !duplicates
      let reverse = !reverse
      let cost = match !orders with
      | [] -> fun _s -> 0
      | _  -> TblRename.find_value costs
      let tnames = !tnames
    end)

let () = X.zyva tests
