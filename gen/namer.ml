(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Normalised names for cycles *)
open Printf

module type S = sig
  type edge
  val mk_name : string -> edge list -> string
end

module Make
    (A:Arch.S)
    (E:Edge.S with type dp = A.dp and type fence=A.fence) : S with type edge = E.edge = struct

      type edge = E.edge

      open Code
      open E

      let edge_name = function
        | Po (Same,_,_) -> Some "pos"
        | Po (Diff,_,_) -> Some "po"
        | Fenced (f,_,_,_) -> Some (String.lowercase (A.pp_fence f))
        | Dp (dp,_,_) -> Some (String.lowercase (A.pp_dp dp))
        | Rf Int -> Some "rfi"
        | Ws Int -> Some "wsi"
        | Fr Int -> Some "fri"
        | Detour e ->
            Some (sprintf "det%s" (String.lowercase (pp_extr e)))
        | DetourWs e ->
            Some (sprintf "det%sw" (String.lowercase (pp_extr e)))
        | Store -> Some "sto"
        | _ -> None

      let atom_name = function
        | Plain -> 'p'
        | Atomic -> 'a'
        | Reserve -> 'r'

      let atoms_name a1 a2 = match a1,a2 with
      | Plain,Plain -> ""
      | _ -> sprintf "%c%c" (atom_name a1) (atom_name a2)

      let one_name e = match edge_name e.edge with
      | Some n ->
          Some (sprintf "%s%s" n (atoms_name e.a1 e.a2))
      | None -> None


      let all_same = function
        | x::xs ->
            let rec do_rec = function
              | y::ys -> if x = y then do_rec ys else None
              | [] -> Some x in
            do_rec xs
        | [] -> None

      let rec count_a = function
        | {edge=(Rf Ext|Fr Ext|Ws Ext); a2=Atomic}::
          ({edge=(Rf Ext|Fr Ext|Ws Ext);a1=Atomic}::_ as es) ->
            "A"::count_a es
        | {edge=(Rf Ext|Fr Ext|Ws Ext); a2=Plain}::
          ({edge=(Rf Ext|Fr Ext|Ws Ext);a1=Plain}::_ as es) ->
            "P"::count_a es
        | _::es -> count_a es
        | [] -> []

      let init_a = function
        | {edge=(Rf Ext|Fr Ext|Ws Ext);a1=Atomic}::_ as es ->
            begin match Misc.last es with
            | {edge=(Rf Ext|Fr Ext|Ws Ext);a2=Atomic} -> ["A"]
            | _ -> []
            end
        | {edge=(Rf Ext|Fr Ext|Ws Ext);a1=Plain}::_ as es ->
            begin match Misc.last es with
            | {edge=(Rf Ext|Fr Ext|Ws Ext);a2=Plain} -> ["P"]
            | _ -> []
            end
        | _ -> []

      let isolated_writes es =
        let x =  init_a es @ count_a es in
        let x =
          if List.for_all (fun s -> s = "P") x then []
          else  x in
        String.concat "" x

(* New naming convention with '-' inbetween consecutive int edges *)
      let add_list xs xss = match xs with
      | [] -> xss
      | _  -> xs::xss

      let rec po_list es = match  es with
      | [] -> [],[]
      | e::es ->
          let xs,xss = po_list es in
          match E.get_ie e with
          | Ext -> [],add_list xs xss
          | Int -> (e::xs),xss

            
      let new_namer es =
        let xs,xss = po_list es in
        let xss = add_list xs xss in
        let xs =
          List.map
            (fun es ->
              String.concat "-"
                (List.map
                   (fun e -> match one_name e with
                   | Some s -> s
                   | None -> Warn.fatal "Namer failure") 
                   es))
            xss in
        xs


      let mk_name base es =
        let name =
          let xs = new_namer es in
          let ys = match isolated_writes es with
          | "" -> []
          | s -> [s] in
          let xs = match all_same xs,xs with
          | Some "po",_ -> ys 
          | Some "pos",[_] -> ys
          | Some _x,[_] -> xs@ys
          | Some x,_::_::_ -> (x ^ "s")::ys
          | None, _ -> xs@ys
          | Some _,[] -> assert false in        
          String.concat "+" (base::xs) in
        name
    end
