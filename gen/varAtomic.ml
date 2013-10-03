(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Edge variations on plain/atomic/reserve *)

module type Config = sig
  val varatom : bool
  val varreserve : bool
end

module DefaultConfig =  struct
  let varatom = true
  let varreserve = false
  let varrfi = false
end


module type S = sig
  type edge

  val varatom_es : edge list list -> edge list list
  val varatom_ess : edge list list list -> edge list list list
end

module Make(Co:Config)(E:Edge.S) : S
with type edge = E.edge = struct

  open Code

  type edge = E.edge

  open E

(* Reserve *)
  let do_nothing e es k = (e::es)::k

  let do_var_reserve_tgt e es k =
    match dir_tgt e,e.a2 with
    | (Dir W,_)|(_,Reserve) -> (e::es)::k
    | (Dir R|Irr),_ -> (e::es)::({ e with a2 = Reserve; }::es)::k

  let var_reserve_tgt =
    if Co.varreserve then do_var_reserve_tgt
    else do_nothing

  let do_var_reserve_src e es k = match dir_src e,e.a1 with
    | (Dir W,_)|(_,Reserve) -> (e::es)::k
    | (Dir R|Irr),_ -> (e::es)::({ e with a1 = Reserve; }::es)::k



  let var_reserve_src =
    if Co.varreserve then do_var_reserve_src
    else do_nothing

  let do_nothing2 e1 e2 es k = (e1::e2::es)::k

  let do_var_reserve_edge e1 e2 es k =
    match dir_tgt e1,e1.a2,dir_src e2,e2.a1 with
    | (Dir R|Irr),(Plain|Atomic as x),
      (Dir R|Irr),(Plain|Atomic as y) when x=y ->
        (e1::e2::es)::
        ({ e1 with a2 = Reserve; }::{ e2 with a1 = Reserve; }::es)::
        k
    | _ -> do_nothing2 e1 e2 es k

  let var_reserve_edge =
    if Co.varreserve then do_var_reserve_edge
    else do_nothing2


(* Atomic *)
  let do_var_atom_tgt e es k =
    var_reserve_tgt { e with a2 = Plain;} es
      (do_nothing { e with a2 = Atomic;} es k)

  let var_tgt =
    if Co.varatom then  do_var_atom_tgt
    else var_reserve_tgt

  let do_var_atom_src e es k =
    var_reserve_src { e with a1 = Plain;} es
      (do_nothing { e with a1 = Atomic;} es k)

  let var_src =
    if Co.varatom then  do_var_atom_src
    else var_reserve_src

  let do_var_atom_edge e1 e2 es k =
    var_reserve_edge { e1 with a2=Plain;} {e2 with a1=Plain;} es
      (do_nothing2 { e1 with a2=Atomic;} {e2 with a1=Atomic;} es k)

  let var_edge =
    if Co.varatom then do_var_atom_edge
    else var_reserve_edge

(* Variation of composite relaxation candidate *)
    let as_cons = function
      | e::es -> e,es
      | [] -> assert false

    let rec varatom_inside = function
      | [] -> assert false
      | [e] ->  var_tgt e [] []
      | e1::(_::_ as ess) ->
          let ess = varatom_inside ess in
          List.fold_right
            (fun es k ->
              let e2,rem = as_cons es in
              var_edge e1 e2 rem k)
            ess []

    let varatom_ones es k = match es with
    | [] -> k
    | _ ->
        let ess =   varatom_inside es in
        List.fold_right
          (fun es k ->
            let e,es = as_cons es in
            var_src e es k)
          ess k

  let varatom_es es = List.fold_right varatom_ones es []

  let varatom_ess = List.map varatom_es
end
