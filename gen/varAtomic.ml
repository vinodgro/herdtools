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

module type S = sig
  type edge

  val var_both : edge ->  edge list
  val varatom_es : edge list list -> edge list list
  val varatom_ess : edge list list list -> edge list list list
end

module Make(E:Edge.S) : S
with type edge = E.edge = struct


  type edge = E.edge

  open E

(* Variations *)
  let var_src e es =
    E.fold_atomo
      (fun ao k -> ({ e with a1=ao }::es)::k)
      []

  let var_tgt e es =
    E.fold_atomo
      (fun ao k -> ({ e with a2=ao }::es)::k)
      []

  let var_edge e1 e2 es =
    E.fold_atomo
      (fun ao k ->
        ({ e1 with a2 = ao}::{ e2 with a1 = ao}::es)::k)
      []

  let var_both e =
     E.fold_atomo
      (fun ao1 k ->
        E.fold_atomo
          (fun ao2 k -> { e with a1=ao1; a2=ao2; }::k)
          k)
      []
(* Variation of composite relaxation candidate *)
    let as_cons = function
      | e::es -> e,es
      | [] -> assert false

    let rec varatom_inside = function
      | [] -> assert false
      | [e] ->  var_tgt e []
      | e1::(_::_ as ess) ->
          let ess = varatom_inside ess in
          List.fold_right
            (fun es k ->
              let e2,rem = as_cons es in
              var_edge e1 e2 rem@k)
            ess []

    let varatom_ones es k = match es with
    | [] -> k
    | _ ->
        let ess =   varatom_inside es in
        List.fold_right
          (fun es k ->
            let e,es = as_cons es in
            var_src e es@k)
          ess k

  let varatom_es es = List.fold_right varatom_ones es []

  let varatom_ess = List.map varatom_es
end
