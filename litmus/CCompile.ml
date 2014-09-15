(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type Config = sig
  val numeric_labels : bool
  val signaling : bool
  val timeloop : int
  val barrier : Barrier.t
end

module Make
    (O:Config)
    (T:Test.S with
     type P.code = string CAst.t and
     type A.reg = string and
     type A.loc_reg = string and
     type A.Out.t = CTarget.t) =
  struct

    module A = T.A
    module C = T.C
    module Generic = Compile.Generic(A)

    type t =
      | Test of A.Out.t
      | Global of string

    let rec compat t1 t2 =
      let open CType in
      match t1,t2 with
      | Base s1,Base s2 -> s1 = s2
      | (Volatile t1|Atomic t1),_ -> compat t1 t2
      | _,(Volatile t2|Atomic t2) -> compat t1 t2
      | Pointer t1,Pointer t2 -> compat t1 t2
      | _,_ -> false

    let add_param {CAst.param_ty; param_name} env =
      let ty = CType.strip_volatile param_ty in
      try
        let oty = StringMap.find param_name env in
        if compat oty ty then StringMap.add param_name ty env
        else begin
          Warn.warn_always
            "Parameter %s, type mismatch %s vs. %s\n"
            param_name (CType.dump oty) (CType.dump ty) ;
          env
        end
      with Not_found ->
        StringMap.add param_name ty env

    let add_params = List.fold_right add_param

    let comp_globals init code =
      let env =
        List.fold_right
          (fun (loc,v) env ->
            let env = Generic.add_value v env in
            match loc with
            | A.Location_global (a) ->
                Generic.add_addr_type a (Generic.typeof v) env
            | _ -> env)
          init StringMap.empty in
      let env =
        List.fold_right
          (function
            | CAst.Test {CAst.params; _} -> add_params params
            | _ -> Misc.identity
          )
          code
          env
      in
      StringMap.fold
        (fun a ty k -> (a,ty)::k)
        env []

    let get_local proc f acc = function
      | A.Location_reg (p, name) when p = proc -> f name acc
      | A.Location_reg _
      | A.Location_global _ -> acc

    let get_locals proc =
      let f acc (x, ty) = get_local proc (fun x -> Misc.cons (x, ty)) acc x in
      List.fold_left f []

     let string_of_params =
       let f {CAst.param_name; param_ty; } = param_name,param_ty in
       List.map f

    let comp_template proc init final code =
      let inputs = string_of_params code.CAst.params in
      {
        CTarget.inputs ;
        finals=final ;
        code = code.CAst.body; }

    let get_reg_from_loc = function
      | A.Location_reg (_, reg) -> reg
      | A.Location_global reg -> reg

    let locations p final flocs =
      let locations_atom reg acc = match reg with
        | ConstrGen.LV (loc, _) ->
            let reg = get_reg_from_loc loc in
            A.LocMap.add loc (Generic.type_in_final p reg final flocs) acc
        | ConstrGen.LL (loc1, loc2) ->
            let reg1 = get_reg_from_loc loc1 in
            let reg2 = get_reg_from_loc loc2 in
            A.LocMap.add
              loc1
              (Generic.type_in_final p reg1 final flocs)
              (A.LocMap.add loc2 (Generic.type_in_final p reg2 final flocs) acc)
      in
      let locations_flocs acc = function
        | (x, MiscParser.Ty s) -> A.LocMap.add x (CType.Base s) acc
        | (x, MiscParser.Pointer s) ->
            A.LocMap.add x (CType.Pointer (CType.Base s)) acc
      in
      let locs = ConstrGen.fold_constr locations_atom final A.LocMap.empty in
      let locs = List.fold_left locations_flocs locs flocs in
      A.LocMap.bindings locs

    let comp_code final init flocs procs =
      List.fold_left
        (fun acc -> function
           | CAst.Test code ->
               let proc = code.CAst.proc in
               let regs = get_locals proc (locations proc final flocs) in
               let final = List.map fst regs in
               let volatile = []
(*
                 let f acc = function
                   | {CAst.volatile = true; param_name; _} -> param_name :: acc
                   | {CAst.volatile = false; _} -> acc
                 in
                 List.fold_left f [] code.CAst.params *)
               in
               acc @ [(proc, (comp_template proc init final code, (regs, volatile)))]
           | CAst.Global _ -> acc
        )
        [] procs

    let get_global_code =
      let f acc = function
        | CAst.Global x -> acc @ [x]
        | CAst.Test _ -> acc
      in
      List.fold_left f []

    let compile t =
      let
        { MiscParser.init = init ;
          info = info;
          prog = code;
          condition = final;
          locations = locs ; _
        } = t in
      { T.init = init;
        info = info;
        code = comp_code final init locs code;
        condition = final;
        globals = comp_globals init code;
        flocs = List.map fst locs;
        global_code = get_global_code code;
        src = t;
      }

  end
