(*********************************************************************)
(*                        Diy                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type X = sig
  type t
  val compare : t -> t -> int
  val dump : t -> string
end

module type I = sig
  module C : Constr.S
  module V : X with type t = Constant.v
  module Loc : X with type  t = C.A.location
end

module Make (O:Indent.S) (I:I) :
    sig
      val fundef :
          (I.Loc.t -> string) -> (* For types *)
            (I.Loc.t,I.V.t) ConstrGen.cond -> unit 
      val funcall :
          I.C.constr ->
            (I.Loc.t -> string) -> (string -> string) -> string
    end = struct
      open ConstrGen

      module Switch = struct
        exception Cannot

        open ConstrGen

        type t =
          | Switch of I.Loc.t * (I.V.t * t) list * t
          | Return of bool

        module M = MyMap.Make(I.Loc)
        module VS = MySet.Make(I.V)

        let add loc v m =
          let vs = try M.find loc m with Not_found -> VS.empty in
          M.add loc (VS.add v vs) m

        let rec collect m = function
          | Atom (LV (loc,v)) -> add loc v m
          | Atom (LL (_,_)) -> raise Cannot
          | Not p -> collect m p
          | And ps|Or ps -> List.fold_left collect m ps
          | Implies (p1,p2) -> collect (collect m p1) p2

        let choose_loc m =
          let locs =
            M.fold (fun loc vs k -> ((loc,vs),VS.cardinal vs)::k) m [] in
          match locs with
          | [] -> assert false
          | p0::rem ->
              let r,_ =
                List.fold_left
                  (fun (_,cx as px) (_,cy as py) ->
                    if cx <= cy then px else py)
                  p0 rem in
              r


        let do_implies p1 p2 = match p1,p2 with
        | Or [],_ -> And []
        | And [],_ -> p2
        | _,Or [] -> Not p1
        | _,And [] -> p1
        | _,_ -> Implies (p1,p2)

        let do_or p1 p2 = match p1,p2 with
        | Or ps1,Or ps2 -> Or (ps1@ps2)
        | (p,And []) | (And [],p) -> p
        | Or ps,p -> Or (ps@[p])
        | p,Or ps -> Or (p::ps)
        | _,_ -> Or [p1;p2]

        let do_and p1 p2 = match p1,p2 with
        | And ps1,And ps2 -> And (ps1@ps2)
        | (p,Or []) | (Or [],p) -> Or []
        | And ps,p -> And (ps@[p])
        | p,And ps -> And (p::ps)
        | _,_ -> And [p1;p2]

        let do_not = function
          | Or [] -> And []
          | And [] -> Or []
          | p -> Not p

        let atom_pos loc v loc0 v0 =
          if  I.Loc.compare loc loc0 = 0 then
            Some (I.V.compare v v0 = 0)
          else None

        let atom_neg loc vs loc0 v0 =
          if  I.Loc.compare loc loc0 = 0 then
            Some (not (VS.mem v0 vs))
          else None


        let eval atom  =
          let rec eval_rec p = match p with
          | Atom (LV (loc0,v0)) ->
              begin match atom loc0 v0 with
              | None -> p
              | Some b -> if b then And [] else Or []
              end
          | Atom (LL _) -> assert false
          | Not p -> do_not (eval_rec p)
          | Or []|And [] -> p
          | Or ps -> eval_recs do_or ps
          | And ps -> eval_recs do_and ps
          | Implies (p1,p2) -> do_implies (eval_rec p1) (eval_rec p2)

          and eval_recs mk = function
            | [] -> assert false
            | [p] -> eval_rec p
            | p::ps ->
                let p1 = eval_rec p
                and p2 = eval_recs mk ps in
                mk p1 p2 in
          eval_rec

        let eval_pos loc v = eval (atom_pos loc v)
        let eval_neg loc vs = eval (atom_neg loc vs)

        let rec comp p =
          let m = collect M.empty p in
          if M.is_empty m then match p with
          | Or [] -> Return false
          | And [] -> Return true
          | _ -> assert false
          else
            let loc,vs = choose_loc m in
            let cls =
              VS.fold
                (fun v k -> (v,comp (eval_pos loc v p))::k)
                vs [] in
            let d = comp (eval_neg loc vs p) in
            Switch (loc,cls,d)

        let compile p = comp p
            
      end

      let dump  =
        let rec dump_prop p = match p with
        | Atom (LV (loc,v)) ->          
            O.fprintf "%s == %s" (I.Loc.dump loc) (I.V.dump v)
        | Atom (LL (loc1,loc2)) ->
            O.fprintf"%s == %s" (I.Loc.dump loc1) (I.Loc.dump loc2)
        | Not p ->
            O.output "!(" ;
            dump_prop p ;
            O.output ")"
        | Or [] -> O.output "0"
        | Or [p] -> dump_prop p
        | Or (p::ps) ->
            O.output "(" ;
            dump_prop p ;
            O.output ") || (" ;
            dump_prop (Or ps) ;
            O.output ")" ;
            ()
        | And [] -> O.output "1"
        | And [p] -> dump_prop p
        | And (p::ps) ->
            O.output "(" ;
            dump_prop p ;
            O.output ") && (" ;
            dump_prop (And ps) ;
            O.output ")" ;
            ()
        | Implies (p1,p2) ->
            O.output "!(" ;
            dump_prop p1 ;
            O.output ") || (" ;
            dump_prop p2 ;
            O.output ")" ;
            () in
        dump_prop

      let funname = "final_cond"


      let fundef find_type cond =
        let locs = I.C.locations cond in
        let plocs =
          List.map
            (fun loc ->
              let t = find_type loc in
              Printf.sprintf "%s %s" t (I.Loc.dump loc))
            locs in
        let vals = I.C.location_values cond in
        let pvals =
          List.map
            (fun loc -> Printf.sprintf
                "void *%s" (I.V.dump (Constant.Symbolic loc))) vals in
        O.f "inline static int %s(%s) {" funname
          (String.concat "," (plocs@pvals)) ;
        O.fprintf "%sint cond = " (Indent.as_string Indent.indent) ;
        let p = ConstrGen.prop_of cond in
        let _x = Switch.compile p in
        dump p ;
        O.output ";\n" ;
        O.oi "return cond;" ;
        O.o "}" ;
        O.o ""


      let funcall cond dump_loc dump_val =
        let locs = I.C.locations cond in
        let plocs = List.map dump_loc locs in
        let vals = I.C.location_values cond in
        let pvals = List.map dump_val vals in
        Printf.sprintf "%s(%s)" funname (String.concat "," (plocs@pvals))


    end
