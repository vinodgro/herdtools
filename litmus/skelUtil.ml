(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type Config = sig
  val preload : Preload.t
  val mode : Mode.t
  val kind : bool
end

type stat =
    { tags : string list ; name : string ;
      max : string; tag : string;
      process : string -> string; }

(* Skeleton utilities, useful for Skel and PreSi *)

module Make
    (P:sig type code end)
    (A:Arch.Base)
    (T:Test.S with type P.code = P.code and module A = A) : sig

(* Typing utilities *)
      type env
      val build_env : T.t -> env
      val find_type : A.location -> env -> CType.t
      val select_types : (A.location -> 'a option) -> env -> ('a * CType.t) list
      val select_proc : int -> env -> (A.reg * CType.t) list
      val select_global : env -> (A.loc_global * CType.t) list
(* Locations *)
      val get_final_locs : T.t -> A.LocSet.t
      val get_final_globals : T.t -> A.LocSet.t
      val is_ptr : A.location -> env -> bool
      val ptr_in_outs : env -> T.t -> bool

(* Instructions *)
      val do_store : CType.t -> string -> string -> string
      val do_load : CType.t -> string -> string

(* Info *)
      val get_info : string -> T.t -> string option
      val get_prefetch_info : T.t -> string

(* Dump stuff *)
      module Dump : functor (Cfg:Config) -> functor (O:Indent.S) -> sig
        (* Same output as shell script in (normal) shell driver mode *)
        val prelude : Name.t -> T.t -> unit

        (* Dump results *)
        val postlude :
            Name.t -> T.t -> Affi.t option -> bool ->
              stat list -> unit
      end
    end = struct

      open Printf

      type env = CType.t A.LocMap.t

      let build_env test =
        let e = A.LocMap.empty in
        let e =
          List.fold_left
            (fun e (s,t) -> A.LocMap.add (A.Location_global s) t e)
            e test.T.globals in
        let e = 
          List.fold_left
            (fun e (proc,(_,(outs, _))) ->
              List.fold_left
                (fun e  (reg,t) ->
                  A.LocMap.add (A.Location_reg (proc,reg)) t e)
                e outs)
            e test.T.code in
        e

      let find_type loc env =
        try A.LocMap.find loc env
        with Not_found -> Compile.base

      let select_types f env =
        A.LocMap.fold
          (fun loc t k -> match f loc with
          | Some r -> (r,t)::k
          | None -> k)
          env []

      let select_proc (p:int) env =
        select_types
          (function
            | A.Location_reg (q,reg) when p = q -> Some reg
            | A.Location_global _ | A.Location_reg _ -> None)
          env

      let select_global env =
        select_types
          (function
            | A.Location_reg _ -> None
            | A.Location_global loc -> Some loc)
          env

(* Locations *)
      let get_final_locs t =
        A.LocSet.union
          (T.C.locations t.T.condition)
          (A.LocSet.of_list t.T.flocs)

      let get_final_globals t =
        A.LocSet.filter
          (function
            | A.Location_global _ -> true
            | A.Location_reg _ -> false)
          (get_final_locs t)

      let is_ptr loc env = CType.is_ptr (find_type loc env)

      let ptr_in_outs env test =
        let locs = get_final_locs test in
        A.LocSet.exists (fun loc ->is_ptr loc env ) locs

(* Instructions *)
      let do_store t loc v =
        if CType.is_atomic t then
          sprintf "atomic_store_explicit(&%s,%s,memory_order_relaxed)" loc v
        else
          sprintf "%s = %s" loc v

      let do_load t loc =
        if CType.is_atomic t then
          sprintf "atomic_load_explicit(&%s,memory_order_relaxed)" loc
        else loc


(* Info *)
      let get_info key test =
        try Some (List.assoc key test.T.info)
        with Not_found -> None

      let get_prefetch_info test = match get_info "Prefetch" test with
      | Some i -> i
      | None -> ""

(* Dump *)

      module Dump (Cfg:Config) (O:Indent.S) = struct

        open Preload

        let prelude doc test =
          O.o "#ifdef ASS" ;
          O.o "static void ass(FILE *out) { }" ;
          O.o "#else" ;
          O.f "#include \"%s\"" (MyName.outname doc.Name.file ".h") ;
          O.o "#endif" ;
          O.o "" ;
          let dstring s = O.fi "fprintf(out,\"%%s\\n\",\"%s\");"
              (String.escaped s) in
(* Static information *)
          O.o "static void prelude(FILE *out) {" ;
          let title = sprintf "%% Results for %s %%" doc.Name.file in
          let nice = String.make (String.length title) '%' in
          dstring nice ;
          dstring title ;
          dstring nice ;
          let xs = T.D.lines doc test.T.src in
          List.iter dstring xs ;
          O.oi "fprintf(out,\"Generated assembler\\n\");" ;
          O.oi "ass(out);" ;
          O.o "}" ;
          O.o "" ;
          ()

(* Postlude *)
        open ConstrGen

        let pp_atom a =
          match a with
          | LV (loc,v) ->
              sprintf "%s=%s" (A.pp_location loc) (A.V.pp_v v)
          | LL (loc1,loc2) ->
              sprintf "%s=%s" (A.pp_location loc1) (A.pp_rval loc2)

        let pp_cond c =
          String.escaped
            (ConstrGen.constraints_to_string pp_atom c)

        let pp_nstates nstates =
          O.fi "fprintf(out,\"Histogram (%%i states)\\n\",%s);" nstates

        let postlude doc test affi show_topos stats =
          begin match Cfg.mode with
          | Mode.Std ->
              O.o "static void postlude(FILE *out,cmd_t *cmd,hist_t *hist,count_t p_true,count_t p_false,tsc_t total) {"
          | Mode.PreSi ->
              O.o "static void postlude(FILE *out,global_t *g,count_t p_true,count_t p_false,tsc_t total) {" ;
              O.oi "hash_t *hash = &g->hash ;"
          end ;
(* Print header *)
          let c = test.T.condition in
          if Cfg.kind then
            O.fi "fprintf(out,\"Test %s %s\\n\") ;"
              doc.Name.name (ConstrGen.pp_kind (ConstrGen.kind_of c))
          else
            O.fi "fprintf(out,\"Test %s\\n\") ;"
              doc.Name.name ;
(* Print histogram *)
          begin match Cfg.mode with
          | Mode.Std ->
              pp_nstates "finals_outs(hist->outcomes)" ;
              O.oi "just_dump_outcomes(out,hist);"
          | Mode.PreSi ->
              pp_nstates "hash->nhash" ;
              O.oi "pp_hash(out,hash,g->group);"
          end ;
(* Print condition and witnesses *)
          if Cfg.kind then begin
            let to_check = match c with
            | ExistsState _ -> "p_true > 0"
            | ForallStates _|NotExistsState _ -> "p_true == 0" in
            O.fi "int cond = %s;" to_check ;
            O.fi "fprintf(out,\"%%s\\n\",%s);" "cond?\"Ok\":\"No\"" ;
            O.oi "fprintf(out,\"\\nWitnesses\\n\");" ;
            let fmt = "Positive: %\"PCTR\", Negative: %\"PCTR\"\\n" in
            O.fi "fprintf(out,\"%s\",%s,%s);" fmt
              (match c with
              | ExistsState _ -> "p_true"
              | NotExistsState _|ForallStates _ -> "p_false")
              (match c with
              | ExistsState _ -> "p_false"
              | NotExistsState _|ForallStates _ -> "p_true") ;
            O.fi "fprintf(out,\"Condition %s is %%svalidated\\n\",%s);"
              (pp_cond c)
              (sprintf "%s ? \"\" : \"NOT \"" to_check)
          end else begin
            O.fi "fprintf(out,\"\\nCondition %s\\n\");"
              (pp_cond c)
          end ;

(* Print meta-information *)
          List.iter
            (fun (k,vs) ->
              if k = "Relax" then
                let fmt = sprintf "Relax %s %%s %%s\\n" doc.Name.name in
                O.fi "fprintf(out,\"%s\",p_true > 0 ? \"Ok\" : \"No\",\"%s\");"
                  fmt (String.escaped vs) ;
              else if k = "Prefetch" then begin
              end else
                let fmt = "%s=%s\\n" in
                O.fi "fprintf(out,\"%s\",\"%s\",\"%s\");"
                  fmt (String.escaped k) (String.escaped vs))
            test.T.info ;
(* Prefetch shown whenever activated *)
          begin match Cfg.mode with
          | Mode.Std ->
              begin match Cfg.preload with
              | CustomPL ->
                  let fmt = "%s=" in
                  O.fi "fprintf(out,\"%s\",\"%s\");" fmt "Prefetch" ;
                  O.oi "prefetch_dump(out,cmd->prefetch);" ;
                  O.oi "putc('\\n',out);"
              | StaticPL|StaticNPL _ ->
                  let fmt = "%s=%s\\n" in
                  let prf = get_prefetch_info test in
                  O.fi "fprintf(out,\"%s\",\"%s\",\"%s\");" fmt "Prefetch" prf
              | NoPL|RandomPL -> ()
              end
          | Mode.PreSi -> ()
          end ;
(* Affinity info, as computed *)
          begin match Cfg.mode with
          | Mode.Std ->
              begin match affi with
              | Some affi ->
                  O.oi "if (cmd->aff_mode == aff_custom) {" ;
                  let fmt = "%s=%s\\n" in
                  O.fii "fprintf(out,\"%s\",\"%s\",\"%s\");"
                    fmt "Affinity" (Affi.pp affi) ;
                  O.oi "}"
              | None -> ()
              end
          | Mode.PreSi -> ()
          end ;
(* Observation summary *)
          O.fi
            "count_t cond_true = %s;"
            (match test.T.condition with
            | ExistsState _|NotExistsState _ -> "p_true"
            | ForallStates _ -> "p_false") ;
          O.fi
            "count_t cond_false = %s;"
            (match test.T.condition with
            | ExistsState _|NotExistsState _ -> "p_false"
            | ForallStates _ -> "p_true") ;
          let fmt =
            sprintf
              "Observation %s %%s %%\"PCTR\" %%\"PCTR\"\\n"
              doc.Name.name  in
          let obs = 
            "!cond_true ? \"Never\" : !cond_false ? \"Always\" : \"Sometimes\"" in
          O.fi "fprintf(out,\"%s\",%s,cond_true,cond_false) ;" fmt obs;
(* Topologies sumaries *)
          begin match Cfg.mode with
          | Mode.Std ->
              if show_topos then begin
                O.oi "if (cmd->aff_mode == aff_scan) {" ;
                O.oii "for (int k = 0 ; k < SCANSZ ; k++) {" ;
                O.oiii "count_t c = ngroups[k];" ;
                let fmt = "\"Topology %-6\" PCTR\":> %s\\n\"" in
                O.fiii "if (c > 0) { printf(%s,c,group[k]); }" fmt ;
                O.oii "}" ;
                O.oi "} else if (cmd->aff_mode == aff_topo) {"  ;
                O.oii "printf(\"Topology %-6\" PCTR \":> %s\\n\",ngroups[0],cmd->aff_topo);" ;
                O.oi "}" 
              end
          | Mode.PreSi ->
              O.oi "count_t *ngroups = &g->stats.groups[0];" ;
              O.oi "for (int k = 0 ; k < SCANSZ ; k++) {" ;
              O.oii "count_t c = ngroups[k];" ;
              let fmt = "\"Topology %-6\" PCTR\":> part=%i %s \\n\"" in
              O.fii "if (c > 0) printf(%s,c,k,g->group[k]);" fmt ;
              O.oi "}"
          end ;
(* Other stats *)
          List.iter
            (fun {tags; name; max; tag; process; } ->
              let ks = Misc.interval 0 (List.length tags) in
              let rec loop_rec i = function
                | [] ->
                    O.fx i "{" ;
                    let j = Indent.tab i in
                    O.fx j "count_t c = g->stats.%s%s;" name
                      (String.concat ""
                         (List.map (sprintf "[k%i]") ks))  ;
                    let fmt =
                      sprintf "%s %%-6\"PCTR\":> {%s}\\n"
                        tag
                        (String.concat ", "
                           (List.map (sprintf "%s=%%i") tags))
                    and args =
                      String.concat ","
                        (List.map
                           (fun k -> process (sprintf "k%i" k))
                           ks) in
                    O.fx j "if ((c*100)/p_true >= 20) fprintf(out,\"%s\",c,%s);"
                      fmt args ;
                    O.fx i "}"
                | k::ks ->
                    let i = Indent.tab i in
                    O.fx i "for (int k%i = 0 ; k%i < %s; k%i++)"
                      k k max k ;
                    loop_rec i ks in
              loop_rec Indent.indent0 ks)
            stats ;
(* Show running time *)
          let fmt = sprintf "Time %s %%.2f\\n"  doc.Name.name in
          O.fi "fprintf(out,\"%s\",total / 1000000.0) ;" fmt ;
          O.oi "fflush(out);" ;
          O.o "}" ;
          O.o "" ;
          ()

      end
    end
