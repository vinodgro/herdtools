(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

(*****************************)
(* General purpose utilities *)
(*****************************)

let default xo y = match xo with
  | None -> y
  | Some x -> x

let somes xs = List.map (fun x -> Some x) xs

let the = function None -> assert false | Some x -> x

let (!!) opt_ref = the (!opt_ref)

let optional xs = None :: somes xs

let foreach xr vs f = List.iter (fun v -> xr := v; f) vs

let all_families = ["MP";"3LB";"IRIW"]

(************************)
(* Regions              *)
(************************)

type region = Global | Local | Both

let all_regions = [Global; Local; Both]
let single_regions = [Global; Local]

let parse_regions = function
  | "global" -> Global
  | "local" -> Local
  | "both" -> Both
  | x -> raise (Arg.Bad (sprintf "Bad region: %s." x))

let parse_region = function
  | "global" -> Global
  | "local" -> Local
  | x -> raise (Arg.Bad (sprintf "Bad region: %s." x))

let mk_region_arg r k see =
  let usage = "<global|local>" in 
  (k, Arg.String (fun v -> see k; r := Some (parse_region v)), usage)

let mk_regions_arg r k see =
  let usage = "<global|local|both>" in 
  (k, Arg.String (fun v -> see k; r := Some (parse_regions v)), usage)

let pp_region = function
  | Global -> "global"
  | Local -> "local"
  | Both -> "both"

let pp_region_short = function
  | Global -> "G"
  | Local -> "L"
  | Both -> "GL"

let pp_region_flag = function
  | Global -> "CLK_GLOBAL_MEM_FENCE"
  | Local -> "CLK_LOCAL_MEM_FENCE"
  | Both -> "CLK_GLOBAL_MEM_FENCE|CLK_LOCAL_MEM_FENCE"

(************************)
(* Atomicity            *)
(************************)

let all_atomicities = [true; false]

let mk_bool_arg r k see =
  let usage = "<true|false>" in
  (k, Arg.Bool (fun b -> see k; r := Some b), usage)

let pp_atomicity = function
  | true -> "atomic_"
  | false -> ""

let pp_atomicity_short = function
  | true -> "a"
  | false -> "p"

(************************)
(* Access modes         *)
(************************)

type mode = NA | Rlx | Acq | Rel | AR | SC 
          | Bar (* Encode barrier as one mode for a fence *)

let all_modes = [NA; Rlx; Acq; Rel; AR; SC; Bar]
let load_modes = [NA; Rlx; Acq; SC]
let store_modes = [NA; Rlx; Rel; SC]
let fence_modes = [Rlx; Acq; Rel; AR; SC; Bar]

let parse_mode = function
  | "NA" | "na" -> NA
  | "Rlx" | "rlx" -> Rlx
  | "Acq" | "acq" -> Acq
  | "Rel" | "rel" -> Rel
  | "AR" | "ar" -> AR
  | "SC" | "sc" -> SC
  | x -> raise (Arg.Bad (sprintf "Bad mode: %s." x))

let parse_mode_or_bar = function
  | "Bar" | "bar" -> Bar
  | x -> parse_mode x

let mk_mode_arg r k see =
  let usage = "<na|rlx|acq|rel|ar|sc>" in 
  (k, Arg.String (fun v -> see k; r := Some (parse_mode v)), usage) 

let mk_mode_or_bar_arg r k see =
  let usage = "<na|rlx|acq|rel|ar|sc|bar>" in 
  (k, Arg.String (fun v -> see k; r := Some (parse_mode_or_bar v)), usage) 

let pp_mode_short = function
  | NA -> "na"
  | Rlx -> "rlx"
  | Acq -> "acq"
  | Rel -> "rel"
  | AR -> "ar"
  | SC -> "sc"
  | Bar -> "bar"

let pp_mode_long = function
  | Rlx -> "memory_order_relaxed"
  | Acq -> "memory_order_acquire"
  | Rel -> "memory_order_release"
  | AR -> "memory_order_acq_rel"
  | SC -> "memory_order_seq_cst"
  | NA | Bar -> assert false

let is_valid_access_mode mode atomic = 
   atomic || (mode = NA)

(************************)
(* Scope annotations    *)
(************************)

type scope = S_wi | S_wg | S_dev | S_all

let valid_scopes = [S_wg; S_dev; S_all]
let interesting_scopes = [S_wg; S_dev]

let interesting_scopes_for mode =
  if mode = NA then [None] else optional interesting_scopes

let parse_scope = function
  | "wi" -> S_wi
  | "wg" -> S_wg
  | "dev" -> S_dev
  | "all" -> S_all
  | x -> raise (Arg.Bad (sprintf "Bad scope: %s." x))

let mk_scope_arg r k see =
  let usage = "<wi|wg|dev|all>" in 
  (k, Arg.String (fun v -> see k; r := Some (parse_scope v)), usage) 

let pp_scope_short = function
  | S_wi -> "wi"
  | S_wg -> "wg"
  | S_dev -> "dev"
  | S_all -> "all"

let pp_oscope = function
  | None -> ""
  | Some scope -> sprintf "-%s" (pp_scope_short scope)

let pp_scope_long = function
  | S_wi -> assert false
  | S_wg -> "memory_scope_work_group"
  | S_dev -> "memory_scope_device"
  | S_all -> "memory_scope_all_svm_devices"

let is_valid_scope oscope = function
  | None | Some Global | Some Both -> true
  | Some Local -> 
    begin match oscope with
      | None | Some S_wg -> true
      | _ -> false
    end

let default_scope = function
  | Local -> S_wg
  | Global | Both -> S_all

(*********************************)
(* Handling command-line options *)
(*********************************)

let runherd = ref false
let herdargs = ref []
let generate_all = ref false
let region_x : region option ref = ref None
let atomic_x : bool option ref = ref None
let region_y : region option ref = ref None
let atomic_y : bool option ref = ref None
let region_z : region option ref = ref None
let atomic_z : bool option ref = ref None
let load_mode : mode option ref = ref None
let load_scope : scope option ref = ref None
let loadx_mode : mode option ref = ref None
let loadx_scope : scope option ref = ref None
let loady_mode : mode option ref = ref None
let loady_scope : scope option ref = ref None
let loadx1_mode : mode option ref = ref None
let loadx1_scope : scope option ref = ref None
let loady1_mode : mode option ref = ref None
let loady1_scope : scope option ref = ref None
let loadx2_mode : mode option ref = ref None
let loadx2_scope : scope option ref = ref None
let loady2_mode : mode option ref = ref None
let loady2_scope : scope option ref = ref None
let loadz_mode : mode option ref = ref None
let loadz_scope : scope option ref = ref None
let store_mode : mode option ref = ref None
let store_scope : scope option ref = ref None
let storex_mode : mode option ref = ref None
let storex_scope : scope option ref = ref None
let storey_mode : mode option ref = ref None
let storey_scope : scope option ref = ref None
let storez_mode : mode option ref = ref None
let storez_scope : scope option ref = ref None
let fence1_regions : region option ref = ref None
let fence1_mode : mode option ref = ref None
let fence1_scope : scope option ref = ref None
let fence2_regions : region option ref = ref None
let fence2_mode : mode option ref = ref None
let fence2_scope : scope option ref = ref None
let fence3_regions : region option ref = ref None
let fence3_mode : mode option ref = ref None
let fence3_scope : scope option ref = ref None
let scopetree : int option ref = ref None

let usage_msg = 
  let prog = Sys.argv.(0) in
  sprintf "Usage: %s <%s> [options]" 
    prog (String.concat "|" all_families)

let parse_options parse_scopetree scopetree_usage spec =
  let seen = ref [] in
  let see k = seen := k :: !seen in
  let general_options = [
    ("-all",
     Arg.Set generate_all,
     "Automatically generate all tests in this family!");
    ("-herd", 
     Arg.Rest (fun arg -> runherd := true; herdargs := arg :: (!herdargs)), 
     "[options] Run herd with the given options");
    ("-scopetree",
     Arg.String (fun v -> see "-scopetree"; scopetree := Some (parse_scopetree v)),
     scopetree_usage);
  ] in
  let mk_arg (arg_name, mk, _) = mk arg_name see in 
  Arg.current := 1; (* Skip first argument *)
  Arg.parse (general_options @ List.map mk_arg spec)
    (fun x -> raise (Arg.Bad (sprintf "Unlabelled value: %s." x)))
    usage_msg;
  let check_mandatory_arg arg_name =
    if (not (List.mem arg_name !seen)) then
      raise (Arg.Bad (sprintf "Missing %s." arg_name))
  in
  let mandatory_args_of spec = 
    List.map (fun (arg_name, _, _) -> arg_name) 
      (List.filter (fun (_, _, mand) -> mand) spec)
  in
  List.iter check_mandatory_arg 
    (mandatory_args_of spec @ ["-scopetree"])

let check arg_name b =
  let msg = sprintf "Bad %s argument." arg_name in
  if not b then raise (Arg.Bad msg)

(************************************************)
(* Sending tests to herd, writing them to files *)
(************************************************)

let write_file filename file_contents =
  if Sys.file_exists filename then begin
    eprintf "Warning: Overwriting %s.\n" filename
    (* ; exit 0 *)
  end;
  let oc = open_out filename in
  fprintf oc "%s" file_contents;
  close_out oc

let handle_test family test_name file_contents =
  let filename = sprintf "%s/%s.litmus" family test_name in
  match (!runherd), (!generate_all) with
  | true, true ->
    raise (Arg.Bad "Can't have -all and -herd options together.")
  | true, false ->
    write_file filename file_contents;
    let herdargs = String.concat " " (List.rev (!herdargs)) in
    let full_filename = "testsuite/OpenCLTests/" ^ filename in
    let herd_cmd = 
      sprintf "cd ../.. && ./herd %s %s" herdargs full_filename in
    ignore (Sys.command herd_cmd)  
  | false, true ->
    write_file filename file_contents
  | false, false ->
    write_file filename file_contents;
    printf "%s\n" file_contents


(************************)
(* Instructions         *)
(************************)

let pp_load id mode oscope = match mode with
  | NA -> 
    if oscope != None then raise (Arg.Bad "Spurious load scope given.");
    sprintf "*%s" id
  | SC when oscope = None -> 
    sprintf "atomic_load(%s)" id
  | Rlx | Acq | Rel | AR | SC -> 
    begin match oscope with 
      | None -> 
        sprintf "atomic_load_explicit(%s,%s)" 
          id (pp_mode_long mode)
      | Some scope ->
        sprintf "atomic_load_explicit(%s,%s,%s)" 
          id (pp_mode_long mode) (pp_scope_long scope)
    end
  | Bar -> assert false

let pp_store id exp mode oscope = match mode with
  | NA -> 
    if oscope != None then raise (Arg.Bad "Spurious store scope given.");
    sprintf "*%s = %s" id exp
  | SC when oscope = None -> 
    sprintf "atomic_store(%s,%s)" id exp
  | Rlx | Acq | Rel | AR | SC ->
    begin match oscope with
      | None ->
        sprintf "atomic_store_explicit(%s,%s,%s)" 
          id exp (pp_mode_long mode)
      | Some scope ->
        sprintf "atomic_store_explicit(%s,%s,%s,%s)" 
          id exp (pp_mode_long mode) (pp_scope_long scope) 
    end
  | Bar -> assert false

let pp_fence oregion omode oscope = match oregion with
  | None -> 
    if omode != None then 
      raise (Arg.Bad "Spurious fence mode given.");
    if oscope != None then 
      raise (Arg.Bad "Spurious fence scope given.");
    ""
  | Some region -> 
    let mode = default omode SC in
    match mode with
    | Bar ->
      let scope = default oscope S_wg in
      sprintf "work_group_barrier(%s,%s);\n  " 
        (pp_region_flag region) (pp_scope_long scope)
    | _ ->
      let scope = default oscope (default_scope region) in
      sprintf "atomic_work_item_fence(%s,%s,%s);\n  " 
        (pp_region_flag region) (pp_mode_long mode) (pp_scope_long scope)

let pp_fence_short prefix oregion omode oscope = match oregion with
  | None -> ""
  | Some region ->
    let mode = default omode SC in
    match mode with
    | Bar ->
      let scope = default oscope S_wg in
      sprintf "_%sB%s-%s"
        prefix
        (pp_region_short region)
        (pp_scope_short scope)
    | _ ->    
      let scope = default oscope (default_scope region) in
      sprintf "_%sF%s-%s-%s" 
        prefix
        (pp_region_short region) 
        (pp_mode_short mode) 
        (pp_scope_short scope)  

let gen_mp () =

  let all_scopetrees = [0;1] in

  let parse_scopetree = function
    | "P0_P1" -> 0
    | "P0__P1" -> 1
    | x -> raise (Arg.Bad (sprintf "Bad scopetree: %s." x)) in
  
  let scopetree_usage = "<P0_P1|P0__P1>" in

  let pp_scopetree = function
    | 0 -> "device (work_group P0 P1)"
    | 1 -> "device (work_group P0) (work_group P1)"
    | _ -> assert false in

  let pp_scopetree_short = function
    | 0 -> "P0_P1"
    | 1 -> "P0__P1"
    | _ -> assert false in

  parse_options parse_scopetree scopetree_usage [
    ("-region_x", mk_region_arg region_x, true);
    ("-atomic_x", mk_bool_arg atomic_x, true);
    ("-region_y", mk_region_arg region_y, true);
    ("-atomic_y", mk_bool_arg atomic_y, true);
    ("-load_mode", mk_mode_arg load_mode, true);
    ("-load_scope", mk_scope_arg load_scope, false);
    ("-store_mode", mk_mode_arg store_mode, true);
    ("-store_scope", mk_scope_arg store_scope, false);
    ("-fence1_regions", mk_regions_arg fence1_regions, false);
    ("-fence1_mode", mk_mode_or_bar_arg fence1_mode, false);
    ("-fence1_scope", mk_scope_arg fence1_scope, false);
    ("-fence2_regions", mk_regions_arg fence2_regions, false);
    ("-fence2_mode", mk_mode_or_bar_arg fence2_mode, false);
    ("-fence2_scope", mk_scope_arg fence2_scope, false);
  ];

  let generate () =
    check "-load_mode" (is_valid_access_mode !!load_mode !!atomic_y);
    check "-store_mode" (is_valid_access_mode !!store_mode !!atomic_y);
    check "-load_scope" (is_valid_scope !load_scope !region_y);
    check "-store_scope" (is_valid_scope !store_scope !region_y);
    check "-fence1_scope" (is_valid_scope !fence1_scope !fence1_regions);
    check "-fence2_scope" (is_valid_scope !fence2_scope !fence2_regions);

    let test_name = sprintf 
        "MP_%s%s%s_%s%s%s_x%s%s_y%s%s_%s" 
        (pp_mode_short !!store_mode)
        (pp_oscope !store_scope)
        (pp_fence_short "1" !fence1_regions !fence1_mode !fence1_scope)
        (pp_mode_short !!load_mode)
        (pp_oscope !load_scope)
        (pp_fence_short "2" !fence2_regions !fence2_mode !fence2_scope)
        (pp_atomicity_short !!atomic_x)
        (pp_region_short !!region_x)
        (pp_atomicity_short !!atomic_y)
        (pp_region_short !!region_y)
        (pp_scopetree_short !!scopetree) in
    let file_contents = sprintf 
"OpenCL %s
                        
{ 
  [x]=0;
  [y]=0;
}

P0 (%s %sint* x, %s %sint* y) {
  %s;
  %s%s;
}

P1 (%s %sint* x, %s %sint* y) {
  int r0 = %s;
  %sint r1 = %s;
}

scopeTree
(%s)
exists (1:r0=1 /\\ 1:r1=0)"
      test_name
      (pp_region !!region_x)
      (pp_atomicity !!atomic_x)
      (pp_region !!region_y)
      (pp_atomicity !!atomic_y)
      (pp_store "x" "1" NA None)
      (pp_fence !fence1_regions !fence1_mode !fence1_scope)
      (pp_store "y" "1" !!store_mode !store_scope)
      (pp_region !!region_x)
      (pp_atomicity !!atomic_x)
      (pp_region !!region_y)
      (pp_atomicity !!atomic_y)
      (pp_load "y" !!load_mode !load_scope)
      (pp_fence !fence2_regions !fence2_mode !fence2_scope)
      (pp_load "x" NA None)
      (pp_scopetree !!scopetree)
    in
    handle_test "MP" test_name file_contents
  in

  if (!generate_all) then
    foreach region_x (somes single_regions) (
    foreach atomic_x (somes all_atomicities) (
    foreach region_y (somes single_regions) (
    foreach atomic_y (somes all_atomicities) (
    let valid_load_modes, valid_store_modes =
      if !!atomic_y then load_modes, store_modes else [NA], [NA] 
    in
    foreach load_mode (somes valid_load_modes) (
    foreach load_scope (interesting_scopes_for !!load_mode) (
    foreach store_mode (somes valid_store_modes) (
    foreach store_scope (interesting_scopes_for !!store_mode) (
    foreach fence1_regions (optional all_regions) (
    let valid_fence1_modes, valid_fence1_scopes = 
      match !fence1_regions with 
      | None -> [None], [None]
      | _ -> somes fence_modes, somes interesting_scopes 
    in
    foreach fence1_mode valid_fence1_modes (
    foreach fence1_scope valid_fence1_scopes (
    foreach fence2_regions (optional all_regions) (
    let valid_fence2_modes, valid_fence2_scopes = 
      match !fence2_regions with 
      | None -> [None], [None] 
      | _ -> somes fence_modes, somes interesting_scopes 
    in
    foreach fence2_mode valid_fence2_modes (
    foreach fence2_scope valid_fence2_scopes (
    foreach scopetree (somes all_scopetrees) (
      generate ()
    )))))))))))))))
  else
    generate ()

let gen_3lb () =

  let all_scopetrees = [0;1;2] in

  let parse_scopetree = function
    | "P0_P1_P2" -> 0
    | "P0_P1__P2" -> 1
    | "P0__P1__P2" -> 2
    | x -> raise (Arg.Bad (sprintf "Bad scopetree: %s." x)) in

  let scopetree_usage = "<P0_P1_P2|P0_P1__P2|P0__P1__P2>" in
  
  let pp_scopetree = function
    | 0 -> "device (work_group P0 P1 P2)"
    | 1 -> "device (work_group P0 P1) (work_group P2)"
    | 2 -> "device (work_group P0) (work_group P1) (work_group P2)"
    | _ -> assert false in

  let pp_scopetree_short = function
    | 0 -> "P0_P1_P2"
    | 1 -> "P0_P1__P2"
    | 2 -> "P0__P1__P2"
    | _ -> assert false in

  parse_options parse_scopetree scopetree_usage [
    ("-region_x", mk_region_arg region_x, true);
    ("-atomic_x", mk_bool_arg atomic_x, true);
    ("-region_y", mk_region_arg region_y, true);
    ("-atomic_y", mk_bool_arg atomic_y, true);
    ("-region_z", mk_region_arg region_z, true);
    ("-atomic_z", mk_bool_arg atomic_z, true);
    ("-loadx_mode", mk_mode_arg loadx_mode, true);
    ("-loadx_scope", mk_scope_arg loadx_scope, false);
    ("-loady_mode", mk_mode_arg loady_mode, true);
    ("-loady_scope", mk_scope_arg loady_scope, false);
    ("-loadz_mode", mk_mode_arg loadz_mode, true);
    ("-loadz_scope", mk_scope_arg loadz_scope, false);
    ("-storex_mode", mk_mode_arg storex_mode, true);
    ("-storex_scope", mk_scope_arg storex_scope, false);
    ("-storey_mode", mk_mode_arg storey_mode, true);
    ("-storey_scope", mk_scope_arg storey_scope, false);
    ("-storez_mode", mk_mode_arg storez_mode, true);
    ("-storez_scope", mk_scope_arg storez_scope, false);
    ("-fence1_regions", mk_regions_arg fence1_regions, false);
    ("-fence1_mode", mk_mode_or_bar_arg fence1_mode, false);
    ("-fence1_scope", mk_scope_arg fence1_scope, false);
    ("-fence2_regions", mk_regions_arg fence2_regions, false);
    ("-fence2_mode", mk_mode_or_bar_arg fence2_mode, false);
    ("-fence2_scope", mk_scope_arg fence2_scope, false);
    ("-fence3_regions", mk_regions_arg fence3_regions, false);
    ("-fence3_mode", mk_mode_or_bar_arg fence3_mode, false);
    ("-fence3_scope", mk_scope_arg fence3_scope, false);
  ];

  let generate () =
    let test_name = sprintf 
        "3LB_%s%s%s-%s%s_%s%s%s-%s%s_%s%s%s-%s%s_x%s%s_y%s%s_z%s%s_%s" 
        (pp_mode_short !!loadx_mode)
        (pp_oscope !loadx_scope)
        (pp_fence_short "1" !fence1_regions !fence1_mode !fence1_scope)
        (pp_mode_short !!storey_mode)
        (pp_oscope !storey_scope)

        (pp_mode_short !!loady_mode)
        (pp_oscope !loady_scope)
        (pp_fence_short "2" !fence2_regions !fence2_mode !fence2_scope)
        (pp_mode_short !!storez_mode)
        (pp_oscope !storez_scope)

        (pp_mode_short !!loadz_mode)
        (pp_oscope !loadz_scope)
        (pp_fence_short "3" !fence3_regions !fence3_mode !fence3_scope)
        (pp_mode_short !!storex_mode)
        (pp_oscope !storex_scope)

        (pp_atomicity_short !!atomic_x)
        (pp_region_short !!region_x)
        (pp_atomicity_short !!atomic_y)
        (pp_region_short !!region_y)
        (pp_atomicity_short !!atomic_z)
        (pp_region_short !!region_z)
        (pp_scopetree_short !!scopetree) in
    let file_contents = sprintf 
"OpenCL %s
                        
{ 
  [x]=0;
  [y]=0;
  [z]=0;
}

P0 (%s %sint* x, %s %sint* y, %s %sint* z) {
  int r0 = %s;
  %s%s;
}

P1 (%s %sint* x, %s %sint* y, %s %sint* z) {
  int r1 = %s;
  %s%s;
}

P2 (%s %sint* x, %s %sint* y, %s %sint* z) {
  int r2 = %s;
  %s%s;
}

scopeTree
(%s)
exists (0:r0=1 /\\ 1:r1=1 /\\ 2:r2=1)"
      test_name
      (pp_region !!region_x) (pp_atomicity !!atomic_x)
      (pp_region !!region_y) (pp_atomicity !!atomic_y)
      (pp_region !!region_z) (pp_atomicity !!atomic_z)
      (pp_load "x" !!loadx_mode !loadx_scope)
      (pp_fence !fence1_regions !fence1_mode !fence1_scope)
      (pp_store "y" "1" !!storey_mode !storey_scope)

      (pp_region !!region_x) (pp_atomicity !!atomic_x)
      (pp_region !!region_y) (pp_atomicity !!atomic_y)
      (pp_region !!region_z) (pp_atomicity !!atomic_z)
      (pp_load "y" !!loady_mode !loady_scope)
      (pp_fence !fence2_regions !fence2_mode !fence2_scope)
      (pp_store "z" "1" !!storez_mode !storez_scope)

      (pp_region !!region_x) (pp_atomicity !!atomic_x)
      (pp_region !!region_y) (pp_atomicity !!atomic_y)
      (pp_region !!region_z) (pp_atomicity !!atomic_z)
      (pp_load "z" !!loadz_mode !loadz_scope)
      (pp_fence !fence3_regions !fence3_mode !fence3_scope)
      (pp_store "x" "1" !!storex_mode !storex_scope)

      (pp_scopetree !!scopetree)
    in
    handle_test "3LB" test_name file_contents
  in

  if (!generate_all) then
    foreach region_x (somes single_regions) (
    foreach atomic_x (somes all_atomicities) (
    foreach region_y (somes single_regions) (
    foreach atomic_y (somes all_atomicities) (
    foreach region_z (somes single_regions) (
    foreach atomic_z (somes all_atomicities) (

    let valid_loadx_modes, valid_storex_modes =
      if !!atomic_x then load_modes, store_modes else [NA], [NA] 
    in
    let valid_loady_modes, valid_storey_modes =
      if !!atomic_y then load_modes, store_modes else [NA], [NA] 
    in
    let valid_loadz_modes, valid_storez_modes =
      if !!atomic_z then load_modes, store_modes else [NA], [NA] 
    in

    foreach loadx_mode (somes valid_loadx_modes) (
    foreach loadx_scope (interesting_scopes_for !!loadx_mode) (
    foreach loady_mode (somes valid_loady_modes) (
    foreach loady_scope (interesting_scopes_for !!loady_mode) (
    foreach loadz_mode (somes valid_loadz_modes) (
    foreach loadz_scope (interesting_scopes_for !!loadz_mode) (

    foreach storex_mode (somes valid_storex_modes) (
    foreach storex_scope (interesting_scopes_for !!storex_mode) (
    foreach storey_mode (somes valid_storey_modes) (
    foreach storey_scope (interesting_scopes_for !!storey_mode) (
    foreach storez_mode (somes valid_storez_modes) (
    foreach storez_scope (interesting_scopes_for !!storez_mode) (

    foreach fence1_regions (optional all_regions) (
    let valid_fence1_modes, valid_fence1_scopes = 
      match !fence1_regions with 
      | None -> [None], [None]
      | _ -> somes fence_modes, somes interesting_scopes 
    in
    foreach fence1_mode valid_fence1_modes (
    foreach fence1_scope valid_fence1_scopes (

    foreach fence2_regions (optional all_regions) (
    let valid_fence2_modes, valid_fence2_scopes = 
      match !fence2_regions with 
      | None -> [None], [None] 
      | _ -> somes fence_modes, somes interesting_scopes 
    in
    foreach fence2_mode valid_fence2_modes (
    foreach fence2_scope valid_fence2_scopes (

    foreach fence3_regions (optional all_regions) (
    let valid_fence3_modes, valid_fence3_scopes = 
      match !fence3_regions with 
      | None -> [None], [None] 
      | _ -> somes fence_modes, somes interesting_scopes 
    in
    foreach fence3_mode valid_fence3_modes (
    foreach fence3_scope valid_fence3_scopes (

    foreach scopetree (somes all_scopetrees) (
      generate ()
    ))))))))))))))))))))))))))))
  else
    generate ()

let gen_iriw () =

  let all_scopetrees = [0;1;2] in

  let parse_scopetree = function
    | "P0_P1_P2_P3" -> 0
    | "P0_P1_P2__P3" -> 1
    | "P0_P1__P2_P3" -> 2
    | "P0_P1__P2__P3" -> 3
    | "P0__P1_P2_P3" -> 4
    | "P0__P1_P2__P3" -> 5
    | "P0__P1__P2_P3" -> 6
    | "P0__P1__P2__P3" -> 7
    | x -> raise (Arg.Bad (sprintf "Bad scopetree: %s." x)) in

  let scopetree_usage = sprintf "<%s>" (String.concat "|" [
    "P0_P1_P2_P3";
    "P0_P1_P2__P3";
    "P0_P1__P2_P3";
    "P0_P1__P2__P3";
    "P0__P1_P2_P3";
    "P0__P1_P2__P3";
    "P0__P1__P2_P3";
    "P0__P1__P2__P3";
    ]) in
  
  let pp_scopetree = function
    | 0 -> "device (work_group P0 P1 P2 P3)"
    | 1 -> "device (work_group P0 P1 P2) (work_group P3)"
    | 2 -> "device (work_group P0 P1) (work_group P2 P3)"
    | 3 -> "device (work_group P0 P1) (work_group P2) (work_group P3)"
    | 4 -> "device (work_group P0) (work_group P1 P2 P3)"
    | 5 -> "device (work_group P0) (work_group P1 P2) (work_group P3)"
    | 6 -> "device (work_group P0) (work_group P1) (work_group P2 P3)"
    | 7 -> "device (work_group P0) (work_group P1) (work_group P2) (work_group P3)"
    | _ -> assert false in

  let pp_scopetree_short = function
    | 0 -> "P0_P1_P2_P3"
    | 1 -> "P0_P1_P2__P3"
    | 2 -> "P0_P1__P2_P3"
    | 3 -> "P0_P1__P2__P3"
    | 4 -> "P0__P1_P2_P3"
    | 5 -> "P0__P1_P2__P3"
    | 6 -> "P0__P1__P2_P3"
    | 7 -> "P0__P1__P2__P3"
    | _ -> assert false in

  parse_options parse_scopetree scopetree_usage [
    ("-region_x", mk_region_arg region_x, true);
    ("-atomic_x", mk_bool_arg atomic_x, true);
    ("-region_y", mk_region_arg region_y, true);
    ("-atomic_y", mk_bool_arg atomic_y, true);
    ("-loadx1_mode", mk_mode_arg loadx1_mode, true);
    ("-loadx1_scope", mk_scope_arg loadx1_scope, false);
    ("-loadx2_mode", mk_mode_arg loadx2_mode, true);
    ("-loadx2_scope", mk_scope_arg loadx2_scope, false);
    ("-loady1_mode", mk_mode_arg loady1_mode, true);
    ("-loady1_scope", mk_scope_arg loady1_scope, false);
    ("-loady2_mode", mk_mode_arg loady2_mode, true);
    ("-loady2_scope", mk_scope_arg loady2_scope, false);
    ("-storex_mode", mk_mode_arg storex_mode, true);
    ("-storex_scope", mk_scope_arg storex_scope, false);
    ("-storey_mode", mk_mode_arg storey_mode, true);
    ("-storey_scope", mk_scope_arg storey_scope, false);
    ("-fence1_regions", mk_regions_arg fence1_regions, false);
    ("-fence1_mode", mk_mode_or_bar_arg fence1_mode, false);
    ("-fence1_scope", mk_scope_arg fence1_scope, false);
    ("-fence2_regions", mk_regions_arg fence2_regions, false);
    ("-fence2_mode", mk_mode_or_bar_arg fence2_mode, false);
    ("-fence2_scope", mk_scope_arg fence2_scope, false);
  ];

  let generate () =
    let test_name = sprintf 
        "IRIW_%s%s_%s%s_%s%s%s-%s%s_%s%s%s-%s%s_x%s%s_y%s%s_%s"
        (pp_mode_short !!storex_mode)
        (pp_oscope !storex_scope)

        (pp_mode_short !!storey_mode)
        (pp_oscope !storey_scope)
 
        (pp_mode_short !!loadx1_mode)
        (pp_oscope !loadx1_scope)
        (pp_fence_short "1" !fence1_regions !fence1_mode !fence1_scope)
        (pp_mode_short !!loady1_mode)
        (pp_oscope !loady1_scope)

        (pp_mode_short !!loadx2_mode)
        (pp_oscope !loadx2_scope)
        (pp_fence_short "1" !fence2_regions !fence2_mode !fence2_scope)
        (pp_mode_short !!loady2_mode)
        (pp_oscope !loady2_scope)

        (pp_atomicity_short !!atomic_x)
        (pp_region_short !!region_x)
        (pp_atomicity_short !!atomic_y)
        (pp_region_short !!region_y)

        (pp_scopetree_short !!scopetree) in
    let file_contents = sprintf 
"OpenCL %s
                        
{ 
  [x]=0;
  [y]=0;
}

P0 (%s %sint* x, %s %sint* y) {
  %s;
}

P1 (%s %sint* x, %s %sint* y) {
  %s;
}

P2 (%s %sint* x, %s %sint* y) {
  int r0 = %s;
  %sint r1 = %s;
}

P3 (%s %sint* x, %s %sint* y) {
  int r2 = %s;
  %sint r3 = %s;
}

scopeTree
(%s)
exists (2:r0=1 /\\ 2:r1=0 /\\ 3:r2=1 /\\ 3:r3=0)"
      test_name
      (pp_region !!region_x) (pp_atomicity !!atomic_x)
      (pp_region !!region_y) (pp_atomicity !!atomic_y)
      (pp_store "x" "1" !!storex_mode !storex_scope)

      (pp_region !!region_x) (pp_atomicity !!atomic_x)
      (pp_region !!region_y) (pp_atomicity !!atomic_y)
      (pp_store "y" "1" !!storey_mode !storey_scope)

      (pp_region !!region_x) (pp_atomicity !!atomic_x)
      (pp_region !!region_y) (pp_atomicity !!atomic_y)
      (pp_load "x" !!loadx1_mode !loadx1_scope)
      (pp_fence !fence1_regions !fence1_mode !fence1_scope)
      (pp_load "y" !!loady1_mode !loady1_scope)

      (pp_region !!region_x) (pp_atomicity !!atomic_x)
      (pp_region !!region_y) (pp_atomicity !!atomic_y)
      (pp_load "y" !!loady2_mode !loady2_scope)
      (pp_fence !fence2_regions !fence2_mode !fence2_scope)
      (pp_load "x" !!loadx2_mode !loadx2_scope)

      (pp_scopetree !!scopetree)
    in
    handle_test "IRIW" test_name file_contents
  in

  if (!generate_all) then
    foreach region_x (somes single_regions) (
    foreach atomic_x (somes all_atomicities) (
    foreach region_y (somes single_regions) (
    foreach atomic_y (somes all_atomicities) (

    let valid_loadx_modes, valid_storex_modes =
      if !!atomic_x then load_modes, store_modes else [NA], [NA] 
    in
    let valid_loady_modes, valid_storey_modes =
      if !!atomic_y then load_modes, store_modes else [NA], [NA] 
    in

    foreach loadx1_mode (somes valid_loadx_modes) (
    foreach loadx1_scope (interesting_scopes_for !!loadx1_mode) (
    foreach loady1_mode (somes valid_loady_modes) (
    foreach loady1_scope (interesting_scopes_for !!loady1_mode) (
    foreach loadx2_mode (somes valid_loadx_modes) (
    foreach loadx2_scope (interesting_scopes_for !!loadx2_mode) (
    foreach loady2_mode (somes valid_loady_modes) (
    foreach loady2_scope (interesting_scopes_for !!loady2_mode) (

    foreach storex_mode (somes valid_storex_modes) (
    foreach storex_scope (interesting_scopes_for !!storex_mode) (
    foreach storey_mode (somes valid_storey_modes) (
    foreach storey_scope (interesting_scopes_for !!storey_mode) (

    foreach fence1_regions (optional all_regions) (
    let valid_fence1_modes, valid_fence1_scopes = 
      match !fence1_regions with 
      | None -> [None], [None]
      | _ -> somes fence_modes, somes interesting_scopes 
    in
    foreach fence1_mode valid_fence1_modes (
    foreach fence1_scope valid_fence1_scopes (

    foreach fence2_regions (optional all_regions) (
    let valid_fence2_modes, valid_fence2_scopes = 
      match !fence2_regions with 
      | None -> [None], [None] 
      | _ -> somes fence_modes, somes interesting_scopes 
    in
    foreach fence2_mode valid_fence2_modes (
    foreach fence2_scope valid_fence2_scopes (

    foreach scopetree (somes all_scopetrees) (
      generate ()
    )))))))))))))))))))))))
  else
    generate ()

let () =
  try 
    match Sys.argv.(1) with
    | "MP" | "mp" -> gen_mp ()
    | "3LB" | "3lb" -> gen_3lb ()
    | "IRIW" | "iriw" -> gen_iriw ()
    | x -> raise (Arg.Bad (sprintf "Error: bad family %s." x))
  with 
  | Invalid_argument _ ->
    eprintf "%s\n%s\n" "Error: missing family." usage_msg; 
    exit 0
  | Arg.Bad msg ->
    eprintf "%s\n%s\n" msg usage_msg;
    exit 0

