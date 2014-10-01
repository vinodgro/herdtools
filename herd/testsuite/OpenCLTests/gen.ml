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

let optional xs = None :: somes xs

let foreach xs f = List.iter f xs

let runherd = ref false
let herdargs = ref []
let generate_all = ref false 

let all_families = ["MP";"3LB"]

(*********************************)
(* Handling command-line options *)
(*********************************)

let get_arg arg arg_name = match (!arg) with 
  | None -> raise (Arg.Bad (sprintf "Missing %s." arg_name))
  | Some v -> v

let prog = Sys.argv.(0)

let usage_msg = 
  sprintf "Usage: %s <%s> [options]" 
    prog 
    (String.concat "|" all_families)

let general_options = [
  ("-all",
   Arg.Set generate_all,
   "Automatically generate all tests in this family!");
  ("-herd", 
   Arg.Rest (fun arg -> runherd := true; herdargs := arg :: (!herdargs)), 
   "[options] Run herd with the given options");
]

let parse_options specific_options =
  Arg.current := 1; (* Skip first argument *)
  Arg.parse (general_options @ specific_options)
    (fun x -> raise (Arg.Bad (sprintf "Unlabelled value: %s." x)))
    usage_msg

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

let mk_region_arg r k =
  let usage = "<global|local>" in 
  (k, Arg.String (fun v -> r := Some (parse_region v)), usage)

let mk_regions_arg r k =
  let usage = "<global|local|both>" in 
  (k, Arg.String (fun v -> r := Some (parse_regions v)), usage)

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

let mk_atomicity_arg r k =
  let usage = "<true|false>" in
  (k, Arg.Bool (fun b -> r := Some b), usage)

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
  | "Bar" | "bar" -> Bar
  | x -> raise (Arg.Bad (sprintf "Bad mode: %s." x))

let mk_mode_arg r k =
  let usage = "<na|rlx|acq|rel|ar|sc|bar>" in 
  (k, Arg.String (fun v -> r := Some (parse_mode v)), usage) 

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

let mk_scope_arg r k =
  let usage = "<wi|wg|dev|all>" in 
  (k, Arg.String (fun v -> r := Some (parse_scope v)), usage) 

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

(************************)
(* Instructions         *)
(************************)

let pp_load id mode oscope = match mode with
  | Bar -> 
    raise (Arg.Bad "Load cannot be in barrier-mode.")
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

let pp_store id exp mode oscope = match mode with
  | Bar -> 
    raise (Arg.Bad "Load cannot be in barrier-mode.")
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
      let scope = default oscope S_all in
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
      let scope = default oscope S_all in
      sprintf "_%sF%s-%s-%s" 
        prefix
        (pp_region_short region) 
        (pp_mode_short mode) 
        (pp_scope_short scope)  

let gen_mp () =
  let region_x = ref None in
  let atomic_x = ref None in
  let region_y = ref None in
  let atomic_y = ref None in
  let load_mode = ref None in
  let load_scope = ref None in
  let store_mode = ref None in
  let store_scope = ref None in
  let fence1_regions = ref None in
  let fence1_mode = ref None in
  let fence1_scope = ref None in
  let fence2_regions = ref None in
  let fence2_mode = ref None in
  let fence2_scope = ref None in
  let scopetree = ref None in

  let all_scopetrees = [0;1] in

  let parse_scopetree = function
    | "P0_P1" -> 0
    | "P0__P1" -> 1
    | x -> raise (Arg.Bad (sprintf "Bad scopetree: %s." x)) in
  
  let pp_scopetree = function
    | 0 -> "device (work_group P0 P1)"
    | 1 -> "device (work_group P0) (work_group P1)"
    | _ -> assert false in

  let pp_scopetree_short = function
    | 0 -> "P0_P1"
    | 1 -> "P0__P1"
    | _ -> assert false in

  parse_options [
    mk_region_arg region_x "-region_x";
    mk_atomicity_arg atomic_x "-atomic_x";
    mk_region_arg region_y "-region_y";
    mk_atomicity_arg atomic_y "-atomic_y";
    mk_mode_arg load_mode "-load_mode";
    mk_scope_arg load_scope "-load_scope";
    mk_mode_arg store_mode "-store_mode";
    mk_scope_arg store_scope "-store_scope";
    mk_regions_arg fence1_regions "-fence1_regions";
    mk_mode_arg fence1_mode "-fence1_mode";
    mk_scope_arg fence1_scope "-fence1_scope";
    mk_regions_arg fence2_regions "-fence2_regions";
    mk_mode_arg fence2_mode "-fence2_mode";
    mk_scope_arg fence2_scope "-fence2_scope";
    ("-scopetree",
     Arg.String (fun v -> scopetree := Some (parse_scopetree v)),
     "<P0_P1|P0__P1>") ;
  ];

  let gen_one region_x atomic_x region_y atomic_y 
      load_mode load_scope store_mode store_scope
      fence1_regions fence1_mode fence1_scope
      fence2_regions fence2_mode fence2_scope
      scopetree =
    let test_name = sprintf 
        "MP_%s%s%s_%s%s%s_x%s%s_y%s%s_%s" 
        (pp_mode_short store_mode)
        (pp_oscope store_scope)
        (pp_fence_short "1" fence1_regions fence1_mode fence1_scope)
        (pp_mode_short load_mode)
        (pp_oscope load_scope)
        (pp_fence_short "2" fence2_regions fence2_mode fence2_scope)
        (pp_atomicity_short atomic_x)
        (pp_region_short region_x)
        (pp_atomicity_short atomic_y)
        (pp_region_short region_y)
        (pp_scopetree_short scopetree) in
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
      (pp_region region_x)
      (pp_atomicity atomic_x)
      (pp_region region_y)
      (pp_atomicity atomic_y)
      (pp_store "x" "1" NA None)
      (pp_fence fence1_regions fence1_mode fence1_scope)
      (pp_store "y" "1" store_mode store_scope)
      (pp_region region_x)
      (pp_atomicity atomic_x)
      (pp_region region_y)
      (pp_atomicity atomic_y)
      (pp_load "y" load_mode load_scope)
      (pp_fence fence2_regions fence2_mode fence2_scope)
      (pp_load "x" NA None)
      (pp_scopetree scopetree)
    in
    handle_test "MP" test_name file_contents
  in

  if (!generate_all) then
    foreach single_regions (fun region_x ->
    foreach all_atomicities (fun atomic_x ->
    foreach single_regions (fun region_y ->
    foreach all_atomicities (fun atomic_y ->
    let valid_load_modes, valid_store_modes =
      match atomic_y with true -> load_modes, store_modes 
                        | false -> [NA], [NA] 
    in
    foreach valid_load_modes (fun load_mode ->
    foreach (interesting_scopes_for load_mode) (fun load_scope ->
    foreach valid_store_modes (fun store_mode ->
    foreach (interesting_scopes_for store_mode) (fun store_scope ->
    foreach (optional all_regions) (fun fence1_regions ->
    let valid_fence1_modes, valid_fence1_scopes = 
      match fence1_regions with 
      | None -> [None], [None]
      | _ -> somes fence_modes, somes interesting_scopes 
    in
    foreach valid_fence1_modes (fun fence1_mode ->
    foreach valid_fence1_scopes (fun fence1_scope ->
    foreach (optional all_regions) (fun fence2_regions ->
    let valid_fence2_modes, valid_fence2_scopes = 
      match fence2_regions with 
      | None -> [None], [None] 
      | _ -> somes fence_modes, somes interesting_scopes 
    in
    foreach valid_fence2_modes (fun fence2_mode ->
    foreach valid_fence2_scopes (fun fence2_scope ->
    foreach all_scopetrees (fun scopetree ->
      gen_one 
        region_x atomic_x region_y atomic_y 
        load_mode load_scope store_mode store_scope
        fence1_regions fence1_mode fence1_scope
        fence2_regions fence2_mode fence2_scope
        scopetree
    )))))))))))))))
  else
    let region_x = get_arg region_x "-region_x" in
    let atomic_x = get_arg atomic_x "-atomic_x" in
    let region_y = get_arg region_y "-region_y" in
    let atomic_y = get_arg atomic_y "-atomic_y" in
    let load_mode = get_arg load_mode "-load_mode" in
    let load_scope = !load_scope in
    let store_mode = get_arg store_mode "-store_mode" in
    let store_scope = !store_scope in
    let fence1_regions = !fence1_regions in
    let fence1_mode = !fence1_mode in
    let fence1_scope = !fence1_scope in
    let fence2_regions = !fence2_regions in
    let fence2_mode = !fence2_mode in
    let fence2_scope = !fence2_scope in
    let scopetree = get_arg scopetree "-scopetree" in
    gen_one 
      region_x atomic_x region_y atomic_y 
      load_mode load_scope store_mode store_scope
      fence1_regions fence1_mode fence1_scope
      fence2_regions fence2_mode fence2_scope
      scopetree

let gen_3lb () =
  let region_x = ref None in
  let atomic_x = ref None in
  let region_y = ref None in
  let atomic_y = ref None in
  let region_z = ref None in
  let atomic_z = ref None in
  let loadx_mode = ref None in
  let loadx_scope = ref None in
  let loady_mode = ref None in
  let loady_scope = ref None in
  let loadz_mode = ref None in
  let loadz_scope = ref None in
  let storex_mode = ref None in
  let storex_scope = ref None in
  let storey_mode = ref None in
  let storey_scope = ref None in
  let storez_mode = ref None in
  let storez_scope = ref None in
  let fence1_regions = ref None in
  let fence1_mode = ref None in
  let fence1_scope = ref None in
  let fence2_regions = ref None in
  let fence2_mode = ref None in
  let fence2_scope = ref None in
  let fence3_regions = ref None in
  let fence3_mode = ref None in
  let fence3_scope = ref None in
  let scopetree = ref None in

  let all_scopetrees = [0;1] in

  let parse_scopetree = function
    | "P0_P1_P2" -> 0
    | "P0_P1__P2" -> 1
    | "P0__P1__P2" -> 2
    | x -> raise (Arg.Bad (sprintf "Bad scopetree: %s." x)) in
  
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

  parse_options [
    mk_region_arg region_x "-region_x";
    mk_atomicity_arg atomic_x "-atomic_x";
    mk_region_arg region_y "-region_y";
    mk_atomicity_arg atomic_y "-atomic_y";
    mk_region_arg region_z "-region_z";
    mk_atomicity_arg atomic_z "-atomic_z";
    mk_mode_arg loadx_mode "-loadx_mode";
    mk_scope_arg loadx_scope "-loadx_scope";
    mk_mode_arg loady_mode "-loady_mode";
    mk_scope_arg loady_scope "-loady_scope";
    mk_mode_arg loadz_mode "-loadz_mode";
    mk_scope_arg loadz_scope "-loadz_scope";
    mk_mode_arg storex_mode "-storex_mode";
    mk_scope_arg storex_scope "-storex_scope";
    mk_mode_arg storey_mode "-storey_mode";
    mk_scope_arg storey_scope "-storey_scope";
    mk_mode_arg storez_mode "-storez_mode";
    mk_scope_arg storez_scope "-storez_scope";
    mk_regions_arg fence1_regions "-fence1_regions";
    mk_mode_arg fence1_mode "-fence1_mode";
    mk_scope_arg fence1_scope "-fence1_scope";
    mk_regions_arg fence2_regions "-fence2_regions";
    mk_mode_arg fence2_mode "-fence2_mode";
    mk_scope_arg fence2_scope "-fence2_scope";
    mk_regions_arg fence3_regions "-fence3_regions";
    mk_mode_arg fence3_mode "-fence3_mode";
    mk_scope_arg fence3_scope "-fence3_scope";
    ("-scopetree",
     Arg.String (fun v -> scopetree := Some (parse_scopetree v)),
     "<P0_P1_P2|P0_P1__P2|P0__P1__P2>") ;
  ];

  let gen_one 
      region_x atomic_x region_y atomic_y region_z atomic_z
      loadx_mode loadx_scope loady_mode loady_scope loadz_mode loadz_scope 
      storex_mode storex_scope storey_mode storey_scope storez_mode storez_scope
      fence1_regions fence1_mode fence1_scope
      fence2_regions fence2_mode fence2_scope
      fence3_regions fence3_mode fence3_scope
      scopetree =
    let test_name = sprintf 
        "3LB_%s%s%s-%s%s_%s%s%s-%s%s_%s%s%s-%s%s_x%s%s_y%s%s_z%s%s_%s" 
        (pp_mode_short loadx_mode)
        (pp_oscope loadx_scope)
        (pp_fence_short "1" fence1_regions fence1_mode fence1_scope)
        (pp_mode_short storey_mode)
        (pp_oscope storey_scope)

        (pp_mode_short loady_mode)
        (pp_oscope loady_scope)
        (pp_fence_short "2" fence2_regions fence2_mode fence2_scope)
        (pp_mode_short storez_mode)
        (pp_oscope storez_scope)

        (pp_mode_short loadz_mode)
        (pp_oscope loadz_scope)
        (pp_fence_short "3" fence3_regions fence3_mode fence3_scope)
        (pp_mode_short storex_mode)
        (pp_oscope storex_scope)

        (pp_atomicity_short atomic_x)
        (pp_region_short region_x)
        (pp_atomicity_short atomic_y)
        (pp_region_short region_y)
        (pp_atomicity_short atomic_z)
        (pp_region_short region_z)
        (pp_scopetree_short scopetree) in
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
      (pp_region region_x) (pp_atomicity atomic_x)
      (pp_region region_y) (pp_atomicity atomic_y)
      (pp_region region_z) (pp_atomicity atomic_z)
      (pp_load "x" loadx_mode loadx_scope)
      (pp_fence fence1_regions fence1_mode fence1_scope)
      (pp_store "y" "1" storey_mode storey_scope)

      (pp_region region_x) (pp_atomicity atomic_x)
      (pp_region region_y) (pp_atomicity atomic_y)
      (pp_region region_z) (pp_atomicity atomic_z)
      (pp_load "y" loady_mode loady_scope)
      (pp_fence fence2_regions fence2_mode fence2_scope)
      (pp_store "z" "1" storez_mode storez_scope)

      (pp_region region_x) (pp_atomicity atomic_x)
      (pp_region region_y) (pp_atomicity atomic_y)
      (pp_region region_z) (pp_atomicity atomic_z)
      (pp_load "z" loadz_mode loadz_scope)
      (pp_fence fence3_regions fence3_mode fence3_scope)
      (pp_store "x" "1" storex_mode storex_scope)

      (pp_scopetree scopetree)
    in
    handle_test "3LB" test_name file_contents
  in

  if (!generate_all) then
    foreach single_regions (fun region_x ->
    foreach all_atomicities (fun atomic_x ->
    foreach single_regions (fun region_y ->
    foreach all_atomicities (fun atomic_y ->
    foreach single_regions (fun region_z ->
    foreach all_atomicities (fun atomic_z ->

    let valid_loadx_modes, valid_storex_modes =
      match atomic_x with true -> load_modes, store_modes 
                        | false -> [NA], [NA] in
    let valid_loady_modes, valid_storey_modes =
      match atomic_y with true -> load_modes, store_modes 
                        | false -> [NA], [NA] in
    let valid_loadz_modes, valid_storez_modes =
      match atomic_z with true -> load_modes, store_modes 
                        | false -> [NA], [NA] in

    foreach valid_loadx_modes (fun loadx_mode ->
    foreach (interesting_scopes_for loadx_mode) (fun loadx_scope ->

    foreach valid_loady_modes (fun loady_mode ->
    foreach (interesting_scopes_for loady_mode) (fun loady_scope ->

    foreach valid_loadz_modes (fun loadz_mode ->
    foreach (interesting_scopes_for loadz_mode) (fun loadz_scope ->

    foreach valid_storex_modes (fun storex_mode ->
    foreach (interesting_scopes_for storex_mode) (fun storex_scope ->

    foreach valid_storey_modes (fun storey_mode ->
    foreach (interesting_scopes_for storey_mode) (fun storey_scope ->

    foreach valid_storez_modes (fun storez_mode ->
    foreach (interesting_scopes_for storez_mode) (fun storez_scope ->

    foreach (optional all_regions) (fun fence1_regions ->
    let valid_fence1_modes, valid_fence1_scopes = 
      match fence1_regions with 
      | None -> [None], [None]
      | _ -> somes fence_modes, somes interesting_scopes 
    in
    foreach valid_fence1_modes (fun fence1_mode ->
    foreach valid_fence1_scopes (fun fence1_scope ->

    foreach (optional all_regions) (fun fence2_regions ->
    let valid_fence2_modes, valid_fence2_scopes = 
      match fence2_regions with 
      | None -> [None], [None] 
      | _ -> somes fence_modes, somes interesting_scopes 
    in
    foreach valid_fence2_modes (fun fence2_mode ->
    foreach valid_fence2_scopes (fun fence2_scope ->

    foreach (optional all_regions) (fun fence3_regions ->
    let valid_fence3_modes, valid_fence3_scopes = 
      match fence3_regions with 
      | None -> [None], [None] 
      | _ -> somes fence_modes, somes interesting_scopes 
    in
    foreach valid_fence3_modes (fun fence3_mode ->
    foreach valid_fence3_scopes (fun fence3_scope ->

    foreach all_scopetrees (fun scopetree ->
      gen_one 
        region_x atomic_x region_y atomic_y region_z atomic_z
        loadx_mode loadx_scope loady_mode loady_scope loadz_mode loadz_scope 
        storex_mode storex_scope storey_mode storey_scope storez_mode storez_scope
        fence1_regions fence1_mode fence1_scope
        fence2_regions fence2_mode fence2_scope
        fence3_regions fence3_mode fence3_scope
        scopetree
    ))))))))))))))))))))))))))))
  else
    let region_x = get_arg region_x "-region_x" in
    let atomic_x = get_arg atomic_x "-atomic_x" in
    let region_y = get_arg region_y "-region_y" in
    let atomic_y = get_arg atomic_y "-atomic_y" in
    let region_z = get_arg region_z "-region_z" in
    let atomic_z = get_arg atomic_z "-atomic_z" in
    let loadx_mode = get_arg loadx_mode "-loadx_mode" in
    let loadx_scope = !loadx_scope in
    let loady_mode = get_arg loady_mode "-loady_mode" in
    let loady_scope = !loady_scope in
    let loadz_mode = get_arg loadz_mode "-loadz_mode" in
    let loadz_scope = !loadz_scope in
    let storex_mode = get_arg storex_mode "-storex_mode" in
    let storex_scope = !storex_scope in
    let storey_mode = get_arg storey_mode "-storey_mode" in
    let storey_scope = !storey_scope in
    let storez_mode = get_arg storez_mode "-storez_mode" in
    let storez_scope = !storez_scope in
    let fence1_regions = !fence1_regions in
    let fence1_mode = !fence1_mode in
    let fence1_scope = !fence1_scope in
    let fence2_regions = !fence2_regions in
    let fence2_mode = !fence2_mode in
    let fence2_scope = !fence2_scope in
    let fence3_regions = !fence3_regions in
    let fence3_mode = !fence3_mode in
    let fence3_scope = !fence3_scope in
    let scopetree = get_arg scopetree "-scopetree" in
    gen_one 
      region_x atomic_x region_y atomic_y region_z atomic_z
      loadx_mode loadx_scope loady_mode loady_scope loadz_mode loadz_scope 
      storex_mode storex_scope storey_mode storey_scope storez_mode storez_scope
      fence1_regions fence1_mode fence1_scope
      fence2_regions fence2_mode fence2_scope
      fence3_regions fence3_mode fence3_scope
      scopetree

let () =
  try 
    match Sys.argv.(1) with
    | "MP" | "mp" -> gen_mp ()
    | "3LB" | "3lb" -> gen_3lb ()
    | x -> raise (Arg.Bad (sprintf "Error: bad family %s." x))
  with 
  | Invalid_argument _ ->
    eprintf "%s\n%s\n" "Error: missing family." usage_msg; 
    exit 0
  | Arg.Bad msg ->
    eprintf "%s\n%s\n" msg usage_msg;
    exit 0

