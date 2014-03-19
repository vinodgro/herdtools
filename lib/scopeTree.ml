open Printf

(*********************************)
(* GPU memory map and scope tree *)
(*********************************)

type gpu_memory_space = 
| GlobalMem
| SharedMem

let pp_gpu_memory_space x =
  match x with
  | GlobalMem -> "global"
  | SharedMem -> "shared"

type thread      = int
type warp        = thread list
type cta         = warp list
type kernel      = cta list
type device      = kernel list

type scope_tree = 
| Scope_tree of device list
| No_scope_tree

let scope_tree = ref No_scope_tree
    
let rec create_list total acc l = 
  if acc = total then l
  else
    create_list total (acc+1) l@[acc]
      
(*The default scope tree for CPUs (used in -ext and -int operator in herd)*)
let cpu_scope_tree n = 
  let w = (create_list n 0 []) in 
  let cta = List.map (fun x -> [x]) w in
  let ker = List.map (fun x -> [x]) cta in
  let dev = List.map (fun x -> [x]) ker in
  let top = List.map (fun x -> [x]) dev in
  Scope_tree(top)

type mem_space_map = 
| Mem_space_map of (string * gpu_memory_space) list
| No_mem_space_map

let mem_space_map = ref No_mem_space_map

let pp_warp w =
  let mapped_list = List.map (fun (i) -> sprintf "P%d" i) w in
  "(warp " ^(String.concat " " mapped_list) ^ ")"

let pp_cta c =
  let mapped_list = List.map (fun (w) -> pp_warp w) c in
  "(cta " ^(String.concat " " mapped_list) ^ ")"

let pp_kernel k =
  let mapped_list = List.map (fun (c) -> pp_cta c) k in
  "(kernel " ^(String.concat " " mapped_list) ^ ")"

let pp_device k =
  let mapped_list = List.map (fun (c) -> pp_kernel c) k in
  "(device " ^(String.concat " " mapped_list) ^ ")"

let pp_scope_tree s = 
  let mapped_list = List.map (fun (k) -> pp_device k) s in
  String.concat " " mapped_list
  

let pp_scope_tree s = 
  match s with
  | Scope_tree s ->  pp_scope_tree s
  | _ -> ""

let pp_memory_space_map m = 
  match m with 
  | Mem_space_map m -> let str_list = 
			 List.map 
			   (fun (x,y) -> sprintf "%s:%s" x (pp_gpu_memory_space y)) m 
		       in
		       String.concat "; " str_list
		       

  | _ -> ""
