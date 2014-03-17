(*********************************)
(* GPU memory map and scope tree *)
(*********************************)

type gpu_memory_space = 
| Global
| Shared

type thread      = int
type warp        = thread list
type cta         = warp list
type kernel      = cta list
type device      = kernel list

type scope_tree = 
| Scope_tree of device list
| No_scope_tree

type mem_space_map = 
| Mem_space_map of (string * gpu_memory_space) list
| No_mem_space_map

val pp_scope_tree : scope_tree -> string
val pp_memory_space_map : mem_space_map -> string
val cpu_scope_tree : int -> scope_tree
