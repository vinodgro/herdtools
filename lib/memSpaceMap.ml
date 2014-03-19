open Printf

(** GPU memory space map *)


type gpu_memory_space = 
| GlobalMem
| SharedMem

let pp_gpu_memory_space x =
  match x with
  | GlobalMem -> "global"
  | SharedMem -> "shared"

type mem_space_map = (string * gpu_memory_space) list

(* let mem_space_map = ref No_mem_space_map *)

let pp_memory_space_map m = 
  let str_list = 
    List.map 
      (fun (x,y) -> sprintf "%s:%s" x (pp_gpu_memory_space y)) m 
  in
  String.concat "; " str_list
