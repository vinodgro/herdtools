type proc = Proc of int

let proc_compare p q = 
  match p,q with
  | Proc i, Proc j -> Misc.int_compare i j

let proc_eq p q =
  match p,q with
  | Proc i, Proc j -> i==j

let pp_proc = function
  | Proc i -> string_of_int i

let proc_to_int = function
  | Proc i -> i

let int_to_proc = function
  | i -> Proc i
