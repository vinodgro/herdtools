module type S = sig
  type ins_
  type reg_arg_

  type pseudo_ =
    | Nop
    | Label of string * pseudo_
    | Instruction of ins_
    | Macro of string * reg_arg_ list

  include Pseudo.S 
    with type pseudo = pseudo_
     and type ins = ins_
     and type reg_arg = reg_arg_

end

(* Common to all arch, memevents and  litmus *)

module Make (I : Pseudo.I) : 
  (S with type ins_ = I.ins and type reg_arg_ = I.reg_arg) =
struct
  type ins = I.ins
  type ins_ = ins
  type reg_arg = I.reg_arg
  type reg_arg_ = reg_arg
(* Parsed instructions, ie instructions enriched with labels *)
  type pseudo_ =
    | Nop
    | Label of string * pseudo_
    | Instruction of ins
    | Macro of string * reg_arg list
  type pseudo = pseudo_

(* Fold/Map lifting *)
  let rec pseudo_map f ins = match ins with
    | Nop -> Nop
    | Instruction ins -> Instruction (f ins)
    | Label (lbl,ins) -> Label (lbl, pseudo_map f ins)
    | Macro (_,_) -> assert false

  let rec pseudo_fold f k ins = match ins with
    | Nop -> k
    | Instruction ins -> f k ins
    | Label (_,ins) -> pseudo_fold f k ins
    | Macro (_,_) -> assert false

(* Fold/Map over labels *)

  let rec fold_labels f k ins = match ins with
  | Nop -> k
  | Instruction ins -> I.fold_labels k f ins
  | Label (lbl,ins) -> fold_labels f (f k lbl) ins
  | Macro _ -> assert false

  let rec map_labels f ins = match ins with
  | Nop -> Nop
  | Instruction ins -> Instruction (I.map_labels f ins)
  | Label (lbl,ins) -> Label (f lbl, map_labels f ins)
  | Macro _ -> assert false

(* For printing the program, code per processor *)
  type nice_prog = (int * pseudo list) list

(* Counting memory accesses *)
  let get_naccesses code =
    List.fold_left
      (pseudo_fold
         (fun k ins -> k + I.get_naccesses ins))
      0 code
end
