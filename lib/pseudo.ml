(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Closed signature for basic view of architectures *)

module type S = sig
  type ins
  type reg_arg

  type pseudo =
    | Nop
    | Label of string * pseudo
    | Instruction of ins
    | Macro of string * reg_arg list
    | Choice of ins * pseudo list * pseudo list
    | Loop of ins * pseudo list

(* Lifting of fold/map *)
  val pseudo_map : (ins -> ins) -> pseudo -> pseudo
  val pseudo_fold : ('a -> ins -> 'a) -> 'a -> pseudo -> 'a

(* Fold/Map over labels *)
  val fold_labels : ('a -> string -> 'a) -> 'a -> pseudo -> 'a
  val map_labels : (string -> string) -> pseudo -> pseudo

(* For printing the program, code per processor *)
  type nice_prog = (int * pseudo list) list

(* Counting (static) memory accesses *)
  val get_naccesses : pseudo list -> int
end

(* Input signature *)
module type I = sig
  type ins
  type reg_arg

(* Number of memory access per instruction *)
  val get_naccesses : ins -> int
(* fold/map over labels in instructions,
   used for label normalisation *)
  val fold_labels : 'a -> ('a -> string -> 'a) -> ins -> 'a
  val map_labels : (string -> string) -> ins -> ins
end

(* Common to all arch, memevents and  litmus *)

module Make (I : I) : 
  (S with type ins = I.ins and type reg_arg = I.reg_arg) =
struct
  type ins = I.ins
  type reg_arg = I.reg_arg
(* Parsed instructions, ie instructions enriched with labels *)
  type pseudo =
    | Nop
    | Label of string * pseudo
    | Instruction of ins
    | Macro of string * reg_arg list
    | Choice of ins * pseudo list * pseudo list
    | Loop of ins * pseudo list

(* Fold/Map lifting *)
  let rec pseudo_map f ins = match ins with
    | Nop -> Nop
    | Instruction ins -> Instruction (f ins)
    | Label (lbl,ins) -> Label (lbl, pseudo_map f ins)
    | Macro (_,_) -> assert false
    | Choice (ins,p1,p2) -> 
      let p1' = List.map (pseudo_map f) p1 in
      let p2' = List.map (pseudo_map f) p2 in 
      Choice (f ins, p1', p2')
    | Loop (ins,p) -> 
      let p' = List.map (pseudo_map f) p in
      Loop (f ins, p')

  let rec pseudo_fold f k ins = match ins with
    | Nop -> k
    | Instruction ins -> f k ins
    | Label (_,ins) -> pseudo_fold f k ins
    | Macro (_,_) -> assert false
    | Choice (ins, p1, p2) ->
      let k = f k ins in
      let k = List.fold_left (pseudo_fold f) k p1 in
      let k = List.fold_left (pseudo_fold f) k p2 in
      k
    | Loop (ins, p) ->
      let k = f k ins in
      let k = List.fold_left (pseudo_fold f) k p in
      k

(* Fold/Map over labels *)

  let rec fold_labels f k ins = match ins with
  | Nop -> k
  | Instruction ins -> I.fold_labels k f ins
  | Label (lbl,ins) -> fold_labels f (f k lbl) ins
  | Macro _ -> assert false
  | Choice(ins,p1,p2) -> 
    let k = I.fold_labels k f ins in
    let k = List.fold_left (fold_labels f) k p1 in
    let k = List.fold_left (fold_labels f) k p2 in
    k
  | Loop(ins,p) ->
    let k = I.fold_labels k f ins in
    let k = List.fold_left (fold_labels f) k p in
    k

  let rec map_labels f ins = match ins with
  | Nop -> Nop
  | Instruction ins -> Instruction (I.map_labels f ins)
  | Label (lbl,ins) -> Label (f lbl, map_labels f ins)
  | Macro _ -> assert false
  | Choice(ins,p1,p2) -> 
    let ins' = I.map_labels f ins in
    let p1' = List.map (map_labels f) p1 in
    let p2' = List.map (map_labels f) p2 in 
    Choice(ins', p1', p2')
  | Loop(ins,p) ->
    let ins' = I.map_labels f ins in
    let p' = List.map (map_labels f) p in
    Loop(ins', p')

(* For printing the program, code per processor *)
  type nice_prog = (int * pseudo list) list

(* Counting memory accesses *)
  let get_naccesses code =
    List.fold_left
      (pseudo_fold
         (fun k ins -> k + I.get_naccesses ins))
      0 code
end
