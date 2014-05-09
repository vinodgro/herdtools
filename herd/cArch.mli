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

(** Basic arch, ie with no definition of what a global location is *)

module type Config = sig
  val texmacros : bool
  val hexa : bool
  val brackets : bool
end

module type S =
  sig

  (* Who am I ? *)
  val arch : Archs.t

  (***********************************************)
  (* Basic arch types and their basic operations *)
  (***********************************************)

  type reg
  val pc : reg (* Program counter *)

  val parse_reg : string -> reg option
  val pp_reg : reg -> string
  val reg_compare : reg -> reg -> int

(*
  type reservation 
  val pp_res : reservation -> string
  val res_compare : reservation -> reservation -> int 
*)

  type barrier
  val all_kinds_of_barriers : barrier list
  val pp_barrier            : barrier -> string
  val barrier_compare : barrier -> barrier -> int


  type instruction
  val pp_instruction : PPMode.t -> instruction -> string
  (* Shorthand for parsable dump *)
  val dump_instruction : instruction -> string

  (*************************************)
  (* All this needed for symbolic regs *)
  (*************************************)

  (* Registers that can be allocated to symbolic registers *)
  val allowed_for_symb : reg list

  (* Apply fonctions to all registers  either real or symbolic *)
  val fold_regs :
	(reg -> 'a -> 'a) * (string -> 'b -> 'b) ->
	  'a * 'b -> instruction -> 'a * 'b

  (* Map over all registers *)
    val map_regs :
	(reg -> reg) -> (string -> reg) -> instruction -> instruction

  (* Apply function to addresses present in  code *)
  val fold_addrs : (SymbConstant.v -> 'a -> 'a) -> 'a -> instruction -> 'a

  (* Map over addresses *)
  val map_addrs :
      (SymbConstant.v -> SymbConstant.v) -> instruction -> instruction

  (* Normalize instruction (for hashes) *)
  val norm_ins : instruction -> instruction

  (* Recognise store, data read from register r *)
  val is_data : reg -> instruction -> bool

  (* Instruction continuation *)
  val get_next : instruction -> Label.next list


(*  include Pseudo.S with type ins = instruction and type reg_arg = reg 

  val get_macro :
      string ->
        (reg list -> pseudo list -> pseudo list)  
*)

(* For printing the program *)
  type nice_prog = CAst.t list

    module V : Value.S

    include ArchExtra.S with module I.V = V
    and type I.arch_reg = reg
    and type I.arch_instruction = instruction
   end
      
