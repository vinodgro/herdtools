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

(*********************************************************************************************************************)

(* Define a monad, which is a composition of event set state and a single variable state 
   (to pick new eiids *)

module type S =
  sig
    module A     : Arch.S

    module VC    : Valconstraint.S      
    with type atom = A.V.v
    and type cst = A.V.cst
    and type solution = A.V.solution
    and type location = A.location
    and type state = A.state

    type 'a t
	  
    val zeroT        : unit t
    val unitT        : 'a -> 'a t
    val (>>=) : 'a t -> ('a -> 'b t) -> ('b) t
    val (>>*=) : 'a t -> ('a -> 'b t) -> ('b) t
    val exch : 'a t -> 'a t -> ('a -> 'b t) ->  ('a -> 'b t) ->  ('b * 'b) t
    val (>>>) : 'a t -> ('a -> 'b t) -> 'b t
    val (>>|) : 'a t -> 'b t -> ('a * 'b)  t
    val (>>::) : 'a t -> 'a list t -> 'a list t
    val (|*|)   : unit t -> unit t -> unit t   (* Cross product *)
    val lockT : 'a t -> 'a t
    val forceT : 'a -> 'b t -> 'a t
    val (>>!) : 'a t -> 'b -> 'b t

    val discardT : 'a t -> unit t
    val addT : 'a -> 'b t -> ('a * 'b) t
    val filterT : A.V.v -> A.V.v t -> A.V.v t
    val choiceT : A.V.v -> 'a t -> 'a t -> 'a t  
    val altT : 'a t -> 'a t -> 'a t  

    val tooFar : string -> unit t

    val read_loc : A.location -> A.inst_instance_id -> A.V.v t
    val read_loc_atrb : A.location -> A.inst_instance_id -> A.atrb list -> A.V.v t
    val read_reg : A.reg -> A.inst_instance_id -> A.V.v t
    val read_mem : A.global_loc -> A.inst_instance_id -> A.V.v t
    val read_mem_atomic : A.global_loc -> A.inst_instance_id -> A.V.v t

    val write_loc : A.location -> A.V.v -> A.inst_instance_id -> unit t
    val write_loc_atrb : A.location -> A.V.v -> A.inst_instance_id -> A.atrb list -> unit t

    val rmw_loc : 
      A.location -> A.V.v -> A.V.v -> A.inst_instance_id 
      -> A.atrb list -> A.atrb list -> unit t

    val lock_loc : A.location -> A.inst_instance_id -> unit t
    val unlock_loc : A.location -> A.inst_instance_id -> unit t

    val write_reg : A.reg -> A.V.v -> A.inst_instance_id -> unit t
    val write_mem : A.global_loc -> A.V.v -> A.inst_instance_id -> unit t
    val write_mem_atomic : A.global_loc -> A.V.v -> A.inst_instance_id -> unit t

    val write_flag :
	A.reg -> Op.op -> A.V.v -> A.V.v ->  A.inst_instance_id -> unit t
	
    val create_barrier : A.barrier -> A.inst_instance_id -> unit t
    val create_barrier_atrb : A.barrier -> A.inst_instance_id -> A.atrb list -> unit t

    val op1 : Op.op1 -> A.V.v -> A.V.v t
    val op : Op.op -> A.V.v -> A.V.v -> A.V.v t
    val op3 : Op.op3 -> A.V.v -> A.V.v -> A.V.v -> A.V.v t
    val add : A.V.v -> A.V.v -> A.V.v t

    val assign : A.V.v -> A.V.v -> unit t
    val commit :  A.inst_instance_id -> unit t

    val initwrites : (A.location * A.V.v) list -> unit t


(* Read out monad *)
    type evt_struct
    type output = VC.cnstrnts * evt_struct

    val get_output  : 'a t -> output list
  end


