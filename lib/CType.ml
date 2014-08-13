(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

type base = string

type t =
  | Base of base
  | Volatile of t
  | Atomic of t
  | Pointer of t
(** OpenCL *)
  | Global of t
  | Local of t

let rec  dump = function
  | Base s -> s
  | Volatile (Base s) -> "volatile " ^ s
  | Atomic (Base s) -> "_Atomic " ^ s
  | Volatile t -> sprintf "%s volatile" (dump t)
  | Atomic t -> sprintf "_Atomic (%s)" (dump t)
  | Pointer t -> dump t  ^ "*"
  | Global t -> sprintf "__global %s" (dump t)
  | Local t -> sprintf "__local %s" (dump t)

let rec is_global = function
  | Volatile t | Atomic t | Pointer t -> is_global t
  | Global _ -> true
  | _ -> false

let rec is_local = function
  | Volatile t | Atomic t | Pointer t -> is_local t
  | Local _ -> true
  | _ -> false

let rec is_private = function
  | Volatile t | Atomic t | Pointer t -> is_private t
  | Local _ | Global _ -> false
  | Base _ -> true

let rec is_atomic = function
  | Volatile t | Local t | Global t -> is_atomic t
  | Atomic _ -> true
  | _ -> false

let strip_atomic0 = function
  | Atomic t -> t
  | t -> t

let rec strip_atomic = function
  | Volatile t -> Volatile (strip_atomic t)
  | t ->  strip_atomic0 t

let strip_volatile0 = function
  | Volatile t -> t
  | t -> t

let rec strip_volatile = function
  | Atomic t -> Atomic (strip_volatile t)
  | t ->  strip_volatile0 t


let rec is_ptr_to_atomic = function
  | Volatile t | Local t | Global t -> is_ptr_to_atomic t
  | Pointer t -> is_atomic t
  | _ -> false

let rec is_ptr_to_global = function
  | Volatile t | Atomic t -> is_ptr_to_global t
  | Pointer t -> is_global t
  | _ -> false

let rec is_ptr_to_local = function
  | Volatile t | Atomic t -> is_ptr_to_local t
  | Pointer t -> is_local t
  | _ -> false

let rec is_ptr_to_private = function
  | Volatile t | Atomic t -> is_ptr_to_private t
  | Pointer t -> is_private t
  | _ -> false

let rec is_mutex = function
  | Volatile t | Atomic t | Global t | Local t -> is_mutex t
  | Base s -> RunTypeUtils.mutex_is_substring s
