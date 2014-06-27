(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

type t = Ty of string | Pointer of string

let dump = function
  | Ty x -> x
  | Pointer x -> x ^ "*"

let atomic_prf = "_Atomic "
let alen = String.length atomic_prf

let is_atomic_type s =
  String.length s >= alen && String.sub s 0 alen = atomic_prf

let is_atomic = function
  | Pointer _ -> false
  | Ty s -> is_atomic_type s

let is_ptr_to_atomic = function
  | Pointer s -> is_atomic_type s
  | Ty _ -> false

let is_mutex = function
  | Pointer _ -> false
  | Ty s -> RunTypeUtils.mutex_is_substring s

let strip_atomic t = match t with
  | Pointer _ -> t
  | Ty s ->
      Ty
        begin
          if is_atomic_type s then
            String.sub s alen (String.length s - alen)
          else s
        end
