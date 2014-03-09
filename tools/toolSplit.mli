(*********************************************************************)
(*                        DIY                                        *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(**************************************************)
(* Apply a function (zyva) to one (splitted) test *)
(**************************************************)

module Top :
    functor (LexConf:Splitter.Config) ->
      functor (T:sig type t end) -> (* Return type, must be abstracted *)
        functor (B: sig val zyva : in_channel -> Splitter.result -> T.t end) ->
sig
  val from_file : string -> T.t
end
