(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Dump a test, litmus sources *)


module Make(T: Test.S) : sig
  type test =
      (T.A.state, (int * T.A.pseudo list) list, T.C.constr, T.A.location)
        MiscParser.result
  val dump : out_channel -> Name.t -> test -> unit
  val lines : Name.t -> test -> string list
end = struct
  type test =
      (T.A.state, (int * T.A.pseudo list) list, T.C.constr, T.A.location)
        MiscParser.result
  include SimpleDumper.Make
      (struct
        open Printf

        module A = T.A

        let dump_state_atom (loc,v) =
            sprintf "%s=%s" (A.pp_location loc) (A.V.pp_v v)

        type state = A.state
        let dump_state st =
            String.concat " "
              (List.map
                 (fun a -> sprintf "%s;" (dump_state_atom a))
              st)

            
        type constr = T.C.constr
        let dump_atom a =
          let open ConstrGen in
          match a with
          | LV (loc,v) -> dump_state_atom (loc,v)
          | LL (loc1,loc2) ->
              sprintf "%s=%s" (A.pp_location loc1) (A.pp_rval loc2)

        let dump_constr = ConstrGen.constraints_to_string dump_atom

        type location = A.location
        let dump_location loc = T.A.pp_location loc
      end)
end
