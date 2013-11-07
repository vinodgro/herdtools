(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type Config = sig
  val numeric_labels : bool
  val signaling : bool
  val timeloop : int
  val barrier : Barrier.t
end

module Make
    (O:Config)
    (T:Test.S) =
  struct

    let compile t =
      let
          { MiscParser.init = init ;
            info = info;
            prog = code;
            condition = final;
            locations = locs ; _
          } = t in
      { T.init = (* init *) assert false (* TODO… *);
        info = info;
        code = assert false;
        condition = (* final *) assert false (* TODO… *);
        globals = assert false;
        flocs = assert false (* List.map fst locs *) (* TODO… *) ;
        src = t;
      }

  end
