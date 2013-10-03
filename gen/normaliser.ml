(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)
open Code
open Misc
open Printf


exception CannotNormalise

module type Config = sig
  val lowercase : bool
end

module Make : functor (C:Config) -> functor (E:Edge.S) ->
  sig
(* Normalise, return normalised cycle *)
    val normalise : E.edge list ->  E.edge list
(* Return family name, without normalising *)
    val family : E.edge list -> string
(* Both *)
    val normalise_family : E.edge list ->  string * E.edge list
  end
    =  functor (C:Config) -> functor (E : Edge.S) ->
  struct
(* Cycles of edges *)
    module CE = struct
      type t =
          { edge : E.edge ; mutable dir : dir ;
            mutable next : t ; mutable    prev : t; }

      let e0 = E.parse_edge "Rfi"

      let rec nil =
        { edge = e0; dir=R ;  next = nil ; prev = nil ; }

      let dir_src e = match E.dir_src e with
      | Dir d -> d
      | Irr -> Warn.fatal "Unresolved direction"


      let mk_cycle es =
        let ms =
          List.map
            (fun e ->
              { edge=e; dir=dir_src e;next=nil; prev=nil;}) es in

        let patch = function
          | [] -> assert false
          | x::xs ->
              let rec do_rec prev = function
                | [] ->
                    prev.next <- x ; x.prev <- prev
                | y::ys ->
                    prev.next <- y ; y.prev <- prev ;
                    do_rec y ys in
              do_rec x xs ; x in
        patch ms


      let same_proc e = E.get_ie e = Int
      let diff_proc e = E.get_ie e = Ext

      exception NotFound

      let find_node p n =
        let rec do_rec m =
          if p m then m
          else
            let m = m.next in
            if m == n then raise NotFound
            else do_rec m in
        do_rec n

      let map f n =
        let rec do_rec m =
          let y = f m in
          let m = m.next in
          if m == n then [y]
          else y::do_rec m in
        do_rec n

      let edges n = map (fun n -> n.edge) n

      let pp n =
        let xs = map (fun n -> E.pp_edge n.edge) n in
        String.concat " " xs

      let find_edge p = find_node (fun n -> p n.edge)

      let find_start_proc n =
        if
          diff_proc n.prev.edge
        then n
        else    
          let n = find_edge diff_proc n in
          try find_edge same_proc n
          with NotFound -> n

      let split_procs n =
        let n = find_start_proc n in
        let rec do_rec m =
          let e = m in  (* n is the entry of a proc *)
          let o = find_edge diff_proc m in
          if o.next == n then [(e,o)]
          else (e,o)::do_rec o.next in
        do_rec n

      let compare_edges e1 e2 =
        let open E in
        match e1.edge,e2.edge with
        | (Po _|Rf _),(Fenced _|Dp _)
        | Dp _,Fenced _
          -> 1
        | (Fenced _|Dp _),(Po _|Rf _)
        | Fenced _,Dp _ -> -1              
        | _,_ -> Pervasives.compare e1 e2

      let ninternals n =
        let rec do_rec r m =
          match E.get_ie m.edge with
          | Ext -> r
          | Int ->
              if m.next == n then r
              else do_rec (r+1) m.next in
        do_rec 0 n

      let compare_edges_cycle n1 n2 =
        let rec do_rec m1 m2 =
          match compare_edges m1.edge m2.edge with
          | 0 ->
              let m1 = m1.next and m2 = m2.next in
              if m1 == n1 && m2 == n2 then 0
              else begin
                assert (m1 != n1 && m2 != n2) ;
                do_rec m1 m2
              end
          | r -> r in
        let i1 = ninternals n1
        and i2 = ninternals n2 in
        match Pervasives.compare i1 i2 with
        | 0 ->  do_rec n1 n2
        | r -> r
    end
(* In/Out *)
    type points = One of dir | Two of dir * dir

    let order =
      [|
        One W;
        Two (W,W);
        Two (R,R);
        Two (R,W);
        Two (W,R);
        One R;
      |]

    let pp_points = function
      | One d -> pp_dir d
      | Two (d1,d2) -> pp_dir d1 ^ pp_dir d2

    let t_id = Hashtbl.create 17

    let () =
      Array.iteri
        (fun k p -> Hashtbl.add t_id p k)
        order

    let id p =
      try Hashtbl.find t_id p
      with Not_found -> assert false

    let compare_points p1 p2 =
      let i1 = id p1 and i2 = id p2 in
      Misc.int_compare i1 i2

    module CP = struct
      type t =
          { points : points ; cycle : CE.t ;
            mutable next : t ; mutable prev : t ; }

      let rec nil =
        { points = One R; cycle = CE.nil; prev = nil; next = nil; }

      let mk_cycle cy =
        let es = CE.mk_cycle cy in
        let eos = CE.split_procs es in
        let ms =
          List.map
            (fun (e,o) ->
              let p =
                if e == o then One e.CE.dir
                else Two (e.CE.dir,o.CE.dir) in
              { points=p; cycle=e; prev=nil; next=nil; })
            eos in
        let patch = function
          | [] -> assert false
          | x::xs ->
              let rec do_rec prev = function
                | [] ->
                    prev.next <- x ; x.prev <- prev
                | y::ys ->
                    prev.next <- y ; y.prev <- prev ;
                    do_rec y ys in
              do_rec x xs ; x in
        patch ms


      let map f n =
        let rec do_rec m =
          let y = f m in
          let m = m.next in
          if m == n then [y]
          else y::do_rec m in
        do_rec n

      let compare_points_cycle n1 n2 =
        let rec do_rec m1 m2 =
          match compare_points m1.points m2.points with
          | 0 ->
              let m1 = m1.next and m2 = m2.next in
              if m1 == n1 && m2 == n2 then 0
              else begin
                assert (m1 != n1 && m2 != n2) ;
                do_rec m1 m2
              end
          | r -> r in
        do_rec n1 n2

      let compare n1 n2 = match compare_points_cycle n1 n2 with
      | 0 -> CE.compare_edges_cycle n1.cycle n2.cycle 
      | r -> r
      let norm n =
        let rec do_rec r m =
          let cmp = compare r m in
          let r = if cmp < 0 then r else m in
          let m = m.next in
          if m == n then r
          else do_rec r m in
        do_rec n n.next

      let pp n =
        let xs = map (fun n -> pp_points n.points) n in
        String.concat "+" xs

    end

    let pp_key key =
      let pp = match key with
      | "WW+RR" -> "MP"          
      | "WR+WR" -> "SB"
      | "WR+WR+WR" -> "3.SB"
      | "WR+WR+WR+WR" -> "4.SB"
      | "W+RW+RR" -> "WRC"
      | "W+RR+WR" -> "RWC"
      | "RW+RW" -> "LB"
      | "RW+RW+RW" -> "3.LB"
      | "RW+RW+RW+RW" -> "4.LB"
      | "WW+WR" -> "R"
      | "W+RW+WR" -> "WRW+WR"
      | "W+RR+WW" -> "WRR+2W"
      | "WW+RW" -> "S"
      | "W+RW+RW" -> "WWC"
      | "WW+WW" -> "2+2W"
      | "WW+WW+WW" -> "3.2W"
      | "WW+WW+WW+WW" -> "4.2W"
      | "W+RW+WW" -> "WRW+2W"
      | "WW+RR+WR" -> "W+RWC"
      | "WW+RW+RR" -> "ISA2"
      | "W+RR+W+RR" -> "IRIW"
      | "W+RR+W+RW"|"W+RW+W+RR" -> "IRRWIW"
      | "W+RW+W+RW" -> "IRWIW"
      | "WW+RW+WR" -> "Z6.0"
      | "WW+WW+RW" -> "Z6.1"
      | "WW+RW+RW" -> "Z6.2"
      | "WW+WW+RR" -> "Z6.3"
      | "WW+WR+WR" -> "Z6.4"
      | "WW+WW+WR" -> "Z6.5"
      | k -> k in
      if C.lowercase then String.lowercase pp else pp

    let normalise cy =
      try
        let ps = CP.mk_cycle cy in
        let ps = CP.norm ps in
        let cy = CE.map (fun e -> e.CE.edge) ps.CP.cycle in
        cy
      with CE.NotFound ->
        raise CannotNormalise

    let family cy =
      let ps = CP.mk_cycle cy in
      let key = CP.pp ps  in
      pp_key key
      
    let normalise_family cy =
      try
        let ps = CP.mk_cycle cy in
        let ps = CP.norm ps in
        let key = CP.pp ps  in
        let cy = CE.map (fun e -> e.CE.edge) ps.CP.cycle in
        pp_key key,cy
      with CE.NotFound ->
        raise CannotNormalise
  end
