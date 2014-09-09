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


(* Systematic allocation of test threads according to machine topology *)

module type Config = sig
  val verbose : int
  val nthreads : int
  val avail : int
  val smt : int
  val nsockets : int
  val smtmode : Smt.t
end

module Make(Cfg:Config) (O:Indent.S) : sig
  val dump_alloc : unit -> unit
end = struct
  open Cfg
  open Printf

  let ncores = avail / smt
  let cores_in_sock = ncores / nsockets
  let ninst =  avail / nthreads

  let pp_t pp_elt t =
    let pp = List.map (fun e -> sprintf "%s" (pp_elt e)) (Array.to_list t) in
    "{" ^ String.concat ", " pp ^"}"

  let topo =
    Array.init nsockets
      (fun s ->
        Array.init cores_in_sock
          (fun i ->
            Array.init smt
              (match smtmode with
              | Smt.Seq ->  fun j -> cores_in_sock * smt * s + smt*i + j
              | Smt.End ->  fun j -> i + cores_in_sock * s + ncores * j
              | Smt.No -> assert false)))

      
  let pp_ints = pp_t (sprintf "%i")
  let pp_intss = pp_t pp_ints
  let pp_intsss = pp_t pp_intss



  let circu = function
    | x::xs -> xs @ [x]
    | [] -> []

  let procs = Misc.interval 0 nthreads

(*
  let pp_ints xs =
    sprintf "[%s]" (String.concat "," (List.map (sprintf "%i") xs))

  let pp_groups chan gs =
    fprintf chan "%s" (String.concat " " (List.map pp_ints gs))

*)

  let pp_xs pp_elt xs =
    sprintf "[%s]"
      (String.concat "," (List.map pp_elt xs))

  let pp_g xs = pp_xs (sprintf "%i") xs

  let pp_gs gs = pp_xs pp_g gs

  let pp_gss gss = String.concat "," (List.map pp_gs gss)

  let sort_gs gs =
    List.sort
      (fun g1 g2 -> match compare (List.length g2) (List.length g1) with
      | 0 -> compare g1 g2
      | r -> r)
      gs


  let norm_gss gss =
    let gss = List.map (List.map (List.sort compare)) gss in
    let gss = List.map sort_gs gss in
    let gss = sort_gs gss in
    gss

  let of_lists xss =  Array.of_list (List.map Array.of_list xss)
  let to_lists xss =  Array.to_list (Array.map Array.to_list xss)

  let compute_cpu is rs =
    let is = of_lists is and rs = of_lists rs in
    let len = Array.length is in
    let cpu = Array.init len (fun _ -> Array.make (ninst*nthreads) (-1)) in
    for x=0 to len-1 do
      for k = 0 to avail-1 do
        let i = is.(x).(k) and r = rs.(x).(k) in
        if i >= 0 then begin
          let idx = i*nthreads + r in
          cpu.(x).(idx) <- k
        end
      done
    done ;
    to_lists cpu

(*
  module Int = struct
    type t = int
    let compare = Pervasives.compare
  end

  module IntMap = MyMap.Make(Int)
  module IntSet = MySet.Make(Int)


(* Core related info *)
  type info = { sock:int ; next:int; ids:int array}

(* Initial 'next' array, indexed by core_id *)
  let alloc_next()  =
    let m = ref IntMap.empty in
    let core_id = ref 0 in
    for s = 0 to nsockets-1 do
      for c = 0 to cores_in_sock-1 do
        let i = { sock = s; next=smt; ids=topo.(s).(c) } in
        m := IntMap.add !core_id i !m ;
        incr core_id
      done
    done ;
    !m

  let find_core next ok sz =
    let rec find_rec found nfound k =
      if k >= ncores then []
      else
        let i = IntMap.find k next in
        if ok k i && i.next + sz  <= smt
        then
          let nfound = nfound+1 in
          find_rec
            (if Random.int nfound = 0 then Some k else found) nfound (k+1)
        else find_rec found nfound (k+1) in
    find_rec None 0 0

*)
  let alloc_groups (k,is,rs) gss =
    let next = Array.make ncores 0 in
    let inst = Array.make avail (-1) in
    let role = Array.make avail (-1) in
    let gss = norm_gss gss in
    if verbose > 1 then eprintf "NORM: %s\n" (pp_gss gss) ;
    let socks = ref (Misc.interval 0 nsockets) in
    for i = 0 to ninst-1 do
      let seen = Array.make ncores false in
      let undo =
        List.iter
          (fun (id,c) ->
            seen.(c) <- false ; next.(c) <- next.(c)-1 ;
            role.(id) <- -1 ; inst.(id) <- -1) in
      let alloc_group sock ids g =
        let fst = cores_in_sock * sock in
        let sz = List.length g in
        let rec do_rec core =
          if core < cores_in_sock then
            let idx = fst+core in
            if not seen.(fst+core) && sz + next.(idx) <= smt then begin
              let ids =
                List.fold_left
                  (fun ids r ->
                    let id = topo.(sock).(core).(next.(idx)) in
(*                  eprintf "id=%i -> i=%i, r=%i\n" id i r ; *)
                    inst.(id) <- i ;
                    role.(id) <- r ;
                    next.(idx) <- next.(idx)+1 ;
                    (id,idx)::ids)
                  ids g in
              seen.(idx) <- true ;
              ids
            end else do_rec (core+1)
          else ids in
        do_rec 0 in
      let rec alloc_socks rem socks ids xss ss = match xss,ss with
      | [],_ ->
          if List.length ids < nthreads then begin
            undo ids ;
            if rem > 0 then begin
              let socks = circu socks in
              alloc_socks (rem-1) socks [] gss socks  
            end
          end
      | _::_,[] -> assert false
      | xs::xss,s::ss ->          
          let ids =
            List.fold_left (alloc_group s) ids xs in
          alloc_socks rem socks ids xss ss in
      alloc_socks (nsockets-1) !socks [] gss !socks ;
      socks := circu !socks
    done ;
    (gss::k,Array.to_list inst::is,Array.to_list role::rs)


(* maxelt  : maximum cardinal of an subset in partition
   maxpart : maximum cardinal of a partition *)

let part pp_part maxelt maxpart k r =
  let rec p_rec r cp p = function
    | [] ->
        if verbose > 0 then eprintf "%s\n" (pp_part p) ;
        k r p
    | x::xs ->
        let r =
          if cp < maxpart then
            p_rec r (cp+1) ([x]::p) xs
          else r in
        let rec do_rec r prev = function
          | [] -> r
          | y::ys ->
              let r =
                let y = x::y in
                if List.length y <= maxelt then
                  p_rec r cp (y::prev@ys) xs
                else r in
              do_rec r (y::prev) ys in
        do_rec r [] p in
  function
    | [] -> assert false
    | x::xs -> p_rec r 1 [[x]] xs
              

  let dump_carray name gs t =
    O.f "static int %s[] ={" name ;
    List.iter2
      (fun g e ->
        O.f "// %s" (pp_gss g) ;
        O.o (String.concat " " (List.map (sprintf "%i,") e)))
      gs t ;
    O.o "};" ;
    O.o ""

  let dump_alloc () =
    O.o "/*" ;
    O.f " Topology: %s" (pp_intsss topo) ;
    O.o "*/" ;
    O.o "" ;
(* Partition according to sockets *)
    let sockets =
      part (fun x -> "SOCKET: " ^pp_gss x)
        cores_in_sock nsockets alloc_groups in
(* Partition according to smt *)
    let groups =
      part (fun x -> "SMT: " ^pp_gs x) smt ncores sockets in
(* Actual virtual proc numbers *)
    let (gs,is,rs) =  groups ([],[],[]) procs in
(* Dump group *)
  O.o "char *group[] = {" ;
  List.iter
    (fun g -> O.f "\"%s\"," (pp_gss g))
    gs ;
  O.o "};" ;
  O.o "" ;
  if false then begin
(* Dump instances *)
    dump_carray "inst" gs is ;
(* Dump role *)
    dump_carray "role" gs rs ;
(* Dump cpu allocation *)
  end else begin
    let cpu = compute_cpu is rs in
    dump_carray "cpu_scan" gs cpu ;
    O.f "#define SCANSZ %i" (List.length gs) ;
    O.f "#define SCANLINE %i" (ninst*nthreads) ;
    O.o "" ;
    O.o "static count_t ngroups[SCANSZ];" ;
    O.o "" ;
  end ;    
  ()

end 
