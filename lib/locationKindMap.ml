open Printf

(** Map each location to a "location kind".

This is used in the CPP11 model. *)


type location_kind = 
  | LK_mutex
  | LK_atomic
  | LK_nonatomic

let pp_location_kind = function
  | LK_mutex -> "mutex"
  | LK_atomic -> "atomic"
  | LK_nonatomic -> "nonatomic"


type lk_map = (string * location_kind) list


let pp_lk_map m = 
  let str_list = 
    List.map 
      (fun (x,y) -> sprintf "%s:%s" x (pp_location_kind y)) m 
  in
  String.concat "; " str_list

let is_atomicloc lkm x =
  let lk =
    try
      List.assoc x lkm
    with 
      Not_found -> 
      Warn.fatal "Location %s not in location-kind map" x
  in match lk with
  | LK_atomic -> true
  | _ -> false

let is_nonatomicloc lkm x =
  let lk =
    try
      List.assoc x lkm
    with 
      Not_found -> 
      Warn.fatal "Location %s not in location-kind map" x
  in match lk with
  | LK_nonatomic -> true
  | _ -> false

let is_mutexloc lkm x =
  let lk =
    try
      List.assoc x lkm
    with 
      Not_found -> 
      Warn.fatal "Location %s not in location-kind map" x
  in match lk with
  | LK_mutex -> true
  | _ -> false
