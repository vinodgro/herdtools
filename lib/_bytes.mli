type t = string
include module type of String with type t := string

val sub_string : string -> int -> int -> string
