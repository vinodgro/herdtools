(*Mostly from ARM*)

module type S = sig
  type a
  type b = unit
  val a_to_b : a -> b
end
