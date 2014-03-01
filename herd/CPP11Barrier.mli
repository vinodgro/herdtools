(*Mostly from ARM*)

module type S = sig
  type a
  type b = Fence of CPP11Base.mem_order
  val a_to_b : a -> b
end
