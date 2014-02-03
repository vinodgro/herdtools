(* Mostly from C++11 *)

module type S = sig
  type a
  type b = Fence of OpenCLBase.mem_order * OpenCLBase.mem_scope
  val a_to_b : a -> b
end
