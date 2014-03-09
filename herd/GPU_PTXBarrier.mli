(*Mostly from ARM*)

module type S = sig
  type a
  type b = Membar of GPU_PTXBase.bar_scope
  val a_to_b : a -> b
end
