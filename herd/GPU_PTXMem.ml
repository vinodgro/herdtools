(*Mostly taken from the X86 Model*)

module type Config = sig
  val model : Model.t
  include Model.Config
end

module Make
    (O:Config)
    (S:Sem.Semantics)
    (B:GPU_PTXBarrier.S with type a = S.barrier)
 :
    XXXMem.S with
module S = S
    =
  struct

    open Model

    let model = O.model

    module S = S

    module ModelConfig = (O : Model.Config)

    let check_event_structure test = match O.model with
    | Generic m ->
        let module X =
          MachModelChecker.Make
            (struct
              let m = m
              include ModelConfig
             end)(S)(AllBarrier.FromGPU_PTX(B)) in
        X.check_event_structure test
    | File _ -> assert false
    | m ->
        Warn.fatal "Model %s not implemented for PTX" (Model.pp m)
end
