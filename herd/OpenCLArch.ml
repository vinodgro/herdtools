(* Pretty much just the C++11 arch *)

module Make (C:Arch.Config) (V:Value.S)
=
  struct
    include OpenCLBase

(* Now global locations, that include reservations *)
    module V = V

    include ArchExtra.Make(C)        
	(struct
	  module V = V 

	  type arch_reg = reg
	  let pp_reg = pp_reg
	  let pp_atrb = pp_atrb
	  let reg_compare = reg_compare

	  type arch_instruction = instruction
	  type arch_atrb = atrb
	end)
  end
