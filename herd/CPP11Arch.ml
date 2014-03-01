(*Pretty much just the PPC arch*)

module Make (C:Arch.Config) (V:Value.S) = struct
    include CPP11Base

(* Now global locations, that include reservations *)
    module V = V

    include ArchExtra.Make(C)        
	(struct
	  module V = V 

	  type arch_reg = reg
	  let pp_reg = pp_reg
	  let reg_compare = reg_compare

	  type arch_instruction = instruction
	end)
  end
