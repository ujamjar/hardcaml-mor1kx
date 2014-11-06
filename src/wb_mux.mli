module Cappuccino : sig

  module I : interface
    clk rst
    alu_result lsu_result mul_result
    op_mul op_lsu_load 
  end

  module O : interface
    rf_result
  end

  val wb_mux : HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t

end

module Espresso : sig

  module I : interface
   alu_result lsu_result
   pc_fetch_next
   spr
   op_jal op_lsu_load op_mfspr
  end

  module O : interface
   rf_result
  end

  val wb_mux : HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t

end


