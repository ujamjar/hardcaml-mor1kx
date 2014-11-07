(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

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


