(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Description: RF writeback mux 

  Choose between ALU and LSU input.  espresso is all combinatorial

  Copyright (C) 2012 Authors

  Author(s): Julius Baxter <juliusbaxter@gmail.com>

***************************************************************************** *)

(* note; a few ununused signals have been remove from the interfaces *)

open HardCaml.Signal.Comb

let operand_width = 32

module Cappuccino = struct

  module I = interface
    clk[1] rst[1]
    alu_result[operand_width] lsu_result[operand_width] mul_result[operand_width]
    op_mul[1] op_lsu_load[1] 
  end

  module O = interface
    rf_result[operand_width]
  end

  let wb_mux i = 
    let open I in
    let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in
    let rf_result = R.reg ~e:vdd @@ mux2 i.op_lsu_load i.lsu_result i.alu_result in
    let wb_op_mul = R.reg ~e:vdd i.op_mul in
    O.({ rf_result = mux2 wb_op_mul i.mul_result rf_result })

end

module Espresso = struct

  module I = interface
   alu_result[operand_width] lsu_result[operand_width]
   pc_fetch_next[operand_width]
   spr[operand_width]
   op_jal[1] op_lsu_load[1] op_mfspr[1]
  end

  module O = interface
   rf_result[operand_width]
  end

  let wb_mux i = 
    let open I in
    O.({
      rf_result = 
        mux2 i.op_lsu_load i.lsu_result @@
        mux2 i.op_mfspr i.spr @@
        mux2 i.op_jal i.pc_fetch_next @@
        i.alu_result
    })

end


