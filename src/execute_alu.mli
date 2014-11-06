module I : interface
  clk rst
  padv_decode padv_execute padv_ctrl
  opc_alu opc_alu_secondary
  imm16 immediate immediate_sel
  decode_immediate decode_immediate_sel decode_valid decode_op_mul
  op_alu op_add op_mul op_mul_signed op_mul_unsigned op_div
  op_div_signed op_div_unsigned op_shift op_ffl1 op_setflag op_mtspr
  op_mfspr op_movhi op_jbr op_jr
  immjbr_upper pc_execute
  adder_do_sub adder_do_carry
  decode_rfa decode_rfb
  rfa rfb
  flag carry
end

module O : interface
  flag_set flag_clear carry_set carry_clear overflow_set overflow_clear
  alu_result alu_valid
  mul_result adder_result
  undriven
end

val execute_alu : calculate_branch_dest:bool -> Option.options -> Option.features -> 
  HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t


