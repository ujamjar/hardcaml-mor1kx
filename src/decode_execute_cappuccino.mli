(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Authors

  Author(s): Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module Make(M : Utils.Module_cfg_signal) : sig
  module I : interface
    clk
    rst
    padv
    pc_decode
    decode_rfb
    execute_rfb
    predicted_flag
    pipeline_flush
    decode_opc_alu
    decode_opc_alu_secondary
    decode_imm16
    decode_immediate
    decode_immediate_sel
    decode_adder_do_sub
    decode_adder_do_carry
    decode_immjbr_upper
    decode_rfd_adr
    decode_rfa_adr
    decode_rfb_adr
    ctrl_rfd_adr
    ctrl_op_lsu_load
    ctrl_op_mfspr
    ctrl_op_mul
    decode_rf_wb
    decode_op_alu
    decode_op_setflag
    decode_op_jbr
    decode_op_jr
    decode_op_jal
    decode_op_bf
    decode_op_bnf
    decode_op_brcond
    decode_op_branch
    decode_op_lsu_load
    decode_op_lsu_store
    decode_op_lsu_atomic
    decode_lsu_length
    decode_lsu_zext
    decode_op_mfspr
    decode_op_mtspr
    decode_op_rfe
    decode_op_add
    decode_op_mul
    decode_op_mul_signed
    decode_op_mul_unsigned
    decode_op_div
    decode_op_div_signed
    decode_op_div_unsigned
    decode_op_shift
    decode_op_ffl1
    decode_op_movhi
    decode_opc_insn
    decode_except_ibus_err
    decode_except_itlb_miss
    decode_except_ipagefault
    decode_except_illegal
    decode_except_syscall
    decode_except_trap
  end
  module O : interface
    execute_predicted_flag
    execute_mispredict_target
    execute_opc_alu
    execute_opc_alu_secondary
    execute_imm16
    execute_immediate
    execute_immediate_sel
    execute_adder_do_sub
    execute_adder_do_carry
    execute_immjbr_upper
    execute_rfd_adr
    execute_rf_wb
    execute_op_alu
    execute_op_setflag
    execute_op_jbr
    execute_op_jr
    execute_op_jal
    execute_op_brcond
    execute_op_branch
    execute_op_lsu_load
    execute_op_lsu_store
    execute_op_lsu_atomic
    execute_lsu_length
    execute_lsu_zext
    execute_op_mfspr
    execute_op_mtspr
    execute_op_rfe
    execute_op_add
    execute_op_mul
    execute_op_mul_signed
    execute_op_mul_unsigned
    execute_op_div
    execute_op_div_signed
    execute_op_div_unsigned
    execute_op_shift
    execute_op_ffl1
    execute_op_movhi
    execute_jal_result
    execute_opc_insn
    decode_branch
    decode_branch_target
    execute_except_ibus_err
    execute_except_itlb_miss
    execute_except_ipagefault
    execute_except_illegal
    execute_except_ibus_align
    execute_except_syscall
    execute_except_trap
    pc_execute
    decode_valid
    decode_bubble
    execute_bubble
  end

  val decode_execute_cappuccino : M.Bits.t I.t -> M.Bits.t O.t
  val decode_execute_cappuccino_inst : M.Bits.t I.t -> M.Bits.t O.t
end
