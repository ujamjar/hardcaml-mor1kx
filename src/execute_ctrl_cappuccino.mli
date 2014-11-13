(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2012 Authors

  Author(s): Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)


module Make(M : Utils.Module_cfg_signal) : sig
  module I : interface
    clk
    rst
    padv
    padv_ctrl
    execute_except_ibus_err
    execute_except_itlb_miss
    execute_except_ipagefault
    execute_except_illegal
    execute_except_ibus_align
    execute_except_syscall
    lsu_except_dbus
    lsu_except_align
    lsu_except_dtlb_miss
    lsu_except_dpagefault
    execute_except_trap
    pipeline_flush
    op_mul
    op_lsu_load
    op_lsu_store
    op_lsu_atomic
    lsu_length
    lsu_zext
    op_mfspr
    op_mtspr
    alu_valid
    lsu_valid
    op_jr
    op_jal
    op_rfe
    alu_result
    adder_result
    rfb
    execute_jal_result
    flag_set
    flag_clear
    carry_set
    carry_clear
    overflow_set
    overflow_clear
    pc_execute
    execute_rf_wb
    execute_rfd_adr
    execute_bubble
    ctrl_mfspr_ack
    ctrl_mtspr_ack
  end
  module O : interface
    ctrl_rf_wb
    wb_rf_wb
    ctrl_rfd_adr
    wb_rfd_adr
    ctrl_alu_result
    ctrl_lsu_adr
    ctrl_rfb
    ctrl_flag_set
    ctrl_flag_clear
    ctrl_carry_set
    ctrl_carry_clear
    ctrl_overflow_set
    ctrl_overflow_clear
    pc_ctrl
    ctrl_op_mul
    ctrl_op_lsu_load
    ctrl_op_lsu_store
    ctrl_op_lsu_atomic
    ctrl_lsu_length
    ctrl_lsu_zext
    ctrl_op_mfspr
    ctrl_op_mtspr
    ctrl_op_rfe
    ctrl_except_ibus_err
    ctrl_except_itlb_miss
    ctrl_except_ipagefault
    ctrl_except_ibus_align
    ctrl_except_illegal
    ctrl_except_syscall
    ctrl_except_dbus
    ctrl_except_dtlb_miss
    ctrl_except_dpagefault
    ctrl_except_align
    ctrl_except_trap
    execute_valid
    ctrl_valid
  end

  val execute_ctrl_cappuccino : M.Bits.t I.t -> M.Bits.t O.t
  val execute_ctrl_cappuccino_inst : M.Bits.t I.t -> M.Bits.t O.t
end
