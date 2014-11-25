(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Description: execute to control stage signal passing

  Generate valid signal when stage is done

  Copyright (C) 2012 Authors

  Author(s): Julius Baxter <juliusbaxter@gmail.com>
             Stefan Kristiansson <stefan.kristiansson@saunalahti.fi>
             Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)
open Option

module Make(M : Utils.Module_cfg_signal) = struct
    
  open M.Bits

  module I = interface
    clk[1]
    rst[1]
    padv[1]
    padv_ctrl[1]
    execute_except_ibus_err[1]
    execute_except_itlb_miss[1]
    execute_except_ipagefault[1]
    execute_except_illegal[1]
    execute_except_ibus_align[1]
    execute_except_syscall[1]
    lsu_except_dbus[1]
    lsu_except_align[1]
    lsu_except_dtlb_miss[1]
    lsu_except_dpagefault[1]
    execute_except_trap[1]
    pipeline_flush[1]
    op_mul[1]
    op_lsu_load[1]
    op_lsu_store[1]
    op_lsu_atomic[1]
    lsu_length[2]
    lsu_zext[1]
    op_mfspr[1]
    op_mtspr[1]
    alu_valid[1]
    lsu_valid[1]
    op_jr[1]
    op_jal[1]
    op_rfe[1]
    alu_result[M.o.operand_width]
    adder_result[M.o.operand_width]
    rfb[M.o.operand_width]
    execute_jal_result[M.o.operand_width]
    flag_set[1]
    flag_clear[1]
    carry_set[1]
    carry_clear[1]
    overflow_set[1]
    overflow_clear[1]
    pc_execute[M.o.operand_width]
    execute_rf_wb[1]
    execute_rfd_adr[M.o.rf_addr_width]
    execute_bubble[1]
    ctrl_mfspr_ack[1]
    ctrl_mtspr_ack[1]
  end

  module O = interface
    ctrl_rf_wb[1]
    wb_rf_wb[1]
    ctrl_rfd_adr[M.o.rf_addr_width]
    wb_rfd_adr[M.o.rf_addr_width]
    ctrl_alu_result[M.o.operand_width]
    ctrl_lsu_adr[M.o.operand_width]
    ctrl_rfb[M.o.operand_width]
    ctrl_flag_set[1]
    ctrl_flag_clear[1]
    ctrl_carry_set[1]
    ctrl_carry_clear[1]
    ctrl_overflow_set[1]
    ctrl_overflow_clear[1]
    pc_ctrl[M.o.operand_width]
    ctrl_op_mul[1]
    ctrl_op_lsu_load[1]
    ctrl_op_lsu_store[1]
    ctrl_op_lsu_atomic[1]
    ctrl_lsu_length[2]
    ctrl_lsu_zext[1]
    ctrl_op_mfspr[1]
    ctrl_op_mtspr[1]
    ctrl_op_rfe[1]
    ctrl_except_ibus_err[1]
    ctrl_except_itlb_miss[1]
    ctrl_except_ipagefault[1]
    ctrl_except_ibus_align[1]
    ctrl_except_illegal[1]
    ctrl_except_syscall[1]
    ctrl_except_dbus[1]
    ctrl_except_dtlb_miss[1]
    ctrl_except_dpagefault[1]
    ctrl_except_align[1]
    ctrl_except_trap[1]
    execute_valid[1]
    ctrl_valid[1]
  end

  let execute_ctrl_cappuccino i = 
    let open I in
    let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in

    let reset_pc = consti M.o.operand_width M.o.reset_pc in

    let r x = R.reg_fb ~e:vdd ~w:1 (pmux [ i.pipeline_flush, gnd; i.padv, x ]) in
    let ctrl_except_ibus_err = r i.execute_except_ibus_err in
    let ctrl_except_itlb_miss = r i.execute_except_itlb_miss in
    let ctrl_except_ipagefault = r i.execute_except_ipagefault in
    let ctrl_except_ibus_align = r i.execute_except_ibus_align in
    let ctrl_except_illegal = r i.execute_except_illegal in
    let ctrl_except_syscall = r i.execute_except_syscall in
    let ctrl_except_trap = r i.execute_except_trap in
    let r x = R.reg ~e:vdd (mux2 i.pipeline_flush gnd x) in
    let ctrl_except_dbus = r i.lsu_except_dbus in
    let ctrl_except_align = r i.lsu_except_align in
    let r x = R.reg_fb ~e:vdd ~w:1 (fun d -> mux2 i.pipeline_flush d x) in
    let ctrl_except_dtlb_miss = r i.lsu_except_dtlb_miss in
    let ctrl_except_dpagefault = r i.lsu_except_dpagefault in

    let ctrl_alu_result = R.reg ~e:i.padv (mux2 i.op_jal i.execute_jal_result i.alu_result) in
    let ctrl_lsu_adr = R.reg ~e:(i.padv &: (i.op_lsu_store |: i.op_lsu_load)) i.adder_result in
    let ctrl_rfb = R.reg ~e:i.padv i.rfb in

    let r = R.reg ~e:i.padv in
    let ctrl_flag_set = r i.flag_set in
    let ctrl_flag_clear = r i.flag_clear in
    let ctrl_carry_set = r i.carry_set in
    let ctrl_carry_clear = r i.carry_clear in
    let ctrl_overflow_set = r i.overflow_set in
    let ctrl_overflow_clear = r i.overflow_clear in

    (* pc_ctrl should not advance when a nop bubble moves from execute to ctrl/mem stage *)
    let pc_ctrl = R.reg ~rv:reset_pc ~cv:reset_pc 
      ~e:(i.padv &: (~: (i.execute_bubble))) i.pc_execute 
    in

    (* The pipeline flush comes when the instruction that has caused
       an exception or the instruction that has been interrupted is in
       ctrl stage, so the padv_execute signal has to have higher prioity
       than the pipeline flush in order to not accidently kill a valid
       instruction coming in from execute stage. *)
    let ctrl_op_mul = 
      if M.f.multiplier = Multiplier_pipelined then
        R.reg_fb ~e:vdd ~w:1 (pmux [ i.padv, i.op_mul; i.pipeline_flush, gnd; ])
      else
        gnd
    in

    let r x = R.reg_fb ~e:vdd ~w:1 (pmux [ i.padv, x; i.pipeline_flush, gnd ]) in
    let ctrl_op_mfspr = r i.op_mfspr in
    let ctrl_op_mtspr = r i.op_mtspr in
    let ctrl_op_rfe = r i.op_rfe in

    let r x = R.reg_fb ~e:vdd ~w:1
      (pmux [
        (ctrl_except_align |: ctrl_except_dbus |:
          ctrl_except_dtlb_miss |: ctrl_except_dpagefault), gnd;
        i.padv, x;
        i.pipeline_flush, gnd;
      ])
    in
    let ctrl_op_lsu_load = r i.op_lsu_load in
    let ctrl_op_lsu_store = r i.op_lsu_store in
    let ctrl_op_lsu_atomic = r i.op_lsu_atomic in

    let ctrl_lsu_length = R.reg ~e:i.padv i.lsu_length in
    let ctrl_lsu_zext = R.reg ~e:i.padv i.lsu_zext in

    let ctrl_rf_wb = R.reg_fb ~e:vdd ~w:1
      (pmux [
        i.padv, i.execute_rf_wb;
        (* Deassert the write enable when the "bus" access is done, to avoid:
          1) Writing multiple times to RF
          2) Signaling a need to bypass from control stage, when it really
              should be a bypass from wb stage. *)
        (ctrl_op_mfspr &: i.ctrl_mfspr_ack |: ctrl_op_lsu_load &: i.lsu_valid), gnd;
        i.pipeline_flush, gnd;
      ])
    in

    let ctrl_rfd_adr = R.reg ~e:i.padv i.execute_rfd_adr in

    (* load and mfpsr can stall from ctrl stage, so we have to hold off the
       write back on them *)
    let wb_rf_wb = R.reg ~e:vdd
      (pmux [
        i.pipeline_flush, gnd;
        ctrl_op_mfspr, ctrl_rf_wb &: i.ctrl_mfspr_ack;
        ctrl_op_lsu_load, ctrl_rf_wb &: i.lsu_valid;
      ] (ctrl_rf_wb &: i.padv_ctrl))
    in

    let wb_rfd_adr = R.reg ~e:vdd ctrl_rfd_adr in

    (* LSU or MTSPR/MFSPR can stall from ctrl stage *)
    let ctrl_stall = 
      (ctrl_op_lsu_load |: ctrl_op_lsu_store) &:
      (~: (i.lsu_valid)) |:
      ctrl_op_mfspr &: (~: (i.ctrl_mfspr_ack)) |:
      ctrl_op_mtspr &: (~: (i.ctrl_mtspr_ack)) 
    in
    let ctrl_valid = ~: ctrl_stall in

    (* Execute stage can be stalled from ctrl stage and by ALU *)
    let execute_stall = ctrl_stall |: (~: (i.alu_valid)) in
    let execute_valid = ~: execute_stall in

    O.{
      ctrl_rf_wb;
      wb_rf_wb;
      ctrl_rfd_adr;
      wb_rfd_adr;
      ctrl_alu_result;
      ctrl_lsu_adr;
      ctrl_rfb;
      ctrl_flag_set;
      ctrl_flag_clear;
      ctrl_carry_set;
      ctrl_carry_clear;
      ctrl_overflow_set;
      ctrl_overflow_clear;
      pc_ctrl;
      ctrl_op_mul;
      ctrl_op_lsu_load;
      ctrl_op_lsu_store;
      ctrl_op_lsu_atomic;
      ctrl_lsu_length;
      ctrl_lsu_zext;
      ctrl_op_mfspr;
      ctrl_op_mtspr;
      ctrl_op_rfe;
      ctrl_except_ibus_err;
      ctrl_except_itlb_miss;
      ctrl_except_ipagefault;
      ctrl_except_ibus_align;
      ctrl_except_illegal;
      ctrl_except_syscall;
      ctrl_except_dbus;
      ctrl_except_dtlb_miss;
      ctrl_except_dpagefault;
      ctrl_except_align;
      ctrl_except_trap;
      execute_valid;
      ctrl_valid;
    }

  module Inst = M.Inst(I)(O)
  let execute_ctrl_cappuccino_inst = Inst.inst "execute_ctrl_cappuccino" execute_ctrl_cappuccino 

end


