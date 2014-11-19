(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Description: Cappuccino decode to execute module.
  - Decode to execute stage signal passing.
  - Branches are resolved (in decode stage).
  - Hazards that can not be resolved by bypassing are detected and
    bubbles are inserted on such conditions.

  Generate valid signal when stage is done.

  Copyright (C) 2012 Julius Baxter <juliusbaxter@gmail.com>
  Copyright (C) 2013 Stefan Kristiansson <stefan.kristiansson@saunalahti.fi>
  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

open Option

module Make(M : Utils.Module_cfg_signal) = struct
    
  module L = Utils.Logic(M.Bits)
  open M.Bits

  module I = interface
    clk[1]
    rst[1]
    padv[1]
    pc_decode[M.o.operand_width]
    decode_rfb[M.o.operand_width]
    execute_rfb[M.o.operand_width]
    predicted_flag[1]
    pipeline_flush[1]
    decode_opc_alu[Defines.Alu_opc.width]
    decode_opc_alu_secondary[Defines.Alu_opc.width]
    decode_imm16[Defines.imm_width]
    decode_immediate[M.o.operand_width]
    decode_immediate_sel[1]
    decode_adder_do_sub[1]
    decode_adder_do_carry[1]
    decode_immjbr_upper[10]
    decode_rfd_adr[M.o.rf_addr_width]
    decode_rfa_adr[M.o.rf_addr_width]
    decode_rfb_adr[M.o.rf_addr_width]
    ctrl_rfd_adr[M.o.rf_addr_width]
    ctrl_op_lsu_load[1]
    ctrl_op_mfspr[1]
    ctrl_op_mul[1]
    decode_rf_wb[1]
    decode_op_alu[1]
    decode_op_setflag[1]
    decode_op_jbr[1]
    decode_op_jr[1]
    decode_op_jal[1]
    decode_op_bf[1]
    decode_op_bnf[1]
    decode_op_brcond[1]
    decode_op_branch[1]
    decode_op_lsu_load[1]
    decode_op_lsu_store[1]
    decode_op_lsu_atomic[1]
    decode_lsu_length[2]
    decode_lsu_zext[1]
    decode_op_mfspr[1]
    decode_op_mtspr[1]
    decode_op_rfe[1]
    decode_op_add[1]
    decode_op_mul[1]
    decode_op_mul_signed[1]
    decode_op_mul_unsigned[1]
    decode_op_div[1]
    decode_op_div_signed[1]
    decode_op_div_unsigned[1]
    decode_op_shift[1]
    decode_op_ffl1[1]
    decode_op_movhi[1]
    decode_opc_insn[Defines.Opcode.width]
    decode_except_ibus_err[1]
    decode_except_itlb_miss[1]
    decode_except_ipagefault[1]
    decode_except_illegal[1]
    decode_except_syscall[1]
    decode_except_trap[1]
  end

  module O = interface
    execute_predicted_flag[1]
    execute_mispredict_target[M.o.operand_width]
    execute_opc_alu[Defines.Alu_opc.width]
    execute_opc_alu_secondary[Defines.Alu_opc.width]
    execute_imm16[Defines.imm_width]
    execute_immediate[M.o.operand_width]
    execute_immediate_sel[1]
    execute_adder_do_sub[1]
    execute_adder_do_carry[1]
    execute_immjbr_upper[10]
    execute_rfd_adr[M.o.rf_addr_width]
    execute_rf_wb[1]
    execute_op_alu[1]
    execute_op_setflag[1]
    execute_op_jbr[1]
    execute_op_jr[1]
    execute_op_jal[1]
    execute_op_brcond[1]
    execute_op_branch[1]
    execute_op_lsu_load[1]
    execute_op_lsu_store[1]
    execute_op_lsu_atomic[1]
    execute_lsu_length[2]
    execute_lsu_zext[1]
    execute_op_mfspr[1]
    execute_op_mtspr[1]
    execute_op_rfe[1]
    execute_op_add[1]
    execute_op_mul[1]
    execute_op_mul_signed[1]
    execute_op_mul_unsigned[1]
    execute_op_div[1]
    execute_op_div_signed[1]
    execute_op_div_unsigned[1]
    execute_op_shift[1]
    execute_op_ffl1[1]
    execute_op_movhi[1]
    execute_jal_result[M.o.operand_width]
    execute_opc_insn[Defines.Opcode.width]
    decode_branch[1]
    decode_branch_target[M.o.operand_width]
    execute_except_ibus_err[1]
    execute_except_itlb_miss[1]
    execute_except_ipagefault[1]
    execute_except_illegal[1]
    execute_except_ibus_align[1]
    execute_except_syscall[1]
    execute_except_trap[1]
    pc_execute[M.o.operand_width]
    decode_valid[1]
    decode_bubble[1]
    execute_bubble[1]
  end

  let decode_execute_cappuccino i = 

    let open I in
    let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in

    (*let reset_pc = consti M.o.operand_width M.o.reset_pc in*)

    (********************************************)
    let decode_bubble = wire 1 in
    let execute_bubble = wire 1 in
    (********************************************)

    let r x = R.reg_fb ~e:vdd ~w:1
      (fun d ->
        mux2 i.pipeline_flush gnd @@
        mux2 i.padv (mux2 decode_bubble gnd x) d)
    in
    let execute_op_alu = r i.decode_op_alu in
    let execute_op_add = r i.decode_op_add in
    let execute_op_mul = r i.decode_op_mul in
    let execute_op_mul_signed = r i.decode_op_mul_signed in
    let execute_op_mul_unsigned = r i.decode_op_mul_unsigned in
    let execute_op_div = r i.decode_op_div in
    let execute_op_div_signed = r i.decode_op_div_signed in
    let execute_op_div_unsigned = r i.decode_op_div_unsigned in
    let execute_op_shift = r i.decode_op_shift in
    let execute_op_ffl1 = r i.decode_op_ffl1 in
    let execute_op_movhi = r i.decode_op_movhi in
    let execute_op_mfspr = r i.decode_op_mfspr in (* XXX not in pipeline_flush  *)
    let execute_op_mtspr = r i.decode_op_mtspr in (* XXX  ? *)
    let execute_op_lsu_load = r i.decode_op_lsu_load in
    let execute_op_lsu_store = r i.decode_op_lsu_store in
    let execute_op_lsu_atomic = r i.decode_op_lsu_atomic in
    let execute_op_setflag = r i.decode_op_setflag in
    let execute_op_jbr = r i.decode_op_jbr in
    let execute_op_jr = r i.decode_op_jr in
    let execute_op_jal = r i.decode_op_jal in
    let execute_op_brcond = r i.decode_op_brcond in
    let execute_op_branch = r i.decode_op_branch in

    let r x = R.reg_fb ~e:vdd ~w:1 (L.pmux [ i.pipeline_flush, gnd; i.padv, x ]) in
    (* rfe is a special case, instead of pushing the pipeline full
       of nops on a decode_bubble_o, we push it full of rfes.
       The reason for this is that we need the rfe to reach control
       stage so it will cause the branch.
       It will clear itself by the pipeline_flush_i that the rfe
       will generate. *)
    let execute_op_rfe = r i.decode_op_rfe in
    let execute_rf_wb = r (mux2 decode_bubble gnd i.decode_rf_wb) in

    let r = R.reg ~e:i.padv in
    let execute_rfd_adr = r i.decode_rfd_adr in
    let execute_lsu_length = r i.decode_lsu_length in
    let execute_lsu_zext = r i.decode_lsu_zext in
    let execute_imm16 = r i.decode_imm16 in
    let execute_immediate = r i.decode_immediate in
    let execute_immediate_sel = r i.decode_immediate_sel in
    let execute_immjbr_upper = r i.decode_immjbr_upper in
    let execute_opc_alu = r i.decode_opc_alu in
    let execute_opc_alu_secondary = r i.decode_opc_alu_secondary in

    let r r x = 
      R.reg_fb ~rv:r ~cv:r ~e:vdd ~w:(width x)
        (fun d ->
          mux2 i.pipeline_flush r @@
          mux2 i.padv (mux2 decode_bubble r x) d)
    in

    let execute_opc_insn = r (consti Defines.Opcode.width Defines.Opcode.nop) i.decode_opc_insn in
    let execute_adder_do_sub = r gnd i.decode_adder_do_sub in
    let execute_adder_do_carry = r gnd i.decode_adder_do_carry in

    (*  Decode for system call exception *)
    let r f x = if f then R.reg ~e:i.padv x else gnd in
    let execute_except_syscall = r M.f.syscall i.decode_except_syscall in
    let execute_except_trap = r M.f.trap i.decode_except_trap in

    let decode_valid = R.reg ~e:vdd i.padv in

    (* Branch detection *)
    let ctrl_to_decode_interlock = 
      let fmul = if M.f.multiplier = Multiplier_pipelined then vdd else gnd in
      (i.ctrl_op_lsu_load |: i.ctrl_op_mfspr |: i.ctrl_op_mul &: fmul) &:
      ((i.decode_rfa_adr ==: i.ctrl_rfd_adr) |: (i.decode_rfb_adr ==: i.ctrl_rfd_adr)) 
    in
 
    let branch_to_imm = 
      (i.decode_op_jbr &:
        (* l.j/l.jal *)
        ((~: (reduce (|:) @@ bits i.decode_opc_insn.[2:1])) |:
          (* l.bf/bnf and flag is right *)
          (i.decode_opc_insn.[2:2] ==: i.predicted_flag))) 
    in
 
    let branch_to_imm_target = 
      i.pc_decode +: 
        (repeat i.decode_immjbr_upper.[9:9] 4 @:
         i.decode_immjbr_upper @:
         i.decode_imm16 @: 
         zero 2)
    in
    
    let branch_to_reg = 
      i.decode_op_jr &:
        (~: (ctrl_to_decode_interlock |:
          execute_rf_wb &:
          (i.decode_rfb_adr ==: execute_rfd_adr))) 
    in
 
    let decode_branch = (branch_to_imm |: branch_to_reg) &: (~: (i.pipeline_flush)) in
 
    let decode_branch_target = 
      mux2 branch_to_imm branch_to_imm_target @@
      (* If a bubble have been pushed out to get
         the instruction that will write the
         branch target to control stage, then we
         need to use the register result from
         execute stage instead of decode stage. *)
      mux2 (execute_bubble |: execute_op_jr) i.execute_rfb
      i.decode_rfb
    in
 
    let decode_except_ibus_align = 
      decode_branch &: (reduce (|:) @@ bits decode_branch_target.[1:0]) 
    in
 
    let next_pc_after_branch_insn = 
      if M.f.delayslot then i.pc_decode +:. 8 
      else i.pc_decode +:. 4 
    in
 
    let decode_mispredict_target = 
      mux2 (i.decode_op_bf &: (~: (i.predicted_flag)) |: i.decode_op_bnf &: i.predicted_flag)
         branch_to_imm_target 
         next_pc_after_branch_insn 
    in

    (* Decode Illegal instruction *)
    let r x = R.reg ~e:i.padv x in
    let execute_except_illegal = r i.decode_except_illegal in
    let execute_except_ibus_err = r i.decode_except_ibus_err in
    let execute_except_itlb_miss = r i.decode_except_itlb_miss in
    let execute_except_ipagefault = r i.decode_except_ipagefault in
    let execute_except_ibus_align = r decode_except_ibus_align in
    let pc_execute = r i.pc_decode in

    (* Forward branch prediction signals to execute stage *)
    let r x = R.reg ~e:(i.padv &: i.decode_op_brcond) x in
    let execute_mispredict_target = r decode_mispredict_target in
    let execute_predicted_flag = r i.predicted_flag in

    (* Calculate the link register result
       TODO: investigate if the ALU adder can be used for this without
       introducing critical paths *)
    let execute_jal_result = R.reg ~e:i.padv next_pc_after_branch_insn in

    (* Detect the situation where there is an instruction in execute stage
       that will produce it's result in control stage (i.e. load and mfspr),
       and an instruction currently in decode stage needing it's result as
       input in execute stage.
       Also detect the situation where there is a jump to register in decode
       stage and an instruction in execute stage that will write to that
       register.
      
       A bubble is also inserted when an rfe instruction is in decode stage,
       the main purpose of this is to stall fetch while the rfe is propagating
       up to ctrl stage. *)
    
    let () = 
      let fmul = if M.f.multiplier = Multiplier_pipelined then vdd else gnd in
      decode_bubble <== 
      ((
        (* load/mfspr/mul *)
        (execute_op_lsu_load |: execute_op_mfspr |:
         execute_op_mul &:
         fmul) &:
        ((i.decode_rfa_adr ==: execute_rfd_adr) |:
         (i.decode_rfb_adr ==: execute_rfd_adr)) |:
        (* mul *)
        fmul &:
        (i.decode_op_mul &:
         (ctrl_to_decode_interlock |:
          execute_rf_wb &:
          ((i.decode_rfa_adr ==: execute_rfd_adr) |:
           (i.decode_rfb_adr ==: execute_rfd_adr)))) |:
        (* jr *)
        i.decode_op_jr &:
        (ctrl_to_decode_interlock |:
         execute_rf_wb &:
         (i.decode_rfb_adr ==: execute_rfd_adr)) |:
        (* atomic store *)
        execute_op_lsu_store &: execute_op_lsu_atomic |:
        (* rfe *)
        i.decode_op_rfe
      ) &: i.padv)
    in
    
    let () = execute_bubble <== R.reg_fb ~e:vdd ~w:1 
      (L.pmux [ i.pipeline_flush, gnd; i.padv, decode_bubble ]) 
    in

    O.{
      execute_predicted_flag;
      execute_mispredict_target;
      execute_opc_alu;
      execute_opc_alu_secondary;
      execute_imm16;
      execute_immediate;
      execute_immediate_sel;
      execute_adder_do_sub;
      execute_adder_do_carry;
      execute_immjbr_upper;
      execute_rfd_adr;
      execute_rf_wb;
      execute_op_alu;
      execute_op_setflag;
      execute_op_jbr;
      execute_op_jr;
      execute_op_jal;
      execute_op_brcond;
      execute_op_branch;
      execute_op_lsu_load;
      execute_op_lsu_store;
      execute_op_lsu_atomic;
      execute_lsu_length;
      execute_lsu_zext;
      execute_op_mfspr;
      execute_op_mtspr;
      execute_op_rfe;
      execute_op_add;
      execute_op_mul;
      execute_op_mul_signed;
      execute_op_mul_unsigned;
      execute_op_div;
      execute_op_div_signed;
      execute_op_div_unsigned;
      execute_op_shift;
      execute_op_ffl1;
      execute_op_movhi;
      execute_jal_result;
      execute_opc_insn;
      decode_branch;
      decode_branch_target;
      execute_except_ibus_err;
      execute_except_itlb_miss;
      execute_except_ipagefault;
      execute_except_illegal;
      execute_except_ibus_align;
      execute_except_syscall;
      execute_except_trap;
      pc_execute;
      decode_valid;
      decode_bubble;
      execute_bubble;
    }

  module Inst = M.Inst(I)(O)
  let decode_execute_cappuccino_inst = 
    Inst.inst "decode_execute_cappuccino" decode_execute_cappuccino 

end

