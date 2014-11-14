(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Description: mor1kx espresso pipeline control unit

  inputs from execute stage

  generate pipeline controls

  manage SPRs

  issue addresses for exceptions to fetch stage
  control branches going to fetch stage

  contains tick timer

  contains PIC logic

  Copyright (C) 2012 Authors

  Author(s): Julius Baxter <juliusbaxter@gmail.com>
             Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module Espresso = struct

  module Make(M : Utils.Module_cfg_signal) = struct

    open M.Bits
    open Option
    module L = Utils.Logic(M.Bits)
    module Spr = Spr.Make(M.Bits)

    module I = interface
      clk[1]
      rst[1]
      ctrl_alu_result[M.o.operand_width]
      ctrl_rfb[M.o.operand_width]
      ctrl_flag_set[1] 
      ctrl_flag_clear[1]
      ctrl_opc_insn[Defines.Opcode.width]
      pc_fetch[M.o.operand_width]
      fetch_advancing[1]
      except_ibus_err[1]
      except_illegal[1]
      except_syscall[1] 
      except_dbus[1]
      except_trap[1] 
      except_align[1]
      next_fetch_done[1]
      alu_valid[1] 
      lsu_valid[1]
      op_lsu_load[1] 
      op_lsu_store[1]
      op_jr[1] 
      op_jbr[1]
      irq[32]
      carry_set[1]
      carry_clear[1]
      overflow_set[1]
      overflow_clear[1]
      du_addr[16]
      du_stb[1]
      du_dat[M.o.operand_width]
      du_we[1]
      du_stall[1]
      spr_bus_dat_dc[M.o.operand_width]
      spr_bus_ack_dc[1]
      spr_bus_dat_ic[M.o.operand_width]
      spr_bus_ack_ic[1]
      spr_bus_dat_dmmu[M.o.operand_width]
      spr_bus_ack_dmmu[1]
      spr_bus_dat_immu[M.o.operand_width]
      spr_bus_ack_immu[1]
      spr_bus_dat_mac[M.o.operand_width]
      spr_bus_ack_mac[1]
      spr_bus_dat_pmu[M.o.operand_width]
      spr_bus_ack_pmu[1]
      spr_bus_dat_pcu[M.o.operand_width]
      spr_bus_ack_pcu[1]
      spr_bus_dat_fpu[M.o.operand_width]
      spr_bus_ack_fpu[1]
      multicore_coreid[M.o.operand_width]
      rf_wb[1]
    end

    module O = interface
      flag[1]
      spr_npc[M.o.operand_width]
      spr_ppc[M.o.operand_width]
      mfspr_dat[M.o.operand_width]
      ctrl_mfspr_we[1]
      carry[1]
      pipeline_flush[1]
      padv_fetch[1]
      padv_decode[1]
      padv_execute[1]
      fetch_take_exception_branch[1]
      exception_taken[1]
      execute_waiting[1]
      stepping[1]
      du_dat[M.o.operand_width]
      du_ack[1]
      du_stall[1]
      du_restart_pc[M.o.operand_width]
      du_restart[1]
      spr_bus_addr[16]
      spr_bus_we[1]
      spr_bus_stb[1]
      spr_bus_dat[M.o.operand_width]
      spr_sr[16]
      ctrl_branch_occur[1]
      rf_we[1]
    end

    let ctrl i = 
      let open I in
      let open HardCaml.Signal.Guarded in
      let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in

      let reset_pc = consti M.o.operand_width M.o.reset_pc in
      let b = i.ctrl_rfb in

      (*******************************)
      let spr_sr = Array.init Spr.mor1kx_sr_width (fun _ -> R.g_reg ~e:vdd 1) in

      let exception_r = gnd in
      let op_rfe = gnd in
      let doing_rfe = gnd in
      let exception_taken = gnd in
      let except_ibus_align = gnd in
      let except_ticktimer = gnd in
      let except_range = gnd in
      let except_pic = gnd in
      let take_exception= gnd in
      let stepping = gnd in
      let ctrl_branch_occur = gnd in
      let decode_execute_halt = gnd in
      let rfete = gnd in
      let spr_epcr = gnd in
      let exception_pc_addr = gnd in
      let flag = gnd in
      let fetch_advance = gnd in
      let execute_delay_slot = gnd in
      let execute_done = gnd in
      let delay_slot_rf_we_done = gnd in
      let op_mfspr = gnd in
      let spr_esr = gnd in
      let except_ticktimer_nonsrmasked = gnd in
      let except_pic_nonsrmasked = gnd in
      let fetch_take_exception_branch = gnd in
      let waiting_for_except_fetch = gnd in
      let execute_waiting = gnd in
      let cpu_stall = gnd in
      let pstep = gnd in
      let doing_rfe_r = gnd in
      let waiting_for_getch = gnd in
      let waiting_for_fetch = gnd in
      let du_access = gnd in
      let du_restart_from_stall = gnd in
      let spr_ppc = gnd in
      let deassert_doing_rfe = gnd in
      (*******************************)

      let ctrl_branch_exception = 
        (exception_r |: (op_rfe |: doing_rfe)) &: (~: exception_taken) 
      in
      let exception_pending = 
        (i.except_ibus_err |: except_ibus_align |:
         i.except_illegal |: i.except_syscall |:
         i.except_dbus |: i.except_align |:
         except_ticktimer |: except_range |:
         except_pic |: i.except_trap) 
      in

      let exn = exception_pending in

      let fetch_take_exception_branch_o =  
        (take_exception |: op_rfe) &: (~: stepping) 
      in

      let execute_stage_exceptions = 
        i.except_dbus |: i.except_align |: except_range 
      in
      let decode_stage_exceptions = i.except_trap |: i.except_illegal in

      let exception_re = exn &: (~: exception_r) &: (~: exception_taken) in

      let deassert_decode_execute_halt = ctrl_branch_occur &: decode_execute_halt in

      let ctrl_branch_except_pc = 
        mux2 ((op_rfe |: doing_rfe) &: (~: rfete)) 
          spr_epcr 
          exception_pc_addr 
      in

      (* Exceptions take precedence *)
      let ctrl_branch_occur = 
        (* instruction is branch, and flag is right *)
        (i.op_jbr &:
         (* is l.j or l.jal *)
         ((~: (reduce (|:) @@ bits i.ctrl_opc_insn.[2:1])) |:
          (* is l.bf/bnf and flag is right *)
          (i.ctrl_opc_insn.[2:2] ==: flag))) |:
        (i.op_jr &: (~: (except_ibus_align))) 
      in

      let ctrl_branch_occur_o = 
        (* Usual branch signaling *)
        ((ctrl_branch_occur |: ctrl_branch_exception) &: fetch_advance) |:
        (* Need to tell the fetch stage to branch
           when it gets the next instruction because
           there was fetch stalls between the branch
           and the delay slot insn *)
        (execute_delay_slot) 
      in

      let ctrl_branch_target_o = 
        mux2 ctrl_branch_exception ctrl_branch_except_pc @@
        (* jump or branch? *)
        mux2 i.op_jbr i.ctrl_alu_result i.ctrl_rfb
      in

      (* Do writeback when we register our output to the next stage, or if we're doing mfspr *)
      let rf_we_o = 
        (execute_done &: (~: delay_slot_rf_we_done)) &:
        ((i.rf_wb &: (~: op_mfspr) &: 
            (~: ((i.op_lsu_load |: i.op_lsu_store) &:
            i.except_dbus |: i.except_align))) |:
          (op_mfspr)) in

      let except_range = 
        if M.f.range then
          spr_sr.(Spr.sr_ove)#q &: 
            (spr_sr.(Spr.sr_ov)#q |: i.overflow_set &: execute_done) &: 
            (~: doing_rfe) 
        else 
          gnd
      in 

      (* Check for unaligned jump address from register *)
      let except_ibus_align = i.op_jr &: (reduce (|:) @@ bits i.ctrl_rfb.[1:0]) in

      (* Return from exception to exception (if pending tick or PIC ints) *)
      let rfete = 
        (spr_esr.[Spr.sr_iee:Spr.sr_iee] &: except_pic_nonsrmasked) |:
        (spr_esr.[Spr.sr_tee:Spr.sr_tee] &: except_ticktimer_nonsrmasked) 
      in

      let exception_pc_addr = 
        let sel = concat [
          i.except_ibus_err;
          i.except_illegal;
          i.except_align;
          except_ibus_align;
          i.except_syscall;
          i.except_trap;
          i.except_dbus;
          except_range;
          except_pic_nonsrmasked;
          except_ticktimer_nonsrmasked;
        ] in
        let mux l d = 
          let c x = consti Defines.vector_width x in
          L.pmux (List.map (fun (b,v) ->  bit sel b, c v) l) (c d) 
        in 
        R.reg
          ~rv:reset_pc
          ~cv:reset_pc
          ~e:(exception_re |: (rfete &: execute_done))
          (mux [
            9, Defines.berr_vector;
            8, Defines.illegal_vector;
            7, Defines.align_vector;
            6, Defines.align_vector;
            5, Defines.syscall_vector;
            4, Defines.trap_vector;
            3, Defines.berr_vector;
            2, Defines.range_vector;
            1, Defines.int_vector;
          ] Defines.tt_vector)
      in

      let op_mtspr = i.ctrl_opc_insn ==:. Defines.Opcode.mtspr in
      let op_mfspr = i.ctrl_opc_insn ==:. Defines.Opcode.mfspr in
      let op_rfe = i.ctrl_opc_insn ==:. Defines.Opcode.rfe in

      let waiting_for_except_fetch = R.reg_fb ~e:vdd ~w:1
        (L.pmux [
          waiting_for_except_fetch &: i.next_fetch_done, gnd;
          fetch_take_exception_branch, vdd;
        ])
      in
      
      let fetch_advance = 
        (i.next_fetch_done |: i.except_ibus_err) &:
        (~: execute_waiting) &: (~: cpu_stall) &:
        ((~: stepping) |:
        (stepping &: pstep.[0:0] &: (~: (i.next_fetch_done)))) 
      in

      let padv_fetch_o = 
        fetch_advance &: ~: exception_pending &: ~: doing_rfe_r &: ~: cpu_stall 
      in

      let take_exception = R.reg ~e:vdd
        ((exception_pending |: exception_r |: doing_rfe_r) &:
        (fetch_advance |:
          (* Cause exception to always be 'taken' if stepping *)
          (stepping &: execute_done)) &:
        (* Would like this as only a single pulse *)
        (~: take_exception))
      in

      let padv_decode_r = R.reg ~e:vdd padv_fetch_o in

      let execute_go = R.reg ~e:vdd 
        (padv_fetch_o |: execute_waiting |: (stepping &: i.next_fetch_done)) 
      in

      let execute_done = execute_go &: (~: execute_waiting) in

      (* ALU or LSU stall execution, nothing else can *)
      let execute_valid = 
        (~: ((i.op_lsu_load |: i.op_lsu_store) &: 
        (~: (i.lsu_valid)) |: (~: (i.alu_valid)))) 
      in

      let execute_waiting = (~: execute_valid)  &: (~: waiting_for_fetch) in
      let execute_waiting_o = execute_waiting in


      let padv_execute_o = execute_done in

      let spr_addr = mux2 du_access i.du_addr i.ctrl_alu_result.[15:0] in
      let ctrl_mfspr_we_o = op_mfspr &: execute_go in
      
      (* Pipeline flush *)
      let pipeline_flush_o = (execute_done &: op_rfe) |:
                             (exception_re) |:
                             cpu_stall
      in

      (* Flag *)
      let flag = R.reg ~e:execute_done 
        (mux2 i.ctrl_flag_clear gnd @@
         mux2 i.ctrl_flag_set vdd flag)
      in

      let execute_waiting_r = R.reg ~e:vdd execute_waiting in

      let decode_execute_halt = R.reg_fb ~e:vdd ~w:1
        (L.pmux [
          du_restart_from_stall, gnd;
          decode_execute_halt &: deassert_decode_execute_halt, gnd;
          ((op_rfe |: exn) &: (~: decode_execute_halt) &: (~: exception_taken)), gnd;
        ])
      in

      let exception_r = R.reg_fb ~e:vdd ~w:1
        (L.pmux [
          (exception_taken |: du_restart_from_stall), gnd;
          (exn &: (~: exception_r)), vdd;
        ])
      in
      
      let exception_taken = R.reg_fb ~e:vdd ~w:1
        (fun exception_taken -> L.pmux [
          exception_taken, gnd;
          exception_r &: take_exception, vdd;
        ] exception_taken)
      in

      let last_branch_insn_pc = R.reg ~e:(fetch_advance &: ctrl_branch_occur) spr_ppc in
      let last_branch_target_pc = 
        R.reg ~e:(execute_done &: ctrl_branch_occur &: stepping) ctrl_branch_target_o
      in

      let waiting_for_fetch = R.reg_fb ~e:vdd ~w:1
        (L.pmux [
          i.next_fetch_done, gnd;
          ((~: execute_waiting) &: execute_waiting_r &: (~: (i.next_fetch_done))), vdd;
        ])
      in

      let branched_and_waiting_for_fetch = R.reg_fb ~e:vdd ~w:1
        (fun branched_and_waiting_for_fetch -> L.pmux [
          exception_re, gnd;
          (padv_fetch_o &: ctrl_branch_occur_o), vdd;
          branched_and_waiting_for_fetch, ~: (i.next_fetch_done);
        ] branched_and_waiting_for_fetch)
      in

      let doing_rfe = ((execute_done &: op_rfe) |: doing_rfe_r) &: (~: deassert_doing_rfe) in

      (* Basically, the fetch stage should always take the rfe immediately *)
      let deassert_doing_rfe =  doing_rfe_r in

      let doing_rfe_r = R.reg_fb ~e:vdd ~w:1
        (L.pmux [
          deassert_doing_rfe, gnd;
          execute_done, op_rfe;
        ])
      in

      let spr_sr_o = spr_sr in

      O.(map (fun (_,b) -> zero b) t)

    module Inst = M.Inst(I)(O)
    let ctrl_inst = Inst.inst "ctrl" ctrl
  
  end

end

