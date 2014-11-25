module Espresso = struct

  (* ****************************************************************************
    This Source Code Form is subject to the terms of the
    Open Hardware Description License, v. 1.0. If a copy
    of the OHDL was not distributed with this file, You
    can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

    Description: mor1kx espresso debug unit

    Copyright (C) 2012 Authors

    Author(s): Julius Baxter <juliusbaxter@gmail.com>
              Andy Ray <andy.ray@ujamjar.com>

  ***************************************************************************** *)
  module Make(M : Utils.Module_cfg_signal) = struct

    open M.Bits
    open Option
    open Utils
    module Spr = Spr.Make(M.Bits)

    module I = interface
      clk[1]
      rst[1]
      except_trap[1] 
      next_fetch_done[1]
      execute_done[1]
      execute_delay_slot[1]
      last_branch_target_pc[M.o.operand_width]
      ctrl_branch_occur[1]
      du_stb_i[1]
      du_stall_i[1]
      du_we[1]
      du_addr[16]
      du_dat_i[M.o.operand_width]
      op_rfe[1]
      spr_epcr[M.o.operand_width]
      spr_group_present[1]
      spr_access_ack_i[1]
      spr_internal_read_dat_i[M.o.operand_width]
      spr_npc[M.o.operand_width]
      spr_addr[16]
      spr_we
      spr_write_dat[M.o.operand_width]
    end

    module O = interface
      spr_access_ack_o[1]
      spr_internal_read_dat_o[M.o.operand_width]
      cpu_stall[1]
      du_stall_o[1]
      du_ack[1]
      du_restart_pc[M.o.operand_width]
      du_restart[1]
      du_restart_from_stall[1]
      du_access[1]
      du_dat_o[M.o.operand_width]
      stepping[1]
      pstep[1]
      stall_on_trap[1]
    end

    let debugunit i = 
      let open I in
      let open HardCaml.Signal.Guarded in
      let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in

      (* Generate ack back to the debug interface bus *)
      let du_ack = R.reg_fb ~e:vdd ~w:1 
        (fun du_ack -> pmux [
          du_ack, gnd;
          i.du_stb_i,
            pmux [
              (* Unit doesn't exist, ACK to clear the access, nothing done *)
              ~: (i.spr_group_present), vdd;
              (* actual access occurred *)
              i.spr_access_ack_i, vdd; 
            ] du_ack;
        ] du_ack)
      in

      (* Put the incoming stall signal through a register to detect FE *)
      let du_stall_r = R.reg ~e:vdd i.du_stall_i in

      let du_read_dat = R.reg ~e:i.spr_access_ack_i i.spr_internal_read_dat_i in 
      
      (* Pulse to indicate we're restarting after a stall *)
      let du_restart_from_stall = du_stall_r &: (~: (i.du_stall_i)) in

      (* NPC debug control logic *)
      let du_npc_write = (i.du_we &&: (i.du_addr ==:. Spr.Sys.npc) &&:
                             du_ack) in

      (* record if NPC was written while we were stalled.
       If so, we will use this value for restarting *)
      let du_npc_written = R.reg_fb ~e:vdd ~w:1
        (pmux [
          du_restart_from_stall, gnd;
          du_npc_write, vdd;
        ])
      in

      let du_spr_npc = R.reg ~e:du_npc_write i.du_dat_i in

      (*let stepped_into_exception = R.reg_fb ~e:vdd ~w:1
        (pmux [
          du_restart_from_stall, gnd;
          (stepping &: i.execute_done), exn;
        ])
      in*)

      (* DMR1 *)
      let spr_dmr1 = R.reg ~e:(i.spr_we &&: (i.spr_addr ==:. Spr.Du.dmr1))
          (uresize i.spr_write_dat.[23:0] 32) in

      (* DMR2 *)
      let spr_dmr2 = zero 32 in

      (* DSR *)
      let spr_dsr = R.reg ~e:(i.spr_we &&: (i.spr_addr ==:. Spr.Du.dsr))
          (uresize i.spr_write_dat.[13:0] 32) in

      (* Indicate when we're stepping *)
      let stepping = spr_dmr1.[Spr.dmr1_st:Spr.dmr1_st] &:
                     spr_dsr.[Spr.dsr_te:Spr.dsr_te] in

      (* Pick the traps-cause-stall bit out of the DSR *)
      let stall_on_trap = spr_dsr.[Spr.dsr_te:Spr.dsr_te] in

      let pstep = R.reg_fb ~e:vdd ~w:1
        (fun pstep -> pmux [
          (du_restart_from_stall &: stepping), one 2;
          ((pstep.[0:0] &: i.next_fetch_done) |:
               (* decode is always single cycle *)
               (pstep.[1:1] &: i.execute_done)),
            (pstep.[0:0] @: gnd) 
        ] pstep)
      in

      let branch_step = R.reg_fb ~e:vdd ~w:1 
        (fun branch_step -> pmux [
          (stepping &: pstep.[1:1]), (branch_step.[0:0] @: i.ctrl_branch_occur);
          ((~: stepping) &: i.execute_done), (branch_step.[0:0] @: i.execute_delay_slot);
        ] branch_step)
      in

      let stepped_into_delay_slot = branch_step.[1:1] in

      (* DRR *)
      let spr_drr = R.reg_fb ~e:vdd ~w:32
        (fun d -> pmux [
          (i.spr_we &&: (i.spr_addr ==:. Spr.Du.drr)), uresize i.spr_write_dat.[13:0] 32;
          (stall_on_trap &: i.execute_done &: i.except_trap), insert ~t:d ~f:vdd Spr.drr_te;
        ] d)
      in

      (* TODO: check into only letting stall go high when we've gracefully
       completed the instruction currently in the ctrl stage.
       Why? Potentially an instruction like l.mfspr from an external unit
       hasn't completed fully, gets interrupted, and it's assumed it's
       completed, but actually hasn't. *)
      let cpu_stall = i.du_stall_i |: du_restart_from_stall in

      let stepped_into_rfe = R.reg_fb ~e:vdd ~w:1
        (pmux [
          du_restart_from_stall, gnd;
          (stepping &: i.execute_done), i.op_rfe;
        ])
      in

      (* goes out to the debug interface and comes back 1 cycle later
       via du_stall_i *)
      let du_stall_o = (stepping &: i.execute_done) |:
        (stall_on_trap &: i.execute_done &: i.except_trap) in

      let du_restart_pc = 
        pmux [
          du_npc_written, du_spr_npc;
          stepped_into_rfe, i.spr_epcr;
          stepped_into_delay_slot, i.last_branch_target_pc; 
        ] i.spr_npc 
      in

      let du_restart = du_restart_from_stall in

      let du_dat_o = du_read_dat in
      
      (* always single cycle access *)
      let spr_access_ack_o = vdd in 
      let spr_internal_read_dat_o = 
        pmux [
          (i.spr_addr ==:. Spr.Du.dmr1), spr_dmr1;
          (i.spr_addr ==:. Spr.Du.dmr2), spr_dmr2;
          (i.spr_addr ==:. Spr.Du.dsr), spr_dsr;
          (i.spr_addr ==:. Spr.Du.drr), spr_drr;
        ] (zero 32)
      in

      let du_access = i.du_stb_i in

      O.{
        spr_access_ack_o; 
        spr_internal_read_dat_o;
        cpu_stall;
        du_stall_o;
        du_ack;
        du_restart_pc;
        du_restart;
        du_restart_from_stall;
        du_access;
        du_dat_o;
        stepping;
        pstep;
        stall_on_trap;
      }

    module Inst = M.Inst(I)(O)
    let debugunit_inst = Inst.inst "debugunit" debugunit

  end

end
