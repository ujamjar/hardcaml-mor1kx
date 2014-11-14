(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Authors

  Author(s): Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module Espresso : sig

  module Make(M : Utils.Module_cfg_signal) : sig

    module I : interface
      clk 
      rst
      ctrl_alu_result
      ctrl_rfb
      ctrl_flag_set 
      ctrl_flag_clear
      ctrl_opc_insn
      pc_fetch
      fetch_advancing
      except_ibus_err
      except_illegal
      except_syscall 
      except_dbus
      except_trap 
      except_align
      next_fetch_done
      alu_valid 
      lsu_valid
      op_lsu_load 
      op_lsu_store
      op_jr 
      op_jbr
      irq
      carry_set
      carry_clear
      overflow_set
      overflow_clear
      du_addr
      du_stb
      du_dat
      du_we
      du_stall
      spr_bus_dat_dc
      spr_bus_ack_dc
      spr_bus_dat_ic
      spr_bus_ack_ic
      spr_bus_dat_dmmu
      spr_bus_ack_dmmu
      spr_bus_dat_immu
      spr_bus_ack_immu
      spr_bus_dat_mac
      spr_bus_ack_mac
      spr_bus_dat_pmu
      spr_bus_ack_pmu
      spr_bus_dat_pcu
      spr_bus_ack_pcu
      spr_bus_dat_fpu
      spr_bus_ack_fpu
      multicore_coreid
      rf_wb
    end

    module O : interface
      flag
      spr_npc
      spr_ppc
      mfspr_dat
      ctrl_mfspr_we
      carry
      pipeline_flush
      padv_fetch
      padv_decode
      padv_execute
      fetch_take_exception_branch
      exception_taken
      execute_waiting
      stepping
      du_dat
      du_ack
      du_stall
      du_restart_pc
      du_restart
      spr_bus_addr
      spr_bus_we
      spr_bus_stb
      spr_bus_dat
      spr_sr
      ctrl_branch_occur
      rf_we
    end

    val ctrl : M.Bits.t I.t -> M.Bits.t O.t
    val ctrl_inst : M.Bits.t I.t -> M.Bits.t O.t

  end

end

