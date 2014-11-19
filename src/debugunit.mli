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
      except_trap
      next_fetch_done
      execute_done
      execute_delay_slot
      last_branch_target_pc
      ctrl_branch_occur
      du_stb_i
      du_stall_i
      du_we
      du_addr
      du_dat_i
      op_rfe
      spr_epcr
      spr_group_present
      spr_access_ack_i
      spr_internal_read_dat_i
      spr_npc
      spr_addr
      spr_we
      spr_write_dat
    end

    module O : interface
      spr_access_ack_o
      spr_internal_read_dat_o
      cpu_stall
      du_stall_o
      du_ack
      du_restart_pc
      du_restart
      du_restart_from_stall
      du_access
      du_dat_o
      stepping
      pstep
      stall_on_trap
    end

    val debugunit : M.Bits.t I.t -> M.Bits.t O.t
    val debugunit_inst : M.Bits.t I.t -> M.Bits.t O.t
  end

end
