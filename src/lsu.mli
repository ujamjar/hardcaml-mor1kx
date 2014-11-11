(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module Espresso : sig

  module Make(M : Utils.Module_cfg_signal) : sig

    module I : interface
      clk 
      rst 
      padv_fetch 
      lsu_adr 
      rfb 
      op_lsu_load
      op_lsu_store 
      lsu_length 
      lsu_zext 
      exception_taken
      du_restart 
      stepping 
      next_fetch_done 
      dbus_err
      dbus_ack 
      dbus_dat_i
    end

    module O : interface
      lsu_result 
      lsu_valid 
      lsu_except_dbus 
      lsu_except_align
      dbus_adr 
      dbus_req 
      dbus_dat_o 
      dbus_bsel 
      dbus_we
      dbus_burst
    end
    
    val lsu : registered_io:bool -> M.Bits.t I.t -> M.Bits.t O.t
    val lsu_inst : registered_io:bool -> M.Bits.t I.t -> M.Bits.t O.t
  
  end

end

module Cappuccino : sig

  module Make(M : Utils.Module_cfg_signal) : sig

    module I : interface
      clk
      rst
      padv_execute
      padv_ctrl 
      decode_valid
      exec_lsu_adr
      ctrl_lsu_adr
      ctrl_rfb
      exec_op_lsu_load
      exec_op_lsu_store
      exec_op_lsu_atomic
      ctrl_op_lsu_load
      ctrl_op_lsu_store
      ctrl_op_lsu_atomic
      ctrl_lsu_length
      ctrl_lsu_zext
      ctrl_epcr
      spr_bus_addr
      spr_bus_we
      spr_bus_stb
      spr_bus_dat
      dc_enable
      dmmu_enable
      supervisor_mode
      dbus_err
      dbus_ack
      dbus_dat_i
      pipeline_flush
      snoop_adr
      snoop_en
    end

    module O : interface
      store_buffer_epcr
      lsu_result
      lsu_valid
      lsu_except_dbus
      lsu_except_align
      lsu_except_dtlb_miss
      lsu_except_dpagefault
      store_buffer_err
      atomic_flag_set
      atomic_flag_clear
      spr_bus_dat_dc
      spr_bus_ack_dc
      spr_bus_dat_dmmu
      spr_bus_ack_dmmu
      dbus_adr
      dbus_req
      dbus_dat_o
      dbus_bsel
      dbus_we
      dbus_burst
    end

  end

end
