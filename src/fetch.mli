(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Authors

  Author(s): Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module Tcm_pronto_espresso : sig
  module Make(M : Utils.Module_cfg_signal) : sig
    module I : interface
      clk
      rst 
      ibus_err 
      ibus_ack 
      ibus_dat 
      padv
      branch_occur 
      branch_dest 
      du_restart 
      du_restart_pc
      fetch_take_exception_branch 
      execute_waiting 
      du_stall
      stepping 
      flag 
      flag_clear 
      flag_set
    end
    module O : interface
      ibus_adr 
      ibus_req 
      decode_insn 
      fetched_pc 
      fetch_ready
      fetch_rfa_adr 
      fetch_rfb_adr 
      fetch_rf_re 
      pc_fetch_next
      decode_except_ibus_err 
      fetch_sleep
    end
    val fetch : M.Bits.t I.t -> M.Bits.t O.t
    val fetch_inst : M.Bits.t I.t -> M.Bits.t O.t
  end
end

module Pronto_espresso : sig
  module Make(M : Utils.Module_cfg_signal) : sig
    module I : interface 
      clk
      rst
      ibus_err
      ibus_ack
      ibus_dat
      ic_enable
      padv
      branch_occur
      branch_dest
      ctrl_insn_done
      du_restart
      du_restart_pc
      fetch_take_exception_branch
      execute_waiting
      du_stall
      stepping
      flag
      flag_clear
      flag_set
      spr_bus_addr
      spr_bus_we
      spr_bus_stb
      spr_bus_dat
    end
    module O : interface 
      ibus_adr
      ibus_req
      ibus_burst
      decode_insn
      fetched_pc
      fetch_ready
      fetch_rfa_adr
      fetch_rfb_adr
      fetch_rf_re
      pc_fetch_next
      decode_except_ibus_err
      fetch_sleep
      fetch_quick_branch
      spr_bus_dat_ic
      spr_bus_ack_ic
    end
    val fetch : M.Bits.t I.t -> M.Bits.t O.t
    val fetch_inst : M.Bits.t I.t -> M.Bits.t O.t
  end
end

module Espresso : sig
  module Make(M : Utils.Module_cfg_signal) : sig
    module I : interface end
    module O : interface end
    val fetch : M.Bits.t I.t -> M.Bits.t O.t
    val fetch_inst : M.Bits.t I.t -> M.Bits.t O.t
  end
end

module Cappuccino: sig
  module Make(M : Utils.Module_cfg_signal) : sig
    module I : interface end
    module O : interface end
    val fetch : M.Bits.t I.t -> M.Bits.t O.t
    val fetch_inst : M.Bits.t I.t -> M.Bits.t O.t
  end
end
