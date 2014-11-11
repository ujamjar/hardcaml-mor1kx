module Espresso : sig

  module Make(M : Utils.Module_cfg_signal) : sig

    module I : interface
      clk rst
      rfd_adr rfa_adr rfb_adr
      rf_we rf_re
      result
    end

    module O : interface
      rfa rfb
    end

    val rf : M.Bits.t I.t -> M.Bits.t O.t
    val rf_inst : M.Bits.t I.t -> M.Bits.t O.t

  end

end

module Cappuccino : sig

  module Make(M : Utils.Module_cfg_signal) : sig

    module I : interface
      clk
      rst
      padv_decode
      padv_execute
      padv_ctrl
      decode_valid
      fetch_rf_adr_valid
      fetch_rfa_adr
      fetch_rfb_adr
      decode_rfa_adr
      decode_rfb_adr
      execute_rfd_adr
      ctrl_rfd_adr
      wb_rfd_adr
      spr_bus_addr
      spr_bus_stb
      spr_bus_we
      spr_bus_dat
      execute_rf_wb
      ctrl_rf_wb
      wb_rf_wb
      result
      ctrl_alu_result
      pipeline_flush
    end

    module O : interface
      spr_gpr_ack
      spr_gpr_dat
      decode_rfa
      decode_rfb
      execute_rfa
      execute_rfb
    end

    val rf : M.Bits.t I.t -> M.Bits.t O.t
    val rf_inst : M.Bits.t I.t -> M.Bits.t O.t

  end

end

