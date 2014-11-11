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

