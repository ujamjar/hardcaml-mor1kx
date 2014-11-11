module Espresso : sig

  module I : interface
    clk rst
    rfd_adr rfa_adr rfb_adr
    rf_we rf_re
    result
  end

  module O : interface
    rfa rfb
  end

  val rf : HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t

end

