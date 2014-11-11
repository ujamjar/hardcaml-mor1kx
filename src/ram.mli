(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module type S = sig
  val addr_width : int
  val data_width : int
end

module Simple_dp(S : S)(M : Utils.Module_cfg_signal) : sig

  module I : interface
    clk
    raddr re
    waddr we
    din
  end

  module O : interface
    dout
  end

  val ram : enable_bypass:bool -> M.Bits.t I.t -> M.Bits.t O.t
  val ram_inst : enable_bypass:bool -> M.Bits.t I.t -> M.Bits.t O.t

end

module True_dp(S : S)(M : Utils.Module_cfg_signal) : sig
  
  module I : interface
    clk
    addr_a we_a din_a
    addr_b we_b din_b
  end

  module O : interface
    dout_a dout_b
  end

  val ram : M.Bits.t I.t -> M.Bits.t O.t
  val ram_inst : M.Bits.t I.t -> M.Bits.t O.t

end


