(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module Make(M : Utils.Module_cfg_signal) : sig

  module I : interface
    clk
    rst
    irq
    spr_access
    spr_we
    spr_addr
    spr_dat_i
  end

  module O : interface
    spr_picmr
    spr_picsr
    spr_bus_ack
    spr_dat_o
  end

  val pic : M.Bits.t I.t -> M.Bits.t O.t
  val pic_inst : M.Bits.t I.t -> M.Bits.t O.t

end
