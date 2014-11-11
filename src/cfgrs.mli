(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module Make(M : Utils.Module_cfg) : sig

  module I : HardCaml.Interface.S

  module O : interface
      vr vr2 upr cpucfgr dmmucfgr immucfgr
      dccfgr iccfgr dcfgr pccfgr fpcsr avr
  end

  val cfgrs : M.Bits.t I.t -> M.Bits.t O.t
  val cfgrs_inst : M.Bits.t I.t -> M.Bits.t O.t

end
