(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module O : interface
    vr vr2 upr cpucfgr dmmucfgr immucfgr
    dccfgr iccfgr dcfgr pccfgr fpcsr avr
end

val cfgrs : Option.options -> Option.features -> HardCaml.Signal.Comb.t O.t


