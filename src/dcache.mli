(******************************************************************************
 This Source Code Form is subject to the terms of the
 Open Hardware Description License, v. 1.0. If a copy
 of the OHDL was not distributed with this file, You
 can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

 Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

 ******************************************************************************)

module I : interface
  clk
  rst
  dc_enable
  dc_access
  cpu_dat_i
  cpu_adr
  cpu_adr_match
  cpu_req
  cpu_we
  cpu_bsel
  refill_allowed
  wradr
  wrdat
  we
  snoop_adr
  snoop_valid
  spr_bus_addr
  spr_bus_we
  spr_bus_stb
  spr_bus_dat_i
end

module O : interface
  refill
  refill_req
  refill_done
  cpu_err
  cpu_ack
  cpu_dat_o
  snoop_hit
  spr_bus_dat_o
  spr_bus_ack
  redundant
end

val dcache : Option.options -> HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t

