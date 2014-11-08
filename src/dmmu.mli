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
  enable
  virt_addr
  virt_addr_match
  op_store
  op_load
  supervisor_mode
  tlb_reload_ack
  tlb_reload_data
  tlb_reload_pagefault_clear
  spr_bus_addr
  spr_bus_we
  spr_bus_stb
  spr_bus_dat_i
end

module O : interface
  phys_addr
  cache_inhibit
  tlb_miss
  pagefault
  tlb_reload_req
  tlb_reload_busy
  tlb_reload_addr
  tlb_reload_pagefault
  spr_bus_dat_o
  spr_bus_ack
  redundant
end

val dmmu : Option.options -> Option.features ->
  HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t 

