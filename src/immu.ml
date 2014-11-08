(******************************************************************************
 This Source Code Form is subject to the terms of the
 Open Hardware Description License, v. 1.0. If a copy
 of the OHDL was not distributed with this file, You
 can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

 Description: Instruction MMU implementation

 Copyright (C) 2013 Stefan Kristiansson <stefan.kristiansson@saunalahti.fi>
 Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

 ******************************************************************************)

open HardCaml.Signal.Comb
open Utils

let operand_width = 32

module I = interface
  clk[1]
  rst[1]
  enable[1]
  virt_addr[operand_width]
  virt_addr_match[operand_width]
  op_store[1]
  op_load[1]
  supervisor_mode[1]
  tlb_reload_ack[1]
  tlb_reload_data[operand_width]
  tlb_reload_pagefault_clear[1]
  spr_bus_addr[16]
  spr_bus_we[1]
  spr_bus_stb[1]
  spr_bus_dat_i[operand_width]
end

module O = interface
  busy[1]
  phys_addr[operand_width]
  cache_inhibit[1]
  tlb_miss[1]
  pagefault[1]
  tlb_reload_req[1]
  tlb_reload_busy[1]
  tlb_reload_addr[operand_width]
  tlb_reload_pagefault[1]
  spr_bus_dat_o[operand_width]
  spr_bus_ack[1]
  redundant[1]
end

type tlb_sm = Tlb_idle | Tlb_get_pte_pointer | Tlb_get_pte | Tlb_read
  deriving(Enum, Bounded)

let immu o f i = 
  let open I in
  let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in
 
  let module Ram = Ram.True_dp(struct 
    let addr_width = o.Option.dmmu_set_width
    let data_width = operand_width
  end) in

  let itlb_match_huge_dout, itlb_match_dout = Ram.O.( wire operand_width, wire operand_width ) in
  let itlb_trans_huge_dout, itlb_trans_dout = Ram.O.( wire operand_width, wire operand_width ) in

  let spr_bus_ack = R.reg ~e:vdd (i.spr_bus_stb &: (i.spr_bus_addr.[15:11] ==:. 2)) in
  let spr_bus_ack_r = R.reg ~e:vdd spr_bus_ack in



  (* XXX END *)

  (* rams *)

  (* XXX DELETE ME *)
  let itlb_match_addr, itlb_match_we, itlb_match_din = gnd,gnd,gnd in
  let itlb_match_huge_addr, itlb_match_huge_we, itlb_match_reload_din = gnd,gnd,gnd in
  let itlb_trans_addr, itlb_trans_we, itlb_trans_din = gnd,gnd,gnd in
  let itlb_trans_huge_addr, itlb_trans_huge_we, itlb_trans_reload_din = gnd,gnd,gnd in

  let itm = Ram.(ram 
    I.({
      clk = i.clk;
      addr_a = itlb_match_addr;
      we_a = itlb_match_we;
      din_a = itlb_match_din;
      addr_b = itlb_match_huge_addr;
      we_b  = itlb_match_huge_we;
      din_b = itlb_match_reload_din;
    }))
  in

  let itt = Ram.(ram 
    I.({
      clk = i.clk;
      addr_a = itlb_trans_addr;
      we_a = itlb_trans_we;
      din_a = itlb_trans_din;
      addr_b = itlb_trans_huge_addr;
      we_b  = itlb_trans_huge_we;
      din_b = itlb_trans_reload_din;
    }))
  in
  
  let () = Ram.O.(
    itlb_match_dout <== itm.dout_a;
    itlb_match_huge_dout <== itm.dout_b;
    itlb_trans_dout <== itt.dout_a;
    itlb_trans_huge_dout <== itt.dout_b;
  ) in

  let spr_bus_ack = spr_bus_ack &: i.spr_bus_stb &: (i.spr_bus_addr.[15:11] ==:. 2) in

  O.(map (fun (_,b) -> zero b) t)




