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
  
  let tlb_reload_busy = wire 1 in
  let busy = wire 1 in
  let spr_bus_dat = wire operand_width in

  let tlb_reload_req = R.g_reg ~e:vdd 1 in
  let tlb_reload_addr = R.g_reg ~e:vdd o.Option.immu_set_width in
  let tlb_reload_pagefault = R.g_reg ~e:vdd 1 in
  let itlb_trans_reload_we = R.g_reg ~e:vdd 1 in
  let itlb_trans_reload_din = R.g_reg ~e:vdd operand_width in
  let itlb_match_reload_we = R.g_reg ~e:vdd 1 in
  let itlb_match_reload_din = R.g_reg ~e:vdd operand_width in
  let tlb_reload_huge = R.g_reg ~e:vdd 1 in

  let itlb_match_huge_dout, itlb_match_dout = Ram.O.( wire operand_width, wire operand_width ) in
  let itlb_trans_huge_dout, itlb_trans_dout = Ram.O.( wire operand_width, wire operand_width ) in

  let tlb_huge = reduce (&:) (bits itlb_match_huge_dout.[1:0]) in (* huge & valid *)

  let spr_bus_ack_c = i.spr_bus_stb &: (i.spr_bus_addr.[15:11] ==:. 2) in
  let spr_bus_ack = R.reg ~e:vdd spr_bus_ack_c in
  let spr_bus_ack_r = R.reg ~e:vdd spr_bus_ack in

  let spr_bus_dat_r = R.reg ~e:(spr_bus_ack &: (~: spr_bus_ack_r)) spr_bus_dat in

  let cache_inhibit = mux2 tlb_huge itlb_trans_huge_dout.[1:1] itlb_trans_dout.[1:1] in

  let sxe = mux2 tlb_huge itlb_trans_huge_dout.[6:6] itlb_trans_dout.[6:6] in
  let uxe = mux2 tlb_huge itlb_trans_huge_dout.[7:7] itlb_trans_dout.[7:7] in

  let pagefault = (mux2 i.supervisor_mode (~: sxe) (~: uxe)) &: 
    (~: tlb_reload_busy) &: (~: busy) in

  let itlb_match_spr_cs = i.spr_bus_stb &:
    (i.spr_bus_addr >=: Spr.itlbw0mr0_addr) &:
    (i.spr_bus_addr <: Spr.itlbw0tr0_addr) in
  let itlb_trans_spr_cs = i.spr_bus_stb &:
    (i.spr_bus_addr >=: Spr.itlbw0tr0_addr) &:
    (i.spr_bus_addr <: Spr.itlbw1mr0_addr) in

  let itlb_match_addr = 
    mux2 (itlb_match_spr_cs &: (~: spr_bus_ack))
      i.spr_bus_addr.[o.Option.immu_set_width-1:0] 
      i.virt_addr.[13+(o.Option.immu_set_width-1):13] in
  let itlb_trans_addr = 
    mux2 (itlb_trans_spr_cs &: (~: spr_bus_ack))
      i.spr_bus_addr.[o.Option.immu_set_width-1:0] 
      i.virt_addr.[13+(o.Option.immu_set_width-1):13] in

  let itlb_match_we = itlb_match_spr_cs &: i.spr_bus_we &: (~: spr_bus_ack) |:
    itlb_match_reload_we#q &: (~: (tlb_reload_huge#q)) in
  let itlb_trans_we = itlb_trans_spr_cs &: i.spr_bus_we &: (~: spr_bus_ack) |:
    itlb_trans_reload_we#q &: (~: (tlb_reload_huge#q)) in
  let itlb_match_din = 
    mux2 (itlb_match_spr_cs &: i.spr_bus_we &: (~: spr_bus_ack))
      i.spr_bus_dat_i itlb_match_reload_din#q in
  let itlb_trans_din = 
    mux2 (itlb_trans_spr_cs &: i.spr_bus_we &: (~: spr_bus_ack))
      i.spr_bus_dat_i itlb_trans_reload_din#q in

  let itlb_match_huge_addr = i.virt_addr.[24+(o.Option.immu_set_width-1):24] in
  let itlb_trans_huge_addr = i.virt_addr.[24+(o.Option.immu_set_width-1):24] in

  let itlb_match_huge_we = itlb_match_reload_we#q &: tlb_reload_huge#q in
  let itlb_trans_huge_we = itlb_trans_reload_we#q &: tlb_reload_huge#q in

  let immucr_spr_cs, immucr = 
    if f.Option.immu_hw_tlb_reload then
      let cs = i.spr_bus_stb &: (i.spr_bus_addr ==: Spr.immucr_addr) in
      cs, R.reg ~e:(cs &: i.spr_bus_we) i.spr_bus_dat_i
    else
      gnd, zero operand_width
  in
  
  let itlb_match_spr_cs_r = R.reg ~e:vdd itlb_match_spr_cs in
  let itlb_trans_spr_cs_r = R.reg ~e:vdd itlb_trans_spr_cs in
  let immucr_spr_cs_r = R.reg ~e:vdd immucr_spr_cs in

  let () = spr_bus_dat <== 
    mux2 itlb_match_spr_cs_r itlb_match_dout @@
    mux2 itlb_trans_spr_cs_r itlb_trans_dout @@
    mux2 immucr_spr_cs_r immucr (zero operand_width) 
  in

  let tlb_miss = (itlb_match_dout.[31:13] <>: i.virt_addr_match.[31:13]) |:
    (~: itlb_match_dout.[0:0]) in  

  let tlb_huge_miss = (itlb_match_huge_dout.[31:24] <>: i.virt_addr_match.[31:24]) |: 
    (~: itlb_match_huge_dout.[0:0]) in

  let tlb_miss = (tlb_miss &: (~: tlb_huge) |: tlb_huge_miss &: tlb_huge) &:
    (~: (tlb_reload_pagefault#q)) &: (~: busy) in

  let phys_addr = 
    mux2 tlb_huge 
      (itlb_trans_huge_dout.[31:24] @: i.virt_addr_match.[23:0]) 
      (itlb_trans_dout.[31:13] @: i.virt_addr_match.[12:0])
  in

  let tlb_reload_pagefault = 
    let open HardCaml.Signal.Guarded in
    if f.Option.immu_hw_tlb_reload then
      let state_is, sm, next = R.statemachine ~e:vdd 
        (Enum_tlb_sm.enum_from_to Bounded_tlb_sm.min_bound Bounded_tlb_sm.max_bound)
      in
      let do_reload = i.enable &: tlb_miss &: (immucr.[31:10] <>:. 0) in
      let () = tlb_reload_busy <== ((~: (state_is Tlb_idle)) |: do_reload) in
      let tlb_reload_pagefault_o = tlb_reload_pagefault#q &: (~: (i.tlb_reload_pagefault_clear)) in
      let () = compile [
        g_when i.tlb_reload_pagefault_clear [ tlb_reload_pagefault $==. 0 ];
        itlb_trans_reload_we $==. 0;
        itlb_trans_reload_din $==. 0;
        itlb_match_reload_we $==. 0;
        itlb_match_reload_din $==. 0;

        sm [
          Tlb_idle, [
            tlb_reload_huge $==. 0;
            tlb_reload_req $==. 0;
            g_when (do_reload) [
              tlb_reload_req $==. 1;
              tlb_reload_addr $== (immucr.[31:10] @: i.virt_addr_match.[31:24] @: zero 2);
              next Tlb_get_pte_pointer;
            ];
          ];
          Tlb_get_pte_pointer, [
            tlb_reload_huge $==. 0;
            g_when (i.tlb_reload_ack) [
              g_if (i.tlb_reload_data.[31:13] ==:. 0) [
                tlb_reload_pagefault $==. 1;
                tlb_reload_req $==. 0;
                next Tlb_idle;
              ] @@ g_elif (i.tlb_reload_data.[9:9]) [
                tlb_reload_huge $==. 1;
                tlb_reload_req $==. 0;
                next Tlb_get_pte;
              ] [
                tlb_reload_addr $== (i.tlb_reload_data.[31:13] @: 
                                       i.virt_addr_match.[23:13] @: zero 2);
                next Tlb_get_pte;
              ]
            ]
          ];
          Tlb_get_pte, [
            g_when (i.tlb_reload_ack) [
              tlb_reload_req $==. 0;
              g_if (~: (i.tlb_reload_data.[10:10])) [
                tlb_reload_pagefault $==. 1;
                next Tlb_idle;
              ] [
                itlb_trans_reload_din $==
                  List.fold_left (fun t (f,n) -> Utils.insert ~t ~f n)
                  itlb_trans_reload_din#q
                  [
                    i.tlb_reload_data.[31:13], 13;
                    concat [
                      i.tlb_reload_data.[8:8] &: i.tlb_reload_data.[6:6];
                      i.tlb_reload_data.[8:8];
                      i.tlb_reload_data.[5:0];
                    ], 0;
                  ];
                itlb_trans_reload_we $==. 1;

                itlb_match_reload_din $==
                  List.fold_left (fun t (f,n) -> Utils.insert ~t ~f n)
                  itlb_match_reload_din#q
                  [
                    i.virt_addr_match.[31:13], 13;
                    tlb_reload_huge#q @: vdd, 0;
                  ];
                itlb_match_reload_we $==. 1;

                next Tlb_read;
              ]
          ]
          ];
          Tlb_read, [
            next Tlb_idle;
          ];
        ];
      ] in
      tlb_reload_pagefault_o
    else
      let () = tlb_reload_busy <== gnd in
      let () = compile [
        tlb_reload_req $==. 0;
        tlb_reload_addr $==. 0;
        tlb_reload_huge $==. 0;
        tlb_reload_pagefault $==. 0;
        itlb_trans_reload_we $==. 0;
        itlb_trans_reload_din $==. 0;
        itlb_match_reload_we $==. 0;
        itlb_match_reload_din $==. 0;
      ] in
      gnd
  in

  (* rams *)

  let itm = Ram.(ram 
    I.({
      clk = i.clk;
      addr_a = itlb_match_addr;
      we_a = itlb_match_we;
      din_a = itlb_match_din;
      addr_b = itlb_match_huge_addr;
      we_b  = itlb_match_huge_we;
      din_b = itlb_match_reload_din#q;
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
      din_b = itlb_trans_reload_din#q;
    }))
  in
  
  let () = Ram.O.(
    itlb_match_dout <== itm.dout_a;
    itlb_match_huge_dout <== itm.dout_b;
    itlb_trans_dout <== itt.dout_a;
    itlb_trans_huge_dout <== itt.dout_b;
  ) in

  let spr_bus_ack = spr_bus_ack &: spr_bus_ack_c in
  let spr_bus_dat_o = mux2 (spr_bus_ack &: (~: spr_bus_ack_r)) spr_bus_dat spr_bus_dat_r in
  let () = 
    busy <== (((itlb_match_spr_cs |: itlb_trans_spr_cs) &: (~: spr_bus_ack) |:
	      	     (itlb_match_spr_cs_r |: itlb_trans_spr_cs_r) &:
               spr_bus_ack &: (~: spr_bus_ack_r)) &: i.enable) in


  O.{
    busy;
    phys_addr;
    cache_inhibit;
    tlb_miss;
    pagefault;
    tlb_reload_req = tlb_reload_req#q;
    tlb_reload_busy;
    tlb_reload_addr = tlb_reload_addr#q;
    tlb_reload_pagefault;
    spr_bus_dat_o;
    spr_bus_ack;
    redundant = List.fold_left (&:) gnd @@ List.map lsb @@ I.to_list i;
  }

