(******************************************************************************
 This Source Code Form is subject to the terms of the
 Open Hardware Description License, v. 1.0. If a copy
 of the OHDL was not distributed with this file, You
 can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

 Description: Data MMU implementation

 Copyright (C) 2013 Stefan Kristiansson <stefan.kristiansson@saunalahti.fi>
 Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

 ******************************************************************************)

open Utils

module Make(M : Utils.Module_cfg_signal) = struct

  open M
  open Bits
  module L = Utils.Logic(M.Bits)
  open L

  let operand_width = M.o.Option.operand_width

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

  let dmmu i = 
    let open I in
    let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in
  
    (* XXX *)
    let (||:) = (|:) in

    let module Ram = Ram.True_dp(struct 
      let addr_width = o.Option.dmmu_set_width
      let data_width = operand_width
    end)(M) in

    let dtlb_match_huge_dout, dtlb_match_dout = Ram.O.( wire operand_width, wire operand_width ) in
    let dtlb_trans_huge_dout, dtlb_trans_dout = Ram.O.( wire operand_width, wire operand_width ) in

    let spr_bus_ack =
      let cond = i.spr_bus_stb &: (i.spr_bus_addr.[15:11] ==:. 1) in
      (R.reg ~e:vdd cond) &: cond 
    in

    let tlb_huge = reduce (&:) (bits dtlb_match_huge_dout.[1:0]) in (* huge & valid *)

    let tlb_reload_busy = wire 1 in
    let tlb_reload_huge = R.g_reg ~e:vdd 1 in
    let tlb_reload_pagefault = R.g_reg ~e:vdd 1 in
    let dtlb_match_reload_we = R.g_reg ~e:vdd 1 in
    let dtlb_trans_reload_we = R.g_reg ~e:vdd 1 in
    let dtlb_match_reload_din = R.g_reg ~e:vdd operand_width in
    let dtlb_trans_reload_din = R.g_reg ~e:vdd operand_width in

    let cache_inhibit = mux2 tlb_huge dtlb_trans_huge_dout.[1:1] dtlb_trans_dout.[1:1] in
    let ure = mux2 tlb_huge dtlb_trans_huge_dout.[6:6] dtlb_trans_dout.[6:6] in
    let uwe = mux2 tlb_huge dtlb_trans_huge_dout.[7:7] dtlb_trans_dout.[7:7] in
    let sre = mux2 tlb_huge dtlb_trans_huge_dout.[8:8] dtlb_trans_dout.[8:8] in
    let swe = mux2 tlb_huge dtlb_trans_huge_dout.[9:9] dtlb_trans_dout.[9:9] in

    let pagefault = 
      (mux2 i.supervisor_mode 
        ((~: swe) &: i.op_store ||: (~: sre) &: i.op_load) 
        ((~: uwe) &: i.op_store ||: (~: ure) &: i.op_load)) &:
      (~: tlb_reload_busy) 
    in

    let dtlb_match_spr_cs = i.spr_bus_stb &:
            (i.spr_bus_addr >=: M.Spr.dtlbw0mr0_addr) &:
            (i.spr_bus_addr <: M.Spr.dtlbw0tr0_addr) in
    let dtlb_trans_spr_cs = i.spr_bus_stb &:
            (i.spr_bus_addr >=: M.Spr.dtlbw0tr0_addr) &:
            (i.spr_bus_addr <: M.Spr.dtlbw1mr0_addr) in

    let dtlb_match_spr_cs_r = R.reg ~e:vdd dtlb_match_spr_cs in
    let dtlb_trans_spr_cs_r = R.reg ~e:vdd dtlb_trans_spr_cs in

    let dmmucr_spr_cs, dmmucr_spr_cs_r, dmmucr = 
      if f.Option.dmmu_hw_tlb_reload then 
        let dmmucr_spr_cs = i.spr_bus_stb &: (i.spr_bus_addr ==: M.Spr.dmmucr_addr) in
        dmmucr_spr_cs, R.reg ~e:vdd dmmucr_spr_cs, 
        R.reg ~e:(dmmucr_spr_cs &: i.spr_bus_we) i.spr_bus_dat_i
      else
        gnd, gnd, zero operand_width
    in

    let dtlb_match_addr = 
      mux2 dtlb_match_spr_cs 
        i.spr_bus_addr.[o.Option.dmmu_set_width-1:0] 
        i.virt_addr.[13+(o.Option.dmmu_set_width-1):13] in
    let dtlb_trans_addr = 
      mux2 dtlb_trans_spr_cs 
        i.spr_bus_addr.[o.Option.dmmu_set_width-1:0] 
        i.virt_addr.[13+(o.Option.dmmu_set_width-1):13] in

    let dtlb_match_we = dtlb_match_spr_cs &: i.spr_bus_we |: dtlb_match_reload_we#q in
    let dtlb_trans_we = dtlb_trans_spr_cs &: i.spr_bus_we |: dtlb_trans_reload_we#q in

    let dtlb_match_din = mux2 dtlb_match_reload_we#q dtlb_match_reload_din#q i.spr_bus_dat_i in
    let dtlb_trans_din = mux2 dtlb_trans_reload_we#q dtlb_trans_reload_din#q i.spr_bus_dat_i in

    let dtlb_match_huge_addr = i.virt_addr.[24+(o.Option.dmmu_set_width-1):24] in
    let dtlb_trans_huge_addr = i.virt_addr.[24+(o.Option.dmmu_set_width-1):24] in

    let dtlb_match_huge_we = dtlb_match_reload_we#q &: tlb_reload_huge#q in
    let dtlb_trans_huge_we = dtlb_trans_reload_we#q &: tlb_reload_huge#q in

    let spr_bus_dat_o = 
      mux2 dtlb_match_spr_cs_r dtlb_match_dout @@
      mux2 dtlb_trans_spr_cs_r dtlb_trans_dout @@
      mux2 dmmucr_spr_cs_r dmmucr (zero operand_width) in

    let tlb_miss = (dtlb_match_dout.[31:13] <>: i.virt_addr_match.[31:13]) |:
      (~: dtlb_match_dout.[0:0]) in (* valid bit *)

    let tlb_huge_miss = (dtlb_match_huge_dout.[31:24] <>: i.virt_addr_match.[31:24]) |:
      (~: dtlb_match_huge_dout.[0:0]) in

    let tlb_miss = (tlb_miss &: ~: tlb_huge |: tlb_huge_miss &: tlb_huge) &:
            (~: (tlb_reload_pagefault#q)) in

    let phys_addr = 
      mux2 tlb_huge 
        (dtlb_trans_huge_dout.[31:24] @: i.virt_addr_match.[23:0]) 
        (dtlb_trans_dout.[31:13] @: i.virt_addr_match.[12:0]) in
    
    let tlb_reload_req = R.g_reg ~e:vdd 1 in
    let tlb_reload_addr = R.g_reg ~e:vdd operand_width in
    let tlb_reload_pagefault = 
      let open HardCaml.Signal.Guarded in
      if f.Option.dmmu_hw_tlb_reload then
        let state_is, sm, next = R.statemachine ~e:vdd 
          (Enum_tlb_sm.enum_from_to Bounded_tlb_sm.min_bound Bounded_tlb_sm.max_bound)
        in
        let do_reload = i.enable |: tlb_miss &: (dmmucr.[31:10] <>:. 0) &: 
          (i.op_load |: i.op_store) in

        let () = tlb_reload_busy <== (i.enable &: state_is Tlb_idle |: do_reload) in
        let tlb_reload_pagefault_o = tlb_reload_pagefault#q &: (~: (i.tlb_reload_pagefault_clear)) in

        let () = compile [
          g_when i.tlb_reload_pagefault_clear [ tlb_reload_pagefault $==. 0 ];
          dtlb_trans_reload_we $==. 0;
          dtlb_trans_reload_din $==. 0;
          dtlb_match_reload_we $==. 0;
          dtlb_match_reload_din $==. 0;

          sm [
            Tlb_idle, [
              tlb_reload_huge $==. 0;
              tlb_reload_req $==. 0;
              g_when (do_reload) [
                  tlb_reload_req $==. 1;
                  tlb_reload_addr $== (dmmucr.[31:10] @: 
                                      i.virt_addr_match.[31:24] @: 
                                      consti 2 0b00);
                  next Tlb_get_pte_pointer;
              ]
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
                                      i.virt_addr_match.[23:13] @: 
                                      consti 2 0b00);
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

                  dtlb_trans_reload_din $==
                    List.fold_left (fun t (f,n) -> insert ~t ~f n)
                    dtlb_trans_reload_din#q
                    [
                      i.tlb_reload_data.[31:13], 13;
                      concat [
                        i.tlb_reload_data.[7:7];
                        vdd;
                        i.tlb_reload_data.[7:7] &: i.tlb_reload_data.[6:6];
                        i.tlb_reload_data.[6:6];
                        i.tlb_reload_data.[5:0];
                      ], 0;
                    ];
                  dtlb_trans_reload_we $==. 1;

                  dtlb_match_reload_din $==
                    List.fold_left (fun t (f,n) -> insert ~t ~f n)
                    dtlb_match_reload_din#q
                    [
                      i.virt_addr_match.[31:13], 13;
                      vdd, 0;
                    ];
                  dtlb_match_reload_we $==. 1;

                  next Tlb_read;
                ]
              ]
            ];
            Tlb_read, [
              next Tlb_idle;
            ];
          ];

          g_when ((~: (i.enable)) |: (dmmucr.[31:10] ==:. 0)) [ next Tlb_idle ];

        ] in
        tlb_reload_pagefault_o 
      else
        let () = tlb_reload_busy <== gnd in
        let tlb_reload_pagefault_o = gnd in
        let () = compile [ (* XXX these do not need to be registers! *)
          tlb_reload_req $==. 0;
          tlb_reload_addr $==. 0;
          tlb_reload_huge $==. 0;
          tlb_reload_pagefault $==. 0;
          dtlb_trans_reload_we $==. 0;
          dtlb_trans_reload_din $==. 0;
          dtlb_match_reload_we $==. 0;
          dtlb_match_reload_din $==. 0;
        ] in
        tlb_reload_pagefault_o 
    in

    (* rams *)
    let dtm = Ram.(ram_inst
      I.({
        clk = i.clk;
        addr_a = dtlb_match_addr;
        we_a = dtlb_match_we;
        din_a = dtlb_match_din;
        addr_b = dtlb_match_huge_addr;
        we_b  = dtlb_match_huge_we;
        din_b = dtlb_match_reload_din#q;
      }))
    in

    let dtt = Ram.(ram_inst
      I.({
        clk = i.clk;
        addr_a = dtlb_trans_addr;
        we_a = dtlb_trans_we;
        din_a = dtlb_trans_din;
        addr_b = dtlb_trans_huge_addr;
        we_b  = dtlb_trans_huge_we;
        din_b = dtlb_trans_reload_din#q;
      }))
    in
    
    let () = Ram.O.(
      dtlb_match_dout <== dtm.dout_a;
      dtlb_match_huge_dout <== dtm.dout_b;
      dtlb_trans_dout <== dtt.dout_a;
      dtlb_trans_huge_dout <== dtt.dout_b;
    ) in

    O.({
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
    })

  module Inst = M.Inst(I)(O)
  let dmmu_inst = Inst.inst "dmmu" dmmu

end

