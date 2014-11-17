open Utils
open Option

module U(B : HardCaml.Comb.S) = struct
  open B
  module L = Utils.Logic(B)
  open L

  let bsel length adr = mux length [
    mux adr.[1:0] (List.map (consti 4) [ 0b1000; 0b0100; 0b0010; 0b0001]);
    mux adr.[1:1] (List.map (consti 4) [ 0b1100; 0b0011; ]);
    consti 4 0b1111;
    consti 4 0b1111;
  ]

  let aligned adr dat = mux adr.[1:0] [
    dat;
    dat.[23:0] @: zero  8;
    dat.[15:0] @: zero 16;
    dat.[ 7:0] @: zero 24;
  ]
  let extended zext length dat = cases (zext @: length) dat [
      0b100, zero 24 @: dat.[31:24];
      0b101, zero 16 @: dat.[31:16];
      0b000, repeat dat.[31:31] 24 @: dat.[31:24];
      0b001, repeat dat.[31:31] 16 @: dat.[31:16];
  ]

end

module Espresso = struct

  (* ****************************************************************************
    This Source Code Form is subject to the terms of the
    Open Hardware Description License, v. 1.0. If a copy
    of the OHDL was not distributed with this file, You
    can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

    Description:  Load, store unit for espresso pipeline

    All combinatorial outputs to pipeline
    Dbus interface request signal out synchronous

    32-bit specific due to sign extension of results

    Copyright (C) 2012 Authors

    Author(s): Julius Baxter <juliusbaxter@gmail.com>
               Andy Ray <andy.ray@ujamjar.com>

  ***************************************************************************** *)

  module Make(M : Utils.Module_cfg_signal) = struct

    open M.Bits
    module U = U(M.Bits)
    module L = Utils.Logic(M.Bits)
    open L

    let operand_width = M.o.operand_width

    module I = interface
      clk[1] 
      rst[1] 
      padv_fetch[1] 
      lsu_adr[operand_width] 
      rfb[operand_width] 
      op_lsu_load[1]
      op_lsu_store[1] 
      lsu_length[2] 
      lsu_zext[1] 
      exception_taken[1]
      du_restart[1] 
      stepping[1] 
      next_fetch_done[1] 
      dbus_err[1]
      dbus_ack[1] 
      dbus_dat_i[operand_width]
    end

    module O = interface
      lsu_result[operand_width] 
      lsu_valid[1] 
      lsu_except_dbus[operand_width] 
      lsu_except_align[1]
      dbus_adr[operand_width] 
      dbus_req[1] 
      dbus_dat_o[operand_width] 
      dbus_bsel[4] 
      dbus_we[1]
      dbus_burst[1]
    end

    let lsu ~registered_io i = 
      let open I in
      let module R = Regs(struct let clk = i.clk let rst = i.rst end) in

      let load_or_store = i.op_lsu_load |: i.op_lsu_store in
      let execute_go = R.reg ~e:vdd (i.padv_fetch |: (i.stepping &: i.next_fetch_done)) in
      let dbus_adr_r = R.reg ~e:(execute_go &: load_or_store) i.lsu_adr in
      let dbus_err_r = if registered_io then R.reg ~e:vdd i.dbus_err else i.dbus_err in
      let op_lsu = if registered_io then R.reg ~e:vdd load_or_store else load_or_store in
      let dbus_adr = 
        if registered_io then dbus_adr_r else mux2 execute_go i.lsu_adr dbus_adr_r
      in

      let align_err_word = reduce (|:) (bits dbus_adr.[1:0]) in
      let align_err_short = dbus_adr.[0:0] in
      let align_err = 
        (i.lsu_length ==:. 0b10) &: align_err_word |:
        (i.lsu_length ==:. 0b01) &: align_err_short
      in
      let except_align = 
        if registered_io then op_lsu &: load_or_store &: align_err &: (~: execute_go) 
        else op_lsu &: align_err 
      in
      let except_align_r = R.reg ~e:vdd (mux2 i.exception_taken gnd except_align) in
      let lsu_except_align = except_align_r in
      let except_dbus = R.reg_fb ~e:vdd ~w:1 
        (fun except_dbus ->
          mux2 i.exception_taken gnd @@
          mux2 dbus_err_r vdd @@
          except_dbus)
      in

      let access_done = R.reg_fb ~e:vdd ~w:1 
        (fun access_done ->
          mux2 (i.padv_fetch |: i.du_restart) gnd @@
          mux2 (i.dbus_ack |: dbus_err_r |: lsu_except_align) vdd @@
          access_done)
      in

      let dbus_dat_o = 
        mux2 (i.lsu_length ==:. 0b00) (repeat (sel_bottom i.rfb  8) 4) @@ (* byte access *)
        mux2 (i.lsu_length ==:. 0b01) (repeat (sel_bottom i.rfb 16) 2) @@ (* halfword access *)
        i.rfb                                                             (* word access *)
      in

      let lsu_valid = i.dbus_ack |: dbus_err_r |: access_done in
      let lsu_except_dbus = dbus_err_r |: except_dbus in
      let lsu_except_align = except_align_r in

      let dbus_bsel = U.bsel i.lsu_length dbus_adr in
      let dbus_we = i.op_lsu_store in
      let dbus_dat_aligned = U.aligned dbus_adr_r i.dbus_dat_i in
      let dbus_dat_extended = U.extended i.lsu_zext i.lsu_length dbus_dat_aligned in

      let dbus_burst = gnd in

      let lsu_result_r = R.reg ~e:(i.dbus_ack &: i.op_lsu_load) dbus_dat_extended in
      let lsu_result = mux2 access_done lsu_result_r dbus_dat_extended in

      let dbus_req = 
        if registered_io then
          (~: execute_go) &: op_lsu &: 
          (~: (except_align |: except_align_r)) &: 
          (~: access_done) 
        else
          op_lsu &: (~: except_align) &: (~: access_done)
      in

      O.({
        lsu_result; 
        lsu_valid;
        lsu_except_dbus;
        lsu_except_align;
        dbus_adr;
        dbus_req;
        dbus_dat_o;
        dbus_bsel;
        dbus_we;
        dbus_burst;
      })

    module Inst = M.Inst(I)(O)
    let lsu_inst ~registered_io = Inst.inst "lsu" (lsu ~registered_io)

  end

end

module Cappuccino = struct
    
  (* ****************************************************************************
    This Source Code Form is subject to the terms of the
    Open Hardware Description License, v. 1.0. If a copy
    of the OHDL was not distributed with this file, You
    can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

    Description:  Data bus interface

    All combinatorial outputs to pipeline
    Dbus interface request signal out synchronous

    32-bit specific

    Copyright (C) 2012 Julius Baxter <juliusbaxter@gmail.com>
    Copyright (C) 2013 Stefan Kristiansson <stefan.kristiansson@saunalahti.fi>
    Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

  ***************************************************************************** *)

  module Make(M : Utils.Module_cfg_signal) = struct

    open M.Bits
    module U = U(M.Bits)
    module L = Utils.Logic(M.Bits)
    open L

    open HardCaml.Signal.Guarded

    let operand_width = M.o.operand_width

    module Sb = Store_buffer.Make(M)
    module Dc = Dcache.Make(M)
    module Dmmu = Dmmu.Make(M)

    module I = interface
      clk[1]
      rst[1]
      padv_execute[1]
      padv_ctrl[1] 
      decode_valid[1]
      exec_lsu_adr[operand_width]
      ctrl_lsu_adr[operand_width]
      ctrl_rfb[operand_width]
      exec_op_lsu_load[1]
      exec_op_lsu_store[1]
      exec_op_lsu_atomic[1]
      ctrl_op_lsu_load[1]
      ctrl_op_lsu_store[1]
      ctrl_op_lsu_atomic[1]
      ctrl_lsu_length[2]
      ctrl_lsu_zext[1]
      ctrl_epcr[operand_width]
      spr_bus_addr[16]
      spr_bus_we[1]
      spr_bus_stb[1]
      spr_bus_dat[operand_width]
      dc_enable[1]
      dmmu_enable[1]
      supervisor_mode[1]
      dbus_err[1]
      dbus_ack[1]
      dbus_dat_i[operand_width]
      pipeline_flush[1]
      snoop_adr[32]
      snoop_en[1]
    end

    module O = interface
      store_buffer_epcr[operand_width]
      lsu_result[operand_width]
      lsu_valid[1]
      lsu_except_dbus[1]
      lsu_except_align[1]
      lsu_except_dtlb_miss[1]
      lsu_except_dpagefault[1]
      store_buffer_err[1]
      atomic_flag_set[1]
      atomic_flag_clear[1]
      spr_bus_dat_dc[operand_width]
      spr_bus_ack_dc[1]
      spr_bus_dat_dmmu[operand_width]
      spr_bus_ack_dmmu[1]
      dbus_adr[operand_width]
      dbus_req[1]
      dbus_dat_o[operand_width]
      dbus_bsel[4]
      dbus_we[1]
      dbus_burst[1]
    end

    type sm = Idle | Dc_refill | Read | Write | Tlb_reload 
      deriving(Bounded, Enum)

    let lsu i = 
      let open I in
      let module R = Regs(struct let clk = i.clk let rst = i.rst end) in

      let regm x0 x1 = R.reg_fb ~e:vdd ~w:1 (pmux [ x0; x1 ]) in
      let state_is, sm, next = R.statemachine ~e:vdd 
        (Enum_sm.enum_from_to Bounded_sm.min_bound Bounded_sm.max_bound)
      in
      let s_idle, s_dc_refill, s_read, s_write, s_tlb_reload = 
        state_is Idle, state_is Dc_refill, state_is Read, state_is Write, state_is Tlb_reload
      in 

      let sb = Sb.O.(map (fun (_,b) -> wire b) t) in
      let dc = Dc.O.(map (fun (_,b) -> wire b) t) in
      let dmmu = Dmmu.O.(map (fun (_,b) -> wire b) t) in

      let write_done = R.g_reg ~e:vdd 1 in
      let last_write = R.g_reg ~e:vdd 1 in

      let tlb_reload_ack = R.g_reg ~e:vdd 1 in
      let tlb_reload_done = R.g_reg ~e:vdd 1 in
      let tlb_reload_data = R.g_reg ~e:vdd operand_width in

      let dbus_ack = R.g_reg ~e:vdd 1 in
      let dbus_req = R.g_reg ~e:vdd 1 in
      let dbus_we = R.g_reg ~e:vdd 1 in
      let dbus_adr = R.g_reg ~e:vdd operand_width in
      let dbus_bsel_o = R.g_reg ~e:vdd 4 in
      let dbus_atomic = R.g_reg ~e:vdd 1 in
      let dbus_dat = R.g_reg ~e:vdd operand_width in

      let tlb_reload_busy = dmmu.Dmmu.O.tlb_reload_busy in
      let tlb_reload_req = dmmu.Dmmu.O.tlb_reload_req in
      let tlb_reload_addr = dmmu.Dmmu.O.tlb_reload_addr in
      let tlb_reload_pagefault = dmmu.Dmmu.O.tlb_reload_pagefault in
      let tlb_miss = dmmu.Dmmu.O.tlb_miss in
      let pagefault = dmmu.Dmmu.O.pagefault in
      let dmmu_cache_inhibit = dmmu.Dmmu.O.cache_inhibit in
      let dmmu_phys_addr = dmmu.Dmmu.O.phys_addr in

      let dc_refill = dc.Dc.O.refill in
      let dc_refill_req = dc.Dc.O.refill_req in
      let dc_refill_done = dc.Dc.O.refill_done in
      let dc_ack = dc.Dc.O.cpu_ack in
      let dc_ldat = dc.Dc.O.cpu_dat_o in
      let dc_snoop_hit = dc.Dc.O.snoop_hit in

      let store_buffer_epcr = sb.Sb.O.pc_o in
      let store_buffer_atomic = sb.Sb.O.atomic_o in
      let store_buffer_bsel = sb.Sb.O.bsel_o in
      let store_buffer_radr = sb.Sb.O.adr_o in
      let store_buffer_dat = sb.Sb.O.dat_o in
      let store_buffer_full = sb.Sb.O.full in
      let store_buffer_empty = sb.Sb.O.empty in

      let dc_access = wire 1 in
      let dc_adr_match = wire 32 in
      let atomic_reserve = wire 1 in
      let store_buffer_write = wire 1 in
      let lsu_ack = wire 1 in
      let lsu_ldat = wire operand_width in

      (***********)

      let snoop_valid = i.snoop_en &: (~: ((i.snoop_adr ==: dbus_adr#q) &: i.dbus_ack)) in
  
      let ctrl_op_lsu = i.ctrl_op_lsu_load |: i.ctrl_op_lsu_store in
  
      let lsu_sdat = 
        mux2 (i.ctrl_lsu_length ==:. 0b00) 
          (repeat (sel_bottom i.ctrl_rfb  8) 4) @@ (* byte access *)
        mux2 (i.ctrl_lsu_length ==:. 0b01) 
          (repeat (sel_bottom i.ctrl_rfb 16) 2) @@ (* halfword access *)
        i.ctrl_rfb                                 (* word access *)
      in

      let align_err_word = reduce (|:) (bits i.ctrl_lsu_adr.[1:0]) in
      let align_err_short = i.ctrl_lsu_adr.[0:0] in
  
      let align_err = 
        (i.ctrl_lsu_length ==:. 0b10) &: align_err_word |: 
        (i.ctrl_lsu_length ==:. 0b01) &: align_err_short 
      in

      let except_align = ctrl_op_lsu &: align_err in
  
      let except_dtlb_miss = ctrl_op_lsu &: tlb_miss &: i.dmmu_enable &: (~: tlb_reload_busy) in
  
      let except_dpagefault = 
        ctrl_op_lsu &: pagefault &: i.dmmu_enable &: 
        (~: tlb_reload_busy) |: tlb_reload_pagefault 
      in

      let access_done = regm (i.padv_execute, gnd) (lsu_ack,vdd) in
      let except_dbus = regm (i.padv_execute |: i.pipeline_flush, gnd) (i.dbus_err, vdd) in
      (*let except_dtlb_miss_r = regm (i.padv_execute, gnd) (except_dtlb_miss, vdd) in
      let except_dpagefault_r = regm (i.padv_execute, gnd) (except_dpagefault, vdd) in *)
      let store_buffer_err = regm (i.pipeline_flush, gnd) (i.dbus_err &: dbus_we#q, vdd) in

      let dbus_we_o = dbus_we#q &: ((~: (dbus_atomic#q)) |: atomic_reserve) in
      let dbus_bsel = U.bsel i.ctrl_lsu_length i.ctrl_lsu_adr in
      let dbus_dat_aligned = U.aligned i.ctrl_lsu_adr lsu_ldat in
      let dbus_dat_extended = U.extended i.ctrl_lsu_zext i.ctrl_lsu_length dbus_dat_aligned in

      let dbus_access = 
        ((~: dc_access) |: tlb_reload_busy |: i.ctrl_op_lsu_store) &:
        (~: (s_dc_refill)) |: (s_write) (* XXX *)
      in
      let dc_refill_r = R.reg ~e:vdd dc_refill in

      let store_buffer_ack = if M.f.store_buffer then store_buffer_write else write_done#q in
      let store_buffer_wadr = dc_adr_match in

      let () = lsu_ack <== 
          (mux2 (i.ctrl_op_lsu_store |: s_write) 
            (store_buffer_ack &: (~: (i.ctrl_op_lsu_atomic)) |: 
              write_done#q &: i.ctrl_op_lsu_atomic) 
            (mux2 dbus_access dbus_ack#q dc_ack))
      in
      let () = lsu_ldat <== (mux2 dbus_access dbus_dat#q dc_ldat) in

      let next_dbus_adr = 
        if M.o.dcache_block_width = 5 then 
          (dbus_adr#q.[31:5] @: (dbus_adr#q.[4:0] +:. 4)) (* 32 byte *)
        else
          (dbus_adr#q.[31:4] @: (dbus_adr#q.[3:0] +:. 4)) (* 16 byte *)
      in
      let dbus_err = R.reg ~e:vdd i.dbus_err in

      let lsu_valid = (lsu_ack |: access_done) &: (~: tlb_reload_busy) &: (~: dc_snoop_hit) in
      
      let () = compile [
  
        dbus_ack $==. 0;
        write_done $==. 0;
        tlb_reload_ack $==. 0;
        tlb_reload_done $==. 0;
        sm [
          Idle, [
            dbus_req $==. 0;
            dbus_we $==. 0;
            dbus_adr $==. 0;
            dbus_bsel_o $==. 0xf;
            dbus_atomic $==. 0;
            last_write $==. 0;
            g_if (store_buffer_write |: (~: store_buffer_empty)) [
                next Write;
            ] @@ g_elif (ctrl_op_lsu &: dbus_access &: (~: dc_refill) &: (~: (dbus_ack#q)) &:
                          (~: dbus_err) &: (~: except_dbus) &: (~: access_done) &:
                          (~: (i.pipeline_flush))) [
                g_if (tlb_reload_req) [
                  dbus_adr $== tlb_reload_addr;
                  dbus_req $==. 1;
                  next Tlb_reload;
                ] @@ g_elif (i.dmmu_enable) [
                  dbus_adr $== dmmu_phys_addr;
                  g_when ((~: tlb_miss) &: (~: pagefault) &: (~: except_align)) [
                      g_when (i.ctrl_op_lsu_load) [
                        dbus_req $==. 1;
                        dbus_bsel_o $== dbus_bsel;
                        next Read;
                      ]
                  ]
                ] @@ g_elif (~: except_align) [
                  dbus_adr $== i.ctrl_lsu_adr;
                  g_when (i.ctrl_op_lsu_load) [
                      dbus_req $==. 1;
                      dbus_bsel_o $== dbus_bsel;
                      next Read;
                  ]
                ] []
            ] @@ g_elif (dc_refill_req) [
                dbus_req $==. 1;
                dbus_adr $== dc_adr_match;
                next Dc_refill;
            ] []
          ];

          Dc_refill, [
            dbus_req $==. 1;
            g_when (i.dbus_ack) [
                dbus_adr $== next_dbus_adr;
                g_when (dc_refill_done) [
                  dbus_req $==. 0;
                  next Idle;
                ]
            ];

            (* TODO: only abort on snoop-hits to refill address *)
            g_when (i.dbus_err |: dc_snoop_hit) [
                dbus_req $==. 0;
                next Idle;
            ];
          ];

          Read, [
            dbus_ack $== i.dbus_ack;
            dbus_dat $== i.dbus_dat_i;
            g_when (i.dbus_ack |: i.dbus_err) [
                dbus_req $==. 0;
                next Idle;
            ];
          ];

          Write, [
            dbus_req $==. 1;
            dbus_we $==. 1;

            g_when ((~: (dbus_req#q)) |: i.dbus_ack &: (~: (last_write#q))) [
              dbus_bsel_o $== store_buffer_bsel;
              dbus_adr $== store_buffer_radr;
              dbus_dat $== store_buffer_dat;
              dbus_atomic $== store_buffer_atomic;
              last_write $== store_buffer_empty;
            ];

            g_when (store_buffer_write) [
              last_write $==. 0;
            ];

            g_when (last_write#q &: i.dbus_ack |: i.dbus_err) [
              dbus_req $==. 0;
              dbus_we $==. 0;
              g_when (~: store_buffer_write) [
                next Idle;
                write_done $==. 1;
              ]
            ]
          ];

          Tlb_reload, [
            dbus_adr $== tlb_reload_addr;
            tlb_reload_data $== i.dbus_dat_i;
            tlb_reload_ack $== (i.dbus_ack &: tlb_reload_req);

            g_when ((~: tlb_reload_req) |: i.dbus_err) [
              next Idle;
              tlb_reload_done $==. 1;
            ];

            dbus_req $== tlb_reload_req;
            g_when (i.dbus_ack |: tlb_reload_ack#q) [
              dbus_req $==. 0;
            ]
          ];

        ];

      ] in             

      (* atomic operations logic *)
      let atomic_flag_set, atomic_flag_clear, atomic_reserve' = 
        if M.f.atomic then
          let atomic_addr = R.reg 
            ~e:(i.ctrl_op_lsu_load &: i.ctrl_op_lsu_atomic &: i.padv_ctrl) 
            dc_adr_match in
          let atomic_reserve =
            let c0 = 
              i.pipeline_flush |: 
              (i.ctrl_op_lsu_store &: i.ctrl_op_lsu_atomic &: write_done#q ||:
              (~: (i.ctrl_op_lsu_atomic)) &: store_buffer_write &:
              (store_buffer_wadr ==: atomic_addr) ||:
              (snoop_valid &: (i.snoop_adr ==: atomic_addr)))
            in
            let c1 = (i.ctrl_op_lsu_load &: i.ctrl_op_lsu_atomic &: i.padv_ctrl) in
            regm (c0, gnd) (c1, (~: (snoop_valid &: (i.snoop_adr ==: dc_adr_match))))
          in
          let swa_success = i.ctrl_op_lsu_store &: i.ctrl_op_lsu_atomic &:
            atomic_reserve &: (dbus_adr#q ==: atomic_addr) in
          let atomic_flag_set = regm (i.padv_ctrl, gnd) (write_done#q, swa_success &: lsu_valid) in
          let atomic_flag_clear = regm (i.padv_ctrl, gnd) 
            (write_done#q, (~: swa_success) &: lsu_valid &: 
                           i.ctrl_op_lsu_atomic &: i.ctrl_op_lsu_store)
          in
          atomic_flag_set, atomic_flag_clear, atomic_reserve
        else
          gnd, gnd, gnd
      in
      let () = atomic_reserve <== atomic_reserve' in

      (* store buffer *)

      let dbus_stall = 
        tlb_reload_busy |: except_align |: except_dbus |:
        except_dtlb_miss |: except_dpagefault |:
        i.pipeline_flush
      in

      let store_buffer_write_pending = regm
        ((store_buffer_write |: i.pipeline_flush), gnd)
        ((i.ctrl_op_lsu_store &: i.padv_ctrl &: (~: dbus_stall) &:
          (store_buffer_full |: dc_refill |: dc_refill_r |: dc_snoop_hit)), vdd)
      in

      let () = store_buffer_write <== 
        ((i.ctrl_op_lsu_store &: 
          (i.padv_ctrl |: tlb_reload_done#q) |: store_buffer_write_pending) &:
        (~: store_buffer_full) &: (~: dc_refill) &: (~: dc_refill_r) &:
        (~: dbus_stall) &: (~: dc_snoop_hit)) 
      in

      let store_buffer_read = 
        (s_idle) &: store_buffer_write |:
        (s_idle) &: (~: store_buffer_empty) |:
        (s_write) &: (i.dbus_ack |: (~: (dbus_req#q))) &:
        ((~: store_buffer_empty) |: store_buffer_write) &:
        (~: (last_write#q)) |:
        (s_write) &: last_write#q &:
        store_buffer_write 
      in

      let sb' = 
        if M.f.store_buffer then
          Sb.store_buffer_inst ~depth_width:M.o.store_buffer_depth_width
            Sb.I.{
            clk = i.clk;
            rst = i.rst;
            pc_i = i.ctrl_epcr;
            adr_i = store_buffer_wadr;
            dat_i = lsu_sdat;
            bsel_i = dbus_bsel;
            atomic_i = i.ctrl_op_lsu_atomic;
            write = store_buffer_write;
            read = store_buffer_read;
          }
        else
          Sb.O.{
            pc_o = i.ctrl_epcr;
            adr_o = store_buffer_wadr;
            dat_o = lsu_sdat;
            bsel_o = dbus_bsel;
            atomic_o = gnd; (* XXX not assigned in rtl *)
            full = (~: (write_done#q));
            empty = vdd;
          }
      in
      let _ = Sb.O.(map2 (<==) sb sb') in

      (* data cache *)

      let dc_enable_r = regm 
        (i.dc_enable &: (~: (dbus_req#q)), vdd)
        (((~: (i.dc_enable)) &: (~: dc_refill)), gnd)
      in

      let dc_enabled = i.dc_enable &: dc_enable_r in
      let dc_adr = 
        mux2 (i.padv_execute &: (i.exec_op_lsu_load |: i.exec_op_lsu_store))
          i.exec_lsu_adr i.ctrl_lsu_adr
      in
      let () = dc_adr_match <== 
        (mux2 i.dmmu_enable
          (dmmu_phys_addr.[M.o.operand_width-1:2] @: zero 2)
          (i.ctrl_lsu_adr.[M.o.operand_width-1:2] @: zero 2)) 
      in

      let dc_req = ctrl_op_lsu &: dc_access &: (~: access_done) &: (~: dbus_stall) &:
        (~: (dbus_atomic#q &: dbus_we#q &: (~: atomic_reserve))) in
      let dc_refill_allowed = (~: (i.ctrl_op_lsu_store |: s_write)) &:
        (~: dc_snoop_hit) &: (~: snoop_valid) in
       

      let dc', dc_access' = 
        if M.f.datacache then
          let dc_access = 
            if (M.o.dcache_limit_width = M.o.operand_width) then
              i.ctrl_op_lsu_store |: dc_enabled &: (~: (dmmu_cache_inhibit &: i.dmmu_enable))
            else if (M.o.dcache_limit_width < M.o.operand_width) then
              i.ctrl_op_lsu_store |: dc_enabled &: 
                dc_adr_match.[M.o.operand_width-1: M.o.dcache_limit_width] ==:. 0 &: 
                (~: (dmmu_cache_inhibit &: i.dmmu_enable))
            else 
              failwith ("ERROR: Option.dcache_limit_width > Option.operand_width")
          in

          let dc_rst = i.rst |: dbus_err in
          let dc_bsel = dbus_bsel in
          let dc_we = 
            i.exec_op_lsu_store &: (~: (i.exec_op_lsu_atomic)) &: i.padv_execute |:
		        dbus_atomic#q &: dbus_we_o &: (~: (write_done#q)) |:
		        i.ctrl_op_lsu_store &: tlb_reload_busy &: (~: tlb_reload_req)
          in
          let dc = Dc.dcache_inst
            Dc.I.{
              clk = i.clk;
              rst = dc_rst; (* XXX NO NO NO *)
              dc_enable = dc_enabled;
              dc_access;
              cpu_dat_i = lsu_sdat;
              cpu_adr = dc_adr;
              cpu_adr_match = dc_adr_match;
              cpu_req = dc_req;
              cpu_we = dc_we;
              cpu_bsel = dc_bsel;
              refill_allowed = dc_refill_allowed;
              wradr = dbus_adr#q;
              wrdat = i.dbus_dat_i;
              we = i.dbus_ack;
              snoop_adr = i.snoop_adr.[31:0];
              snoop_valid;
              spr_bus_addr = i.spr_bus_addr;
              spr_bus_we = i.spr_bus_we;
              spr_bus_stb = i.spr_bus_stb;
              spr_bus_dat_i = i.spr_bus_dat.[M.o.operand_width:0];
            }
          in
          dc, dc_access 

        else
          Dc.O.{
            refill = gnd;
            refill_req = gnd;
            refill_done = gnd;
            cpu_err = gnd;
            cpu_ack = gnd;
            cpu_dat_o = zero 32;
            snoop_hit= gnd;
            spr_bus_dat_o = zero operand_width;
            spr_bus_ack = gnd;
            redundant = gnd;
          }, gnd
      in 
      let _ = Dc.O.(map2 (<==) dc dc') in
      let () = dc_access <== dc_access' in

      (* data mmu *)
      let dmmu' = 
        if M.f.dmmu then
          let virt_addr = dc_adr in

          (* small hack to delay dmmu spr reads by one cycle
              ideally the spr accesses should work so that the address is presented
              in execute stage and the delayed data should be available in control
              stage, but this is not how things currently work. *)
          let dmmu_spr_bus_stb = i.spr_bus_stb &: ((~: (i.padv_ctrl)) |: i.spr_bus_we) in
          let tlb_reload_pagefault_clear = ~: ctrl_op_lsu in (* use pipeline_flush_i? *)
          let dmmu_enable = i.dmmu_enable &: (~: (i.pipeline_flush)) in

          let dmmu = Dmmu.dmmu_inst
            Dmmu.I.{
              clk = i.clk;
              rst = i.rst;
              enable = dmmu_enable;
              virt_addr = virt_addr;
              virt_addr_match = i.ctrl_lsu_adr;
              op_store = i.ctrl_op_lsu_store;
              op_load = i.ctrl_op_lsu_load;
              supervisor_mode = i.supervisor_mode;
              tlb_reload_ack = tlb_reload_ack#q;
              tlb_reload_data = tlb_reload_data#q;
              tlb_reload_pagefault_clear = tlb_reload_pagefault_clear;
              spr_bus_addr = i.spr_bus_addr.[15:0];
              spr_bus_we = i.spr_bus_we;
              spr_bus_stb = dmmu_spr_bus_stb;
              spr_bus_dat_i = i.spr_bus_dat.[M.o.operand_width-1:0];
            }
          in
          dmmu
        else
          Dmmu.O.{
            phys_addr = dmmu_phys_addr;
            cache_inhibit = gnd;
            tlb_miss = gnd;
            pagefault = gnd;
            tlb_reload_req = gnd;
            tlb_reload_busy = gnd;
            tlb_reload_addr = zero operand_width;
            tlb_reload_pagefault = gnd;
            spr_bus_dat_o = zero operand_width;
            spr_bus_ack = gnd;
            redundant = gnd;
          }
      in
      let _ = Dmmu.O.(map2 (<==) dmmu dmmu') in

      let lsu_result = dbus_dat_extended in
      let lsu_except_dbus = except_dbus |: store_buffer_err in
      let lsu_except_align = except_align &: (~: (i.pipeline_flush)) in
      let lsu_except_dtlb_miss = except_dtlb_miss &: (~: (i.pipeline_flush)) in
      let lsu_except_dpagefault = except_dpagefault &: (~: (i.pipeline_flush)) in

      let dbus_adr = dbus_adr#q in
      let dbus_req = dbus_req#q in
      let dbus_dat_o = dbus_dat#q in
      let dbus_we = dbus_we_o in
      let dbus_burst = (s_dc_refill) &: (~: dc_refill_done) in

      O.{
        store_buffer_epcr;
        lsu_result;
        lsu_valid;
        lsu_except_dbus;
        lsu_except_align;
        lsu_except_dtlb_miss;
        lsu_except_dpagefault;
        store_buffer_err;
        atomic_flag_set;
        atomic_flag_clear;
        spr_bus_dat_dc = dc.Dc.O.spr_bus_dat_o;
        spr_bus_ack_dc = dc.Dc.O.spr_bus_ack;
        spr_bus_dat_dmmu = dmmu.Dmmu.O.spr_bus_dat_o;
        spr_bus_ack_dmmu = dmmu.Dmmu.O.spr_bus_ack;
        dbus_adr;
        dbus_req;
        dbus_dat_o;
        dbus_bsel;
        dbus_we;
        dbus_burst;
      }

    module Inst = M.Inst(I)(O)
    let lsu_inst = Inst.inst "lsu" lsu 

  end

end



