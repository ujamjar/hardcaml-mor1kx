open HardCaml.Signal.Comb
open Utils
open Option

let operand_width = 32

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

  ***************************************************************************** *)

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

    let dbus_bsel = bsel i.lsu_length dbus_adr in
    let dbus_we = i.op_lsu_store in
    let dbus_dat_aligned = aligned dbus_adr_r i.dbus_dat_i in
    let dbus_dat_extended = extended i.lsu_zext i.lsu_length dbus_dat_aligned in

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

  ***************************************************************************** *)

  module I = interface
    clk
    rst
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
(*
  let lsu o f i = 
    let open I in
    let module R = Regs(struct let clk = i.clk let rst = i.rst end) in

    let regm (c0,v0) (c1,v1) = R.reg_fb ~e:vdd ~w:1 (fun d -> mux2 c0 v0 @@ mux2 c1 v1 @@ d) in

    (***********)
    let dc_access = wire 1 in
    (***********)

    let rec snoop_valid = i.snoop_en &: (~: ((i.snoop_adr ==: dbus_adr_o) &: i.dbus_ack)) 
 
    and ctrl_op_lsu = i.ctrl_op_lsu_load |: i.ctrl_op_lsu_store 
 
    and lsu_sdat = 
      mux2 (i.ctrl_lsu_length ==:. 0b00) (repeat (sel_bottom i.ctrl_rfb  8) 4) @@ (* byte access *)
      mux2 (i.ctrl_lsu_length ==:. 0b01) (repeat (sel_bottom i.ctrl_rfb 16) 2) @@ (* halfword access *)
		  i.ctrl_rfb                                                                  (* word access *)

    and align_err_word = reduce (|:) (bits i.ctrl_lsu_adr.[1:0]) 
    and align_err_short = i.ctrl_lsu_adr.[0:0] 
 
    and lsu_valid_o = (lsu_ack |: access_done) &: (~: tlb_reload_busy) &: (~: dc_snoop_hit) 
 
    and lsu_except_dbus_o = except_dbus |: store_buffer_err
 
    and align_err = 
      (i.ctrl_lsu_length ==:. 0b10) &: align_err_word |: 
      (i.ctrl_lsu_length ==:. 0b01) &: align_err_short 
 
    and except_align = ctrl_op_lsu &: align_err 
 
    and lsu_except_align_o = except_align &: (~: (i.pipeline_flush)) 
 
    and except_dtlb_miss = ctrl_op_lsu &: tlb_miss &: i.dmmu_enable &: (~: tlb_reload_busy) 
 
    and lsu_except_dtlb_miss_o = except_dtlb_miss &: (~: (i.pipeline_flush)) 
 
    and except_dpagefault = 
      ctrl_op_lsu &: pagefault &: i.dmmu_enable &: 
      (~: tlb_reload_busy) |: tlb_reload_pagefault 
 
    and lsu_except_dpagefault_o = except_dpagefault &: (~: (i.pipeline_flush)) 

    and access_done = regm (i.padv_execute, gnd) (lsu_ack,vdd) 
    and except_dbus = regm (i.padv_execute |: i.pipeline_flush, gnd) (i.dbus_err, vdd) 
    and except_dtlb_miss_r = regm (i.padv_execute, gnd) (except_dtlb_miss, vdd) 
    and except_dpagefault_r = regm (i.padv_execute, gnd) (except_dpagefault, vdd) 
    and store_buffer_err = regm (i.pipeline_flush, gnd) (i.dbus_err &: dbus_we, vdd) 

    and dbus_we = dbus_we_r &: ((~: dbus_atomic) |: atomic_reserve) 
    and dbus_bsel = bsel i.ctrl_lsu_length i.ctrl_lsu_adr 
    and dbus_dat_aligned = aligned i.ctrl_lsu_adr lsu_ldat 
    and dbus_dat_extended = extended i.ctrl_lsu_zext i.ctrl_lsu_length dbus_dat_aligned 

    and dbus_access = 
      ((~: dc_access) |: tlb_reload_busy |: i.ctrl_op_lsu_store) &:
			(state <>: DC_REFILL) |: (state ==: WRITE) (* XXX *)

    and dc_result_r = R.reg ~e:vdd dc_refill 

    and store_buffer_ack = if f.store_buffer then store_buffer_write else write_done 

    and lsu_ack = 
        mux2 (i.ctrl_op_lsu_store_i |: state ==: WRITE) 
          (store_buffer_ack &: (~: (i.ctrl_op_lsu_atomic)) |: 
           write_done &: i.ctrl_op_lsu_atomic) 
        (mux2 dbus_access dbus_ack dc_ack)

    and lsu_ldat = mux2 dbus_access dbus_dat dc_ldat

    and dbus_adr_o = dbus_adr

    and dbus_dat_o = dbus_dat

    and dbus_burst_o = (state ==: DC_REFILL) &: (~: dc_refill_done)

    and next_dbus_adr = 
      if o.dcache_block_width = 5 then 
			  (dbus_adr.[31:5] @: (dbus_adr.[4:0] +:. 4)) (* 32 byte *)
      else
			  (dbus_adr.[31:4] @: (dbus_adr.[3:0] +:. 4)) (* 16 byte *)

    and dbus_err = R.reg ~e:vdd i.dbus_err 

    (* XXXXXXXX *)
    and tlb_reload_busy = gnd
    and dc_snoop_hit = gnd
    and tlb_miss = gnd
    and pagefault = gnd
    and tlb_reload_pagefault = gnd
    and dbus_we_r = gnd
    and dbus_atomic = gnd 
    and atomic_reserve = gnd
    in

    O.(map (fun (_,b) -> zero b) t)
*)
end


