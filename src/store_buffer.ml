(******************************************************************************
 This Source Code Form is subject to the terms of the
 Open Hardware Description License, v. 1.0. If a copy
 of the OHDL was not distributed with this file, You
 can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

 Description: Store buffer
 Currently a simple single clock FIFO, but with the ambition to
 have combining and reordering capabilities in the future.

 Copyright (C) 2013 Stefan Kristiansson <stefan.kristiansson@saunalahti.fi>
 Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

 ******************************************************************************)

open HardCaml.Signal.Comb

let operand_width = 32

module I = interface
  clk[1]
  rst[1]
  pc_i[operand_width]
  adr_i[operand_width]
  dat_i[operand_width]
  bsel_i[operand_width/8]
  atomic_i[1]
  write[1]
  read[1]
end

module O = interface
  pc_o[operand_width]
  adr_o[operand_width]
  dat_o[operand_width]
  bsel_o[operand_width/8]
  atomic_o[1]
  full[1]
  empty[1]
end

let store_buffer ~depth_width i = 
  let open I in
  let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in

  let waddr = R.reg_fb ~e:i.write ~w:(depth_width+1) (fun d -> d +:. 1) in
  let raddr = R.reg_fb ~e:i.read ~w:(depth_width+1) (fun d -> d +:. 1) in

  let din = i.adr_i @: i.dat_i @: i.bsel_i @: i.pc_i @: i.atomic_i in
  let fifo_data_width = width din in

  let module S = Ram.Simple_dp(struct
    let addr_width = depth_width
    let data_width = fifo_data_width
  end) in

  let fifo = 
    S.(ram ~enable_bypass:true
      I.{
        clk = i.clk;
        raddr = raddr.[depth_width-1:0];
        re = i.read;
        waddr = waddr.[depth_width-1:0];
        we = i.write;
        din = din
      }) 
  in
  
  let extract off wid = off+wid, fifo.S.O.dout.[off+wid-1:off] in
  let off, atomic_o = extract 0 1 in
  let off, pc_o = extract off operand_width in
  let off, bsel_o = extract off (operand_width/8) in
  let off, dat_o = extract off operand_width in
  let   _, adr_o = extract off operand_width in

  O.{
    pc_o; adr_o; dat_o; bsel_o; atomic_o;
    full = (waddr.[depth_width:depth_width] <>: raddr.[depth_width:depth_width]) &: 
           (waddr.[depth_width-1:0] <>: raddr.[depth_width-1:0]); 
    empty = waddr ==: raddr;
  }

