(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Description: mor1kx PIC

  Copyright (C) 2012 Authors

  Author(s): Julius Baxter <juliusbaxter@gmail.com>
             Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

open Defines
open Option
open Utils

module Make(M : Utils.Module_cfg_signal) = struct

  open M.Bits

  module I = interface
    clk[1]
    rst[1]
    irq[32]
    spr_access[1]
    spr_we[1]
    spr_addr[16]
    spr_dat_i[32]
  end

  module O = interface
    spr_picmr[32]
    spr_picsr[32]
    spr_bus_ack[1]
    spr_dat_o[32]
  end

  let irqmap f irq_unmasked spr_dat_i = 
    let a = Array.init 32 (fun i -> f (bit irq_unmasked i) (bit spr_dat_i i)) in
    concat @@ Array.to_list a

  let edge irq_unmasked spr_picsr_access i = 
    let open I in
    let open HardCaml.Signal.Guarded in
    let module R = Regs(struct let clk = i.clk let rst = i.rst end) in
    let irqline irq_unmasked spr_dat_i = 
      let picsr = R.g_reg ~e:vdd 1 in
      let () = compile [
        g_if (i.spr_we &: spr_picsr_access) [
          picsr $== mux2 spr_dat_i gnd picsr#q;
        ] @@ g_elif ((~: (picsr#q)) &: irq_unmasked) [
          picsr $==. 1;
        ] []
      ] in
      picsr#q
    in
    irqmap irqline irq_unmasked i.spr_dat_i

  let level irq_unmasked = irq_unmasked

  let latched_level irq_unmasked spr_picsr_access i = 
    let open I in
    let open HardCaml.Signal.Guarded in
    let module R = Regs(struct let clk = i.clk let rst = i.rst end) in
    let irqline irq_unmasked spr_dat_i = 
      let picsr = R.g_reg ~e:vdd 1 in
      let () = compile [
        g_if (i.spr_we &: spr_picsr_access) [
          picsr $== (irq_unmasked |: spr_dat_i);
        ] @@ g_elif ((~: (picsr#q)) &: irq_unmasked) [
          picsr $== (picsr#q |: irq_unmasked);
        ] []
      ] in
      picsr#q
    in
    irqmap irqline irq_unmasked i.spr_dat_i

  let pic i = 
    let open I in
    let open HardCaml.Signal.Guarded in
    let module R = Regs(struct let clk = i.clk let rst = i.rst end) in

    let spr_picmr_access = i.spr_access &: M.(Spr.offset i.spr_addr ==:. Spr.Pic.picmr) in
    let spr_picsr_access = i.spr_access &: M.(Spr.offset i.spr_addr ==:. Spr.Pic.picsr) in
    
    let spr_picmr =
      let nmi_mask x = 
        if M.o.pic_nmi_width = 0 then i.spr_dat_i
        else x.[31:M.o.pic_nmi_width] @: ones M.o.pic_nmi_width
      in
      R.reg 
        ~cv:(nmi_mask (zero 32))
        ~rv:(nmi_mask (zero 32))
        ~e:(i.spr_we ^: spr_picmr_access)
        (nmi_mask i.spr_dat_i)
    in

    let irq_unmasked = spr_picmr &: i.irq in

    let spr_picsr = 
      match M.o.pic_trigger with
      | Edge -> edge irq_unmasked spr_picsr_access i  
      | Level -> level irq_unmasked 
      | Latched_level -> latched_level irq_unmasked spr_picsr_access i
    in

    let spr_bus_ack = i.spr_access in
    let spr_dat_o = 
      mux2 (i.spr_access &: spr_picsr_access) spr_picsr @@
      mux2 (i.spr_access &: spr_picmr_access) spr_picmr @@
      zero 32
    in

    O.({
      spr_picmr;
      spr_picsr;
      spr_bus_ack;
      spr_dat_o;
    })

  module Inst = M.Inst(I)(O)
  let pic_inst = Inst.inst "pic" pic 

end

