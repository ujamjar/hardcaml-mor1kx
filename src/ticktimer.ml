(* ****************************************************************************
 This Source Code Form is subject to the terms of the
 Open Hardware Description License, v. 1.0. If a copy
 of the OHDL was not distributed with this file, You
 can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

 Description: mor1kx tick timer unit

 Copyright (C) 2012 Authors

 Author(s): Julius Baxter <juliusbaxter@gmail.com>
            Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module Make(M : Utils.Module_cfg_signal) = struct

  open M.Bits
  module L = Utils.Logic(M.Bits)
  open L

  module I = interface
    clk[1] rst[1]
    spr_access[1] spr_we[1] spr_addr[16] spr_dat_i[32]
  end

  module O = interface
    spr_ttmr[32] spr_ttcr[32] spr_bus_ack[1] spr_dat_o[32]
  end

  let ticktimer i = 
    let open I in
    let open HardCaml.Signal.Guarded in
    let open Utils in
    let module R = Regs(struct let clk = i.clk let rst = i.rst end) in

    let spr_ttmr = R.g_reg ~e:vdd 32 in
    let spr_ttcr = R.g_reg ~e:vdd 32 in

    let cmp_spr_access x = i.spr_access &: (M.Spr.offset i.spr_addr ==:. x) in 
    let spr_ttmr_access = cmp_spr_access M.Spr.Tt.ttmr in
    let spr_ttcr_access = cmp_spr_access M.Spr.Tt.ttcr in


    let spr_bus_ack = i.spr_access in
    let spr_dat_o = 
      mux2 (i.spr_access &: spr_ttcr_access) spr_ttcr#q @@
      mux2 (i.spr_access &: spr_ttmr_access) spr_ttmr#q @@ 
      (zero 32)
    in

    let ttcr_match = (sel_bottom spr_ttcr#q 28) ==: sel_bottom spr_ttmr#q 28 in

    let ttcr_clear = (sel_top spr_ttmr#q 2 ==:. 0b01) &: ttcr_match in
    let ttcr_run = 
      (sel_top spr_ttmr#q 2 <>:. 0b00) &: (~: ttcr_match) |: (sel_top spr_ttmr#q 2 ==:. 0b11) 
    in

    let () = compile [
      g_if (i.spr_we &: spr_ttmr_access) [
        spr_ttmr $== i.spr_dat_i;
      ] @@ g_elif (ttcr_match &: (bit spr_ttmr#q 29)) [
        spr_ttmr $==. 1;
      ] [];

      g_if (i.spr_we &: spr_ttcr_access) [
        spr_ttcr $== i.spr_dat_i;
      ] @@ g_elif (ttcr_clear) [
        spr_ttcr $==. 0;
      ] @@ g_elif (ttcr_run) [
        spr_ttcr $== spr_ttcr#q +:. 1;
      ] []
    ] in

    O.({
      spr_ttmr = spr_ttmr#q;
      spr_ttcr = spr_ttcr#q;
      spr_bus_ack;
      spr_dat_o;
    })

  module Inst = M.Inst(I)(O)
  let ticktimer_inst = Inst.inst "ticktimer" ticktimer

end

