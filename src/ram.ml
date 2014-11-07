(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Description: simple and true dual port rams

  Todo: vendor implementations

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

open HardCaml.Signal.Comb

module type S = sig
  val addr_width : int
  val data_width : int
end

module Simple_dp(S : S) = struct

  module I = interface
    clk[1]
    raddr[S.addr_width] re[1]
    waddr[S.addr_width] we[1]
    din[S.data_width]
  end

  module O = interface
    dout[S.data_width]
  end

  let ram ~enable_bypass i = 
    let open I in
    let module R = Utils.Regs(struct let clk = i.clk let rst = empty end) in
    let size = 1 lsl S.addr_width in
    let dout = R.ram_wbr size ~we:i.we ~wa:i.waddr ~d:i.din ~re:i.re ~ra:i.raddr in
    let dout = 
      if enable_bypass then
        let din_r = R.reg ~e:i.re i.din in
        let sel = (i.waddr ==: i.raddr) &: i.we &: i.re in
        let bypass = R.reg ~e:(sel |: i.re) (mux2 sel vdd gnd) in
        mux2 bypass din_r dout
      else dout
    in
    O.({ dout })

end

module True_dp(S : S) = struct
  
  module I = interface
    clk[1]
    addr_a[S.addr_width] we_a[1] din_a[S.data_width]
    addr_b[S.addr_width] we_b[1] din_b[S.data_width]
  end

  module O = interface
    dout_a[S.data_width]
    dout_b[S.data_width]
  end
 
  (* We only support single port rams in hardcaml, so we'll build this
   * from a mutli-lvt style ram (it instantiates multiple single port rams).
   * For proper implementation we will need to use vendor macros *)

  let ram i = 
    let open I in
    let module R = Utils.Regs(struct let clk = i.clk let rst = empty end) in
    let size = 1 lsl S.addr_width in
    R.multi_ram_wbr size
      ~wr:Utils.Multiram.(
        [| { we = i.we_a; wa = i.addr_a; wd = i.din_a; };
           { we = i.we_b; wa = i.addr_b; wd = i.din_b; }; |])
      ~rd:Utils.Multiram.(
        [| { re = ~: (i.we_a); ra = i.addr_a; };
           { re = ~: (i.we_b); ra = i.addr_b; }; |])

end

