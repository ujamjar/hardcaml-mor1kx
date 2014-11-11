open HardCaml.Signal.Comb

let rf_addr_width = 5
let operand_width = 32

module Espresso = struct

  module I = interface
    clk[1] rst[1]
    rfd_adr[rf_addr_width] rfa_adr[rf_addr_width] rfb_adr[rf_addr_width] 
    rf_we[1] rf_re[1]
    result[operand_width]
  end

  module O = interface
    rfa[operand_width] rfb[operand_width]
  end

  let rf i = 
    let open I in
    let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in

    let rf_wren = i.rf_we in

    let rfa_r = R.reg ~e:i.rf_re i.rfa_adr in
    let rfb_r = R.reg ~e:i.rf_re i.rfb_adr in
    (*let rfd_r = R.reg ~e:i.rf_re i.rfd_adr in*)

    let rfd_last = R.reg ~e:rf_wren i.rfd_adr in
    let result_last = R.reg ~e:rf_wren i.result in

    let rfa_o_use_last = (rfd_last ==: rfa_r) in
    let rfb_o_use_last = (rfd_last ==: rfb_r) in

    let rfa_rden_for_last = (rfa_o_use_last &: (~: (i.rf_re))) in
    let rfb_rden_for_last = (rfb_o_use_last &: (~: (i.rf_re))) in

    let rfa_rden = i.rf_re |: rfa_rden_for_last in
    let rfb_rden = i.rf_re |: rfb_rden_for_last in

    (*let rfa_o_using_last = R.reg_fb ~e:vdd ~w:1 (fun d -> 
      mux2 (~: d) (rfa_o_use_last &: (~: rfa_rden)) @@
      mux2 rfa_rden gnd d) in

    let rfb_o_using_last = R.reg_fb ~e:vdd ~w:1 (fun d -> 
      mux2 (~: d) (rfb_o_use_last &: (~: rfb_rden)) @@
      mux2 rfb_rden gnd d) in*)

    let module S = Ram.Simple_dp(struct
      let addr_width = rf_addr_width
      let data_width = operand_width
    end) in

    let rfa = 
      S.(ram ~enable_bypass:false
        I.{
          clk = i.clk;
          raddr = i.rfa_adr;
          re = rfa_rden;
          waddr = i.rfd_adr;
          we = rf_wren;
          din = i.result;
        })
    in

    let rfb = 
      S.(ram ~enable_bypass:false
        I.{
          clk = i.clk;
          raddr = i.rfb_adr;
          re = rfb_rden;
          waddr = i.rfd_adr;
          we = rf_wren;
          din = i.result;
        })
    in

    let rfa = mux2 rfa_o_use_last result_last rfa.S.O.dout in
    let rfb = mux2 rfb_o_use_last result_last rfb.S.O.dout in

    O.{ rfa; rfb }

end

