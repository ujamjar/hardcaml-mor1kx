
module Espresso = struct

  (* ****************************************************************************
    This Source Code Form is subject to the terms of the
    Open Hardware Description License, v. 1.0. If a copy
    of the OHDL was not distributed with this file, You
    can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

    Description: Register file for espresso pipeline

    We get addresses for A and B read directly in from the instruction bus

    Copyright (C) 2014 Authors

    Author(s): Julius Baxter <juliusbaxter@gmail.com>
               Andy Ray <andy.ray@ujamjar.com>

  ***************************************************************************** *)

  module Make(M : Utils.Module_cfg_signal) = struct

    open M.Bits
      
    let rf_addr_width = M.o.Option.rf_addr_width
    let operand_width = M.o.Option.operand_width

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
      end)(M) in

      let rfa = 
        S.(ram_inst ~enable_bypass:false
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
        S.(ram_inst ~enable_bypass:false
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

    module Inst = M.Inst(I)(O)
    let rf_inst = Inst.inst "rf" rf

  end

end

module Cappuccino = struct

  (* ****************************************************************************
    This Source Code Form is subject to the terms of the
    Open Hardware Description License, v. 1.0. If a copy
    of the OHDL was not distributed with this file, You
    can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

    Description: Register file for cappuccino pipeline
    Handles reading the register file rams and register bypassing.

    Copyright (C) 2014 Authors

    Author(s): Julius Baxter <juliusbaxter@gmail.com>
               Stefan Kristiansson <stefan.kristiansson@saunalahti.fi>
               Andy Ray <andy.ray@ujamjar.com>

  ***************************************************************************** *)

  module Make(M : Utils.Module_cfg_signal) = struct

    open M.Bits

    module I = interface
      clk[1]
      rst[1]
      padv_decode[1]
      padv_execute[1]
      padv_ctrl[1]
      decode_valid[1]
      fetch_rf_adr_valid[1]
      fetch_rfa_adr[M.o.Option.rf_addr_width]
      fetch_rfb_adr[M.o.Option.rf_addr_width]
      decode_rfa_adr[M.o.Option.rf_addr_width]
      decode_rfb_adr[M.o.Option.rf_addr_width]
      execute_rfd_adr[M.o.Option.rf_addr_width]
      ctrl_rfd_adr[M.o.Option.rf_addr_width]
      wb_rfd_adr[M.o.Option.rf_addr_width]
      spr_bus_addr[16]
      spr_bus_stb[1]
      spr_bus_we[1]
      spr_bus_dat[M.o.Option.operand_width]
      execute_rf_wb[1]
      ctrl_rf_wb[1]
      wb_rf_wb[1]
      result[M.o.Option.operand_width]
      ctrl_alu_result[M.o.Option.operand_width]
      pipeline_flush[1]
    end

    module O = interface 
      spr_gpr_ack[1]
      spr_gpr_dat[M.o.Option.operand_width]
      decode_rfa[M.o.Option.operand_width]
      decode_rfb[M.o.Option.operand_width]
      execute_rfa[M.o.Option.operand_width]
      execute_rfb[M.o.Option.operand_width]
    end

    let rf i = 
      let open I in
      let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in

      (* XXX how is this actually used?
      let rf_addr_width_ext = 
        match M.o.Option.rf_num_shadow_gpr with 
        | 0 -> 0
        | 1 -> 1
        | _ as x -> HardCaml.Utils.clog2 x
      in *)

      let rifel l = R.reg_fb ~e:vdd ~w:(width @@ snd @@ List.hd l) (pmux l) in

      (* Keep track of the flush signal, this is needed to not wrongly assert
          execute_hazard after an exception (or rfe) has happened.
          What happens in that case is that the instruction in execute stage is
          invalid until the next padv_decode, so it's execute_rfd_adr can not be
          used to evaluate the execute_hazard. *)
      let flushing = rifel [ i.pipeline_flush, vdd; i.padv_decode, gnd ] in
      
      (* Detect hazards *)
      let execute_hazard decode_rf_adr = rifel [
        i.pipeline_flush, gnd;
        i.padv_decode &: (~: flushing), i.execute_rf_wb &: (i.execute_rfd_adr ==: decode_rf_adr)
      ] in
      let execute_hazard_a = execute_hazard i.decode_rfa_adr in
      let execute_hazard_b = execute_hazard i.decode_rfb_adr in
   
      let execute_hazard_result_r = R.reg ~e:i.decode_valid i.ctrl_alu_result in
      let execute_hazard_result = mux2 i.decode_valid i.ctrl_alu_result execute_hazard_result_r in

      let ctrl_hazard decode_rf_adr = R.reg ~e:i.padv_decode 
        (i.ctrl_rf_wb &: (i.ctrl_rfd_adr ==: decode_rf_adr)) 
      in
      let ctrl_hazard_a = ctrl_hazard i.decode_rfa_adr in
      let ctrl_hazard_b = ctrl_hazard i.decode_rfb_adr in

      let ctrl_hazard_result_r = R.reg ~e:i.decode_valid i.result in
      let ctrl_hazard_result = mux2 i.decode_valid i.result ctrl_hazard_result_r in
   
      let wb_hazard decode_rf_adr = 
        R.reg ~e:i.padv_decode (i.wb_rf_wb &: (i.wb_rfd_adr ==: decode_rf_adr)) 
      in
      let wb_hazard_a = wb_hazard i.decode_rfa_adr in
      let wb_hazard_b = wb_hazard i.decode_rfb_adr in
  
      let wb_hazard_result = R.reg ~e:i.padv_decode i.result in

      let rfa_ram = wire M.o.Option.operand_width in
      let rfb_ram = wire M.o.Option.operand_width in

      (* Bypassing to decode stage
        
         Since the decode stage doesn't read from the register file, we have to
         save any writes to the current read addresses in decode stage until
         fetch latch in new values.
         When fetch latch in the new values, and a writeback happens at the
         same time, we bypass that value too. *)
      let bypass decode_rf_adr fetch_rf_adr rf_ram execute_hazard ctrl_hazard wb_hazard = 
        let c0, c1 = i.fetch_rf_adr_valid, i.wb_rf_wb &: (decode_rf_adr ==: i.wb_rfd_adr) in
        let wb_to_decode_result = R.reg ~e:(c0 |: c1) i.result in
        let wb_to_decode_bypass = R.reg ~e:c0 (i.wb_rf_wb &: (i.wb_rfd_adr ==: fetch_rf_adr)) in
        let use_last_wb = rifel [ c0, gnd; c1, vdd ] in
        let execute_to_decode_bypass = i.ctrl_rf_wb &: (i.ctrl_rfd_adr ==: decode_rf_adr) in
        let ctrl_to_decode_bypass = use_last_wb |: i.wb_rf_wb &: (i.wb_rfd_adr ==: decode_rf_adr) in
        let ctrl_to_decode_result = mux2 use_last_wb wb_to_decode_result i.result in
        let decode_rf = 
          mux2 execute_to_decode_bypass i.ctrl_alu_result @@
			    mux2 ctrl_to_decode_bypass ctrl_to_decode_result @@
			    mux2 wb_to_decode_bypass wb_to_decode_result @@
			    rf_ram
        in
        let execute_rf = R.reg ~e:i.padv_decode decode_rf in
        let execute_rf = 
          mux2 execute_hazard execute_hazard_result @@
			    mux2 ctrl_hazard ctrl_hazard_result @@
			    mux2 wb_hazard wb_hazard_result @@
			    execute_rf
        in
        decode_rf, execute_rf
      in
      
      let decode_rfa, execute_rfa = bypass 
        i.decode_rfa_adr i.fetch_rfa_adr rfa_ram execute_hazard_a ctrl_hazard_a wb_hazard_a
      in
      let decode_rfb, execute_rfb = bypass 
        i.decode_rfb_adr i.fetch_rfb_adr rfb_ram execute_hazard_b ctrl_hazard_b wb_hazard_b
      in

      let spr_gpr_ack, rf_wren, rf_wradr, rf_wrdat = 
        if M.f.Option.debugunit || M.f.Option.fastcontexts || M.o.Option.rf_num_shadow_gpr>0 then
          let spr_gpr_we = (i.spr_bus_addr.[15:9] ==:. 2) &: 
            i.spr_bus_stb &: i.spr_bus_we in
          let spr_gpr_re = (i.spr_bus_addr.[15:9] ==:. 2) &: 
            i.spr_bus_stb &: (~: (i.spr_bus_we)) &: (~: (i.padv_ctrl)) in
          let spr_gpr_read_ack = R.reg ~e:vdd spr_gpr_re in
          let spr_gpr_ack = spr_gpr_we &: (~: (i.wb_rf_wb)) |:
            spr_gpr_re &: spr_gpr_read_ack in
          let rf_wren =  i.wb_rf_wb |: spr_gpr_we in
          let rf_wradr = mux2 i.wb_rf_wb i.wb_rfd_adr i.spr_bus_addr in
          let rf_wrdat = mux2 i.wb_rf_wb i.result i.spr_bus_dat in
          (* XXX: todo - Zero-pad unused parts of vector
             .... seems to be related to rf_adr_width_ext, but unused at the moment *)
          spr_gpr_ack, rf_wren, rf_wradr, rf_wrdat
        else
          vdd, i.wb_rf_wb, i.wb_rfd_adr, i.result
      in

      let rfa_rdad = i.fetch_rfa_adr in
      let rfb_rdad = i.fetch_rfb_adr in
      let rfa_rden = i.fetch_rf_adr_valid in
      let rfb_rden = i.fetch_rf_adr_valid in

      let module Rf_ram = Ram.Simple_dp(struct
        let addr_width = M.o.Option.rf_addr_width
        let data_width = M.o.Option.operand_width
      end)(M) in

      let rfa = Rf_ram.ram_inst ~enable_bypass:false 
        Rf_ram.I.{
          clk = i.clk;
          raddr = rfa_rdad;
          re = rfa_rden;
          waddr = rf_wradr;
          we = rf_wren;
          din = rf_wrdat;
        }
      in
      let _ = rfa_ram <== rfa.Rf_ram.O.dout in

      let rfb = Rf_ram.ram_inst ~enable_bypass:false 
        Rf_ram.I.{
          clk = i.clk;
          raddr = rfb_rdad;
          re = rfb_rden;
          waddr = rf_wradr;
          we = rf_wren;
          din = rf_wrdat;
        }
      in
      let _ = rfb_ram <== rfb.Rf_ram.O.dout in

      let spr_gpr_dat = 
        if M.f.Option.debugunit || M.f.Option.fastcontexts || M.o.Option.rf_num_shadow_gpr>0 then
          let rfspr = Rf_ram.ram_inst ~enable_bypass:false 
            Rf_ram.I.{
              clk = i.clk;
              raddr = i.spr_bus_addr.[M.o.Option.rf_addr_width-1:0];
              re = vdd;
              waddr = rf_wradr;
              we = rf_wren;
              din = rf_wrdat;
            }
          in
          rfspr.Rf_ram.O.dout
        else
          zero M.o.Option.operand_width
      in
      O.{
        spr_gpr_ack;
        spr_gpr_dat;
        decode_rfa;
        decode_rfb;
        execute_rfa;
        execute_rfb;
      }

    module Inst = M.Inst(I)(O)
    let rf_inst = Inst.inst "rf" rf

  end

end



