(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module Make(M : Utils.Module_cfg_signal) : sig

  module I : interface
    clk rst cpu_adr cpu_dat_i cpu_req cpu_bsel
    cpu_we cpu_burst avm_readdata avm_waitrequest
    avm_readdatavalid
  end

  module O : interface
    cpu_err cpu_ack cpu_dat_o avm_address
    avm_byteenable avm_read avm_burstcount
    avm_write avm_writedata
  end

  val avalon : burst_len:int -> M.Bits.t I.t -> M.Bits.t O.t
  val avalon_inst : burst_len:int -> M.Bits.t I.t -> M.Bits.t O.t

end

