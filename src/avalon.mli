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

val avalon : burst_len:int -> HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t

