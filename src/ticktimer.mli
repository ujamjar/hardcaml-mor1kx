module I : interface
   clk rst
   spr_access spr_we spr_addr spr_dat_i
end

module O : interface
   spr_ttmr spr_ttcr spr_bus_ack spr_dat_o
end

val ticktimer : HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t

