module I : interface
  clk
  rst
  irq
  spr_access
  spr_we
  spr_addr
  spr_dat_i
end

module O : interface
  spr_picmr
  spr_picsr
  spr_bus_ack
  spr_dat_o
end

val pic : Option.options -> HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t

