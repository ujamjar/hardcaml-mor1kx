module I : interface
  clk
  rst
  pc_i
  adr_i
  dat_i
  bsel_i
  atomic_i
  write
  read
end

module O : interface
  pc_o
  adr_o
  dat_o
  bsel_o
  atomic_o
  full
  empty
end

val store_buffer : depth_width:int -> HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t

