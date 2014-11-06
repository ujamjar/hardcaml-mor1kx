type bus_type = 
  | Classic
  | B3_registered_feedback
  | B3_read_bursting

module I : interface
    clk rst
    cpu_adr cpu_dat_i cpu_req cpu_bsel cpu_we cpu_burst
    wbm_err wbm_ack wbm_dat_i wbm_rty
end

module O : interface
    cpu_err cpu_ack cpu_dat_o
    wbm_adr wbm_stb wbm_cyc wbm_sel
    wbm_we wbm_cti wbm_bte wbm_dat_o
    redundant  
end

val wishbone : bus_type:bus_type -> burst_len:int -> 
  HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t 

