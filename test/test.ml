(* instantiate each submodule to make sure they work *)

(*

  Map of mor1kx verilog to ocaml modules 

  mor1kx_branch_prediction.v          -> Branch_prediction
  mor1kx_bus_if_avalon.v              -> Avalon
  mor1kx_bus_if_wb32.v                -> Wishbone
  mor1kx_cache_lru.v
  mor1kx_cfgrs.v                      -> Cfgrs
  mor1kx_cpu_cappuccino.v
  mor1kx_cpu_espresso.v
  mor1kx_cpu_prontoespresso.v
  mor1kx_cpu.v
  mor1kx_ctrl_cappuccino.v
  mor1kx_ctrl_espresso.v
  mor1kx_ctrl_prontoespresso.v
  mor1kx_dcache.v
  mor1kx_decode_execute_cappuccino.v
  mor1kx_decode.v                     -> Decode
  mor1kx-defines.v                    -> Defines
  mor1kx_dmmu.v
  mor1kx_execute_alu.v                -> Execute_alu
  mor1kx_execute_ctrl_cappuccino.v
  mor1kx_fetch_cappuccino.v
  mor1kx_fetch_espresso.v
  mor1kx_fetch_prontoespresso.v
  mor1kx_fetch_tcm_prontoespresso.v
  mor1kx_icache.v
  mor1kx_immu.v
  mor1kx_lsu_cappuccino.v
  mor1kx_lsu_espresso.v               -> Lsu.Espresso
  mor1kx_pic.v                        -> Pic
  mor1kx_rf_cappuccino.v
  mor1kx_rf_espresso.v
  mor1kx_simple_dpram_sclk.v          -> Ram
  mor1kx-sprs.v                       -> Spr
  mor1kx_store_buffer.v
  mor1kx_ticktimer.v                  -> Ticktimer
  mor1kx_true_dpram_sclk.v            -> Ram
  mor1kx_utils.vh
  mor1kx.v
  mor1kx_wb_mux_cappuccino.v          -> Wb_mux.Cappuccino
  mor1kx_wb_mux_espresso.v            -> Wb_mux.Espresso

*)

open HardCaml
open Signal

module Test_Branch_prediction = struct
  module X = Branch_prediction
  module G = Interface.Gen(Comb)(X.I)(X.O)
  let circ,_,_,_ = G.make "branch_prediction" X.branch_prediction
  let () = Rtl.Verilog.write print_string circ
end

module Test_Cfgrs = struct
  module X = Cfgrs
  module G = Interface.Gen(Comb)(Interface.Empty)(X.O)
  let circ,_,_,_ = G.make "cfgrs" (fun _ -> X.cfgrs Option.default_options Option.default_features)
  let () = Rtl.Verilog.write print_string circ
end

module Test_Avalon = struct
  module X = Avalon
  module G = Interface.Gen(Comb)(X.I)(X.O)
  let circ,_,_,_ = G.make "avalon" (X.avalon ~burst_len:16)
  let () = Rtl.Verilog.write print_string circ
end

module Test_Wishbone = struct
  module X = Wishbone
  module G = Interface.Gen(Comb)(X.I)(X.O)
  let circ,_,_,_ = G.make "avalon_classic" (X.wishbone ~bus_type:X.Classic ~burst_len:16)
  let () = Rtl.Verilog.write print_string circ
  let circ,_,_,_ = G.make "avalon_reg" (X.wishbone ~bus_type:X.B3_registered_feedback ~burst_len:16)
  let () = Rtl.Verilog.write print_string circ
  let circ,_,_,_ = G.make "avalon_read" (X.wishbone ~bus_type:X.B3_read_bursting ~burst_len:16)
  let () = Rtl.Verilog.write print_string circ
end

module Test_Decode = struct
  module X = Decode
  module G = Interface.Gen(Comb)(X.I)(X.O)
  let circ,_,_,_ = G.make "decode" (X.decode Option.default_options Option.default_features)
  let () = Rtl.Verilog.write print_string circ
end

module Test_Execute_alu = struct
  module X = Execute_alu
  module G = Interface.Gen(Comb)(X.I)(X.O)
  let circ,_,_,_ = G.make "exec_alu" 
    (X.execute_alu ~calculate_branch_dest:true Option.default_options Option.default_features)
  let () = Rtl.Verilog.write print_string circ
end

module Test_wb_mux_cappuccino = struct
  module X = Wb_mux.Cappuccino
  module G = Interface.Gen(Comb)(X.I)(X.O)
  let circ,_,_,_ = G.make "wb_mux_cappuccino" (X.wb_mux)
  let () = Rtl.Verilog.write print_string circ
end

module Test_wb_mux_espresso = struct
  module X = Wb_mux.Espresso
  module G = Interface.Gen(Comb)(X.I)(X.O)
  let circ,_,_,_ = G.make "wb_mux_espresso" (X.wb_mux)
  let () = Rtl.Verilog.write print_string circ
end

module Test_Ticktimer = struct
  module X = Ticktimer
  module G = Interface.Gen(Comb)(X.I)(X.O)
  let circ,_,_,_ = G.make "ticktimer" X.ticktimer
  let () = Rtl.Verilog.write print_string circ
end

module Test_Pic = struct
  module X = Pic
  module G = Interface.Gen(Comb)(X.I)(X.O)
  let circ,_,_,_ = G.make "pic" (X.pic Option.default_options)
  let () = Rtl.Verilog.write print_string circ
end

module Test_Lsu_espresso = struct
  module X = Lsu.Espresso
  module G = Interface.Gen(Comb)(X.I)(X.O)
  let circ,_,_,_ = G.make "lsu_espresso" (X.lsu ~registered_io:false)
  let () = Rtl.Verilog.write print_string circ
end

module Test_Dmmu = struct
  module X = Dmmu
  module G = Interface.Gen(Comb)(X.I)(X.O)
  let circ,_,_,_ = G.make "dmmu" (X.dmmu Option.default_options Option.default_features)
  let () = Rtl.Verilog.write print_string circ
end

module Test_Immu = struct
  module X = Immu
  module G = Interface.Gen(Comb)(X.I)(X.O)
  let circ,_,_,_ = G.make "immu" (X.immu Option.default_options Option.default_features)
  let () = Rtl.Verilog.write print_string circ
end

module Test_Cache_lru = struct
  module X = Cache_lru
  module G = Interface.Gen(Comb)(X.I)(X.O)
  let circ,_,_,_ = G.make "cache_lru" (X.cache_lru)
  let () = Rtl.Verilog.write print_string circ
end


