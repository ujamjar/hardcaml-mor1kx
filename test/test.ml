(* instantiate each submodule to make sure they work *)

(*

  Map of mor1kx verilog to ocaml modules 

  mor1kx-defines.v                    -> Defines
  mor1kx_utils.vh                     -> Utils (etc)
  mor1kx-sprs.v                       -> Spr
  mor1kx_branch_prediction.v          -> Branch_prediction
  mor1kx_bus_if_avalon.v              -> Avalon
  mor1kx_bus_if_wb32.v                -> Wishbone
  mor1kx_cache_lru.v                  -> Cache_lru
  mor1kx_cfgrs.v                      -> Cfgrs
  mor1kx_cpu_cappuccino.v
  mor1kx_cpu_espresso.v
  mor1kx_cpu_prontoespresso.v
  mor1kx_cpu.v
  mor1kx_ctrl_cappuccino.v
  mor1kx_ctrl_espresso.v
  mor1kx_ctrl_prontoespresso.v
  mor1kx_dcache.v                     -> Dcache
  mor1kx_decode_execute_cappuccino.v
  mor1kx_decode.v                     -> Decode
  mor1kx_dmmu.v                       -> Dmmu
  mor1kx_execute_alu.v                -> Execute_alu
  mor1kx_execute_ctrl_cappuccino.v    -> Execute_ctrl_cappuccino
  mor1kx_fetch_cappuccino.v           -> Fetch.Cappuccino
  mor1kx_fetch_espresso.v             -> Fetch.Espresso
  mor1kx_fetch_prontoespresso.v       -> Fetch.Pronto_espresso
  mor1kx_fetch_tcm_prontoespresso.v   -> Fetch.Tcm_prontoespress
  mor1kx_icache.v                     -> Icache
  mor1kx_immu.v                       -> Immu
  mor1kx_lsu_cappuccino.v             -> Lsu.Cappuccino
  mor1kx_lsu_espresso.v               -> Lsu.Espresso
  mor1kx_pic.v                        -> Pic
  mor1kx_rf_cappuccino.v              -> rf.Cappuccino
  mor1kx_rf_espresso.v                -> Rf.Espresso
  mor1kx_simple_dpram_sclk.v          -> Ram
  mor1kx_store_buffer.v               -> Store_buffer
  mor1kx_ticktimer.v                  -> Ticktimer
  mor1kx_true_dpram_sclk.v            -> Ram
  mor1kx.v
  mor1kx_wb_mux_cappuccino.v          -> Wb_mux.Cappuccino
  mor1kx_wb_mux_espresso.v            -> Wb_mux.Espresso

*)

open HardCaml.Signal

module Test_Branch_prediction = struct
  module M = Utils.Module_flat
  module X = Branch_prediction.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "branch_prediction"
  let circ = G.make name X.branch_prediction
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Cfgrs = struct
  module M = Utils.Module_flat
  module X = Cfgrs.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "cfgrs"
  let circ = G.make name X.cfgrs
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Avalon = struct
  module M = Utils.Module_flat
  module X = Avalon.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "avalon"
  let circ = G.make name (X.avalon ~burst_len:16)
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Wishbone = struct
  module M = Utils.Module_flat
  module X = Wishbone.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let circ = G.make "wb_classic" (X.wishbone ~bus_type:X.Classic ~burst_len:16)
  let () = HardCaml.Rtl.Verilog.write print_string circ
  let circ = G.make "wb_reg" (X.wishbone ~bus_type:X.B3_registered_feedback ~burst_len:16)
  let () = HardCaml.Rtl.Verilog.write print_string circ
  let circ = G.make "wb_read" (X.wishbone ~bus_type:X.B3_read_bursting ~burst_len:16)
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Decode = struct
  module M = Utils.Module_flat
  module X = Decode.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "decode"
  let circ = G.make name X.decode
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Execute_alu = struct
  module M = Utils.Module_flat
  module X = Execute_alu.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "execute_alu"
  let circ = G.make name (X.execute_alu ~calculate_branch_dest:false)
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Wb_mux_Cappuccino = struct
  module M = Utils.Module_flat
  module X = Wb_mux.Cappuccino.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "wb_mux"
  let circ = G.make name X.wb_mux
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Wb_mux_Espresso = struct
  module M = Utils.Module_flat
  module X = Wb_mux.Espresso.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "wb_mux"
  let circ = G.make name X.wb_mux
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Ticktimer = struct
  module M = Utils.Module_flat
  module X = Ticktimer.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "ticktimer"
  let circ = G.make name X.ticktimer
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Pic = struct
  module M = Utils.Module_flat
  module X = Pic.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "pic"
  let circ = G.make name X.pic
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Lsu_espresso = struct
  module M = Utils.Module_flat
  module X = Lsu.Espresso.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "lsu_espresso"
  let circ = G.make name (X.lsu ~registered_io:false)
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Lsu_cappuccino = struct
  module M = Utils.Module_flat
  module X = Lsu.Cappuccino.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "lsu_cappuccino"
  let circ = G.make name (X.lsu)
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Dmmu = struct
  module M = Utils.Module_flat
  module X = Dmmu.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "dmmu"
  let circ = G.make name X.dmmu
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Immu = struct
  module M = Utils.Module_flat
  module X = Immu.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "immu"
  let circ = G.make name X.immu
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Cache_lru = struct
  module M = Utils.Module_flat
  module X = Cache_lru.Make(struct let numways=4 end)(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "cache_lru"
  let circ = G.make name X.cache_lru
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Dcache = struct
  module M = Utils.Module_flat
  module X = Dcache.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "dcache"
  let circ = G.make name X.dcache
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Icache = struct
  module M = Utils.Module_flat
  module X = Icache.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "icache"
  let circ = G.make name X.icache
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Store_buffer = struct
  module M = Utils.Module_flat
  module X = Store_buffer.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "store_buffer"
  let circ = G.make name (X.store_buffer ~depth_width:4)
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Rf_Espresso = struct
  module M = Utils.Module_flat
  module X = Rf.Espresso.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "rf_espresso"
  let circ = G.make name X.rf
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Rf_Cappuccino = struct
  module M = Utils.Module_flat
  module X = Rf.Cappuccino.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "rf_cappuccino"
  let circ = G.make name X.rf
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Fetch_Tcm_pronto_espresso= struct
  module M = Utils.Module_flat
  module X = Fetch.Tcm_pronto_espresso.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "fetch_tcm_pronto_espresso"
  let circ = G.make name X.fetch
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Fetch_Pronto_espresso= struct
  module M = Utils.Module_flat
  module X = Fetch.Pronto_espresso.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "fetch_pronto_espresso"
  let circ = G.make name X.fetch
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Fetch_Espresso = struct
  module M = Utils.Module_flat
  module X = Fetch.Espresso.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "fetch_espresso"
  let circ = G.make name X.fetch
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Fetch_Cappuccino = struct
  module M = Utils.Module_flat
  module X = Fetch.Cappuccino.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "fetch_cappuccino"
  let circ = G.make name X.fetch
  let () = HardCaml.Rtl.Verilog.write print_string circ
end

module Test_Execute_ctrl_cappuccino = struct
  module M = Utils.Module_flat
  module X = Execute_ctrl_cappuccino.Make(M)
  module G = HardCaml.Interface.Circ(X.I)(X.O)
  let name = "execute_ctrl_cappuccino"
  let circ = G.make name X.execute_ctrl_cappuccino
  let () = HardCaml.Rtl.Verilog.write print_string circ
end



