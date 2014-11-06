(* instantiate each submodule to make sure they work *)

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



