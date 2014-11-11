(* psuedo top level *)

let db = HardCaml.Circuit.Hierarchy.empty ()
module M = Utils.Module_hier(struct let db = db end)

open M.Bits

module I = interface i[1024] end
module O = interface o[1] end

let top i = 

  (* just enough to include all the sub-circuits *)
  let ia = Array.init 1024 (fun j -> if j=0 then empty else select i.I.i (j-1) 0) in
  let outp x = lsb (List.hd x) in
  let inp (_,b) = ia.(b) in

  let o = gnd in

  let module X = Branch_prediction.Make(M) in
  let o = o |: outp X.O.(to_list (X.branch_prediction_inst (X.I.(map inp t)))) in

  let module X = Cfgrs.Make(M) in
  let o = o |: outp X.O.(to_list (X.cfgrs_inst (X.I.(map inp t)))) in

  let module X = Avalon.Make(M) in
  let o = o |: outp X.O.(to_list (X.avalon_inst ~burst_len:16 (X.I.(map inp t)))) in

  let module X = Wishbone.Make(M) in
  let o = o |: outp X.O.(to_list (X.wishbone_inst ~bus_type:X.Classic ~burst_len:16 (X.I.(map inp t)))) in

  let module X = Decode.Make(M) in
  let o = o |: outp X.O.(to_list (X.decode_inst (X.I.(map inp t)))) in

  let module X = Execute_alu.Make(M) in
  let o = o |: outp X.O.(to_list (X.execute_alu_inst ~calculate_branch_dest:false (X.I.(map inp t)))) in

  let module X = Wb_mux.Cappuccino.Make(M) in
  let o = o |: outp X.O.(to_list (X.wb_mux_inst (X.I.(map inp t)))) in

  let module X = Wb_mux.Espresso.Make(M) in
  let o = o |: outp X.O.(to_list (X.wb_mux_inst (X.I.(map inp t)))) in

  let module X = Ticktimer.Make(M) in
  let o = o |: outp X.O.(to_list (X.ticktimer_inst (X.I.(map inp t)))) in

  let module X = Pic.Make(M) in
  let o = o |: outp X.O.(to_list (X.pic_inst (X.I.(map inp t)))) in

  let module X = Lsu.Espresso.Make(M) in
  let o = o |: outp X.O.(to_list (X.lsu_inst ~registered_io:false (X.I.(map inp t)))) in

  let module X = Dmmu.Make(M) in
  let o = o |: outp X.O.(to_list (X.dmmu_inst (X.I.(map inp t)))) in

  let module X = Immu.Make(M) in
  let o = o |: outp X.O.(to_list (X.immu_inst (X.I.(map inp t)))) in

  let module X = Dcache.Make(M) in
  let o = o |: outp X.O.(to_list (X.dcache_inst (X.I.(map inp t)))) in

  let module X = Icache.Make(M) in
  let o = o |: outp X.O.(to_list (X.icache_inst (X.I.(map inp t)))) in

  let module X = Store_buffer.Make(M) in
  let o = o |: outp X.O.(to_list (X.store_buffer_inst ~depth_width:4 (X.I.(map inp t)))) in

  let module X = Rf.Espresso.Make(M) in
  let o = o |: outp X.O.(to_list (X.rf_inst (X.I.(map inp t)))) in

  O.{ o }


module G = HardCaml.Interface.Circ(I)(O)
let circ = G.make "top" top
let () = HardCaml.Rtl.(Hierarchy.write db "./" (fun _ c -> Verilog.write print_string c) circ)


