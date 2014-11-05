open HardCaml.Signal.Comb

type bus_type = 
  | Classic
  | B3_registered_feedback
  | B3_read_bursting

module I = interface
    clk[1] rst[1]
    cpu_adr[32] cpu_dat_i[32] cpu_req[1] cpu_bsel[4] cpu_we[1] cpu_burst[1]
    wbm_err[1] wbm_ack[1] wbm_dat_i[32] wbm_rty[1]
end

module O = interface
    cpu_err[1] cpu_ack[1] cpu_dat_o[32]
    wbm_adr[32] wbm_stb[1] wbm_cyc[1] wbm_sel[4]
    wbm_we[1] wbm_cti[3] wbm_bte[2] wbm_dat_o[32]
    undriven[1]
end

let classic i = 
  let open I in
  let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in
  let cycle_end = R.reg ~cv:vdd ~e:vdd (i.wbm_ack |: i.wbm_err) in
  O.({
    cpu_err = i.wbm_err;
    cpu_ack = i.wbm_ack;
    cpu_dat_o = i.wbm_dat_i;
    wbm_adr = i.cpu_adr;
    wbm_stb = i.cpu_req &: (~: cycle_end);
    wbm_cyc = i.cpu_req;
    wbm_sel = i.cpu_bsel;
    wbm_we = i.cpu_we;
    wbm_cti = gnd;
    wbm_bte = gnd;
    wbm_dat_o = i.cpu_dat_i;
    undriven = i.wbm_rty |: lsb i.cpu_burst;
  })

let b3_registered_feedback ~burst_len i = 
  let open I in
  O.({
    wbm_adr = i.cpu_adr;
    wbm_stb = i.cpu_req;
    wbm_cyc = i.cpu_req;
    wbm_sel = i.cpu_bsel;
    wbm_we = i.cpu_we;
    wbm_cti = mux2 i.cpu_burst (consti 3 0b010) (consti 3 0b111);
    wbm_bte = consti 2
      (match burst_len with
      | 4  -> 0b01 
      | 8  -> 0b10 
      | 16 -> 0b11 
      | _  -> 0b00); (* Linear burst *)
    wbm_dat_o = i.cpu_dat_i;
    cpu_err = i.wbm_err;
    cpu_ack = i.wbm_ack;
    cpu_dat_o = i.wbm_dat_i;
    undriven = i.wbm_rty |: i.rst |: i.clk;
  })

let b3_read_bursting ~burst_len i = 
  let open I in
  let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in

  let baddr_width = 
    match burst_len with
    | 4 -> 2
    | 8 -> 3
    | 16 -> 4
    | _ -> 30
  in

  let finish_burst = wire 1 in

  let bursting = R.reg_fb ~e:vdd ~w:1 
    (fun bursting ->
      mux2 i.wbm_err gnd 
        (mux2 (bursting &: finish_burst &: i.wbm_ack) gnd
          (mux2 (i.cpu_req &: (~: bursting) &: (~: (i.cpu_we))) vdd bursting)))
  in

  let cpu_adr = Utils.drop_bottom i.cpu_adr 2 in
  let burst_address = 
    let incr x = Utils.insert x (Utils.sel_bottom x baddr_width +:. 1) 0 in
    R.reg_fb ~e:vdd ~w:30 
      (fun burst_address ->
        mux2 (i.cpu_req &: (~: bursting)) cpu_adr 
          (mux2 i.wbm_ack (incr burst_address) burst_address))
  in
  let burst_wrap_start = 
    R.reg ~e:(i.cpu_req &: (~: bursting)) (Utils.sel_bottom cpu_adr baddr_width)
  in
  let finish_burst_r = R.reg ~e:vdd (mux2 i.wbm_ack finish_burst gnd) in

  let burst_wrap_finish = burst_wrap_start -:. 1 in

  let address_differs = burst_address <>: cpu_adr in

  let () = finish_burst <== 
    (bursting &:
      (if burst_len=0 then gnd 
       else (Utils.sel_bottom burst_address baddr_width ==: burst_wrap_finish) |:
       address_differs |:
       (~: (i.cpu_req))))
  in
  O.({
    wbm_adr = mux2 bursting (burst_address @: zero 2) i.cpu_adr;
    wbm_stb = bursting &: ~: finish_burst_r;
    wbm_cyc = bursting &: ~: finish_burst_r;
    wbm_sel = i.cpu_bsel;
    wbm_we = i.cpu_we;
    wbm_cti = mux2 bursting (mux2 finish_burst (consti 3 0b111) 
                                               (consti 3 0b010)) 
                            (consti 3 0b000);
    wbm_bte = consti 2
      (match burst_len with
      | 4  -> 0b01 
      | 8  -> 0b10 
      | 16 -> 0b11 
      | _  -> 0b00); (* Linear burst *)
    wbm_dat_o = i.cpu_dat_i;
    cpu_err = i.wbm_err;
    cpu_ack = i.wbm_ack &: (~: (bursting &: address_differs)) &: i.cpu_req;
    cpu_dat_o = mux2 i.wbm_err (zero 32) i.wbm_dat_i;
    undriven = i.wbm_rty |: lsb i.cpu_burst;
  })

let wishbone ~bus_type ~burst_len i = 
  match bus_type with
  | Classic -> classic i
  | B3_registered_feedback -> b3_registered_feedback ~burst_len i
  | B3_read_bursting -> b3_read_bursting ~burst_len i


