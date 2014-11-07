
(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Description: mor1kx processor avalon bus bridge

  Copyright (C) 2013 Stefan Kristiansson <stefan.kristiansson@saunalahti.fi>

***************************************************************************** *)

open HardCaml.Signal.Comb
open Utils

module I = interface
   clk[1]
   rst[1]
   cpu_adr[32]
   cpu_dat_i[32]
   cpu_req[1]
   cpu_bsel[4]
   cpu_we[1]
   cpu_burst[1]
   avm_readdata[32]
   avm_waitrequest[1]
   avm_readdatavalid[1]
end

module O = interface
   cpu_err[1]
   cpu_ack[1]
   cpu_dat_o[32]
   avm_address[32]
   avm_byteenable[4]
   avm_read[1]
   avm_burstcount[4]
   avm_write[1]
   avm_writedata[32]
end

type states = Idle | Read | Burst | Write deriving(Enum,Bounded)

let avalon ~burst_len i = 
  let open I in
  let open HardCaml.Signal.Guarded in
  let module R = Regs(struct let clk = i.clk let rst = i.rst end) in

  let state_is, sm, next = R.statemachine vdd 
    (Enum_states.enum_from_to Bounded_states.min_bound Bounded_states.max_bound)
  in

  let () = compile [

    sm [

      Idle, [
        g_when (i.cpu_req &: (~: (i.avm_waitrequest))) [
          g_if i.cpu_we [ next Write; ] @@ 
          g_elif i.cpu_burst [ next Burst; ] 
          [ next Read; ];
        ];
      ];
      Read, [ g_when i.avm_readdatavalid [ next Idle; ]; ];
      Burst, [ g_when (~: (i.cpu_burst) &: i.avm_readdatavalid) [ next Idle; ] ];
      Write, [ next Idle; ];

    ];

  ] in

  O.({
    cpu_err = gnd;
    cpu_ack = i.avm_readdatavalid |: state_is Write;
    cpu_dat_o = i.avm_readdata;
    avm_address = i.cpu_adr;
    avm_byteenable = i.cpu_bsel;
    avm_read = i.cpu_req &: (~: (i.cpu_we)) &: state_is Idle;
    avm_burstcount = 
      mux2 (i.cpu_burst &: ~: (state_is Burst)) 
        (consti 4 burst_len) (consti 4 1);
    avm_write = i.cpu_req &: i.cpu_we &: state_is Idle;
    avm_writedata = i.cpu_dat_i;
  })


