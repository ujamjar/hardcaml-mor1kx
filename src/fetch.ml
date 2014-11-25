open Option
open Utils

module Tcm_pronto_espresso = struct

  (* ****************************************************************************
    This Source Code Form is subject to the terms of the
    Open Hardware Description License, v. 1.0. If a copy
    of the OHDL was not distributed with this file, You
    can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

    Description: mor1kx pronto espresso fetch unit for TCM memories

    This is the fetch unit for the pipeline, so begins at the reset address and,
    in lock-step with the pipeline, provides the instructions according to the
    program flow.

    It is designed to interface to a single-cycle memory system (usually referred
    to as a tightly-coupled memory, or TCM - a ROM or a RAM). As such, it
    attempts to maximise throughput of the fetch stage to the pipeline.

    It responds to branch/exception indications from the control stage and
    delivers the appropriate instructions to the decode stage when the pipeline
    is ready to advance.

    It will go to "sleep" when it hits a jump-to-self instruction (0x00000000).

    Assumptions:
    Relies _heavily_ on being attached to a single-cycle memory.
    The ibus_adr_o requests byte-addresses, ibus_req_o is analogous to a
    read-enable signal, and the ibus_ack_i should be aligned with the read-data
    coming back.

    indicate ibus errors

    Copyright (C) 2013 Authors

    Author(s): Julius Baxter <juliusbaxter@gmail.com>
               Andy Ray <andy.ray@ujamjar.com>

  ***************************************************************************** *)

  module Make(M : Utils.Module_cfg_signal) = struct
    
    open M.Bits

    module I = interface
      clk[1]
      rst[1] 
      ibus_err[1] 
      ibus_ack[1] 
      ibus_dat[Defines.insn_width] 
      padv[1]
      branch_occur[1] 
      branch_dest[M.o.operand_width] 
      du_restart[1] 
      du_restart_pc[M.o.operand_width]
      fetch_take_exception_branch[1] 
      execute_waiting[1] 
      du_stall[1]
      stepping[1] 
      flag[1] 
      flag_clear[1] 
      flag_set[1]
    end

    module O = interface
      ibus_adr[1] 
      ibus_req[1] 
      decode_insn[Defines.insn_width] 
      fetched_pc[M.o.operand_width] 
      fetch_ready[1]
      fetch_rfa_adr[M.o.rf_addr_width] 
      fetch_rfb_adr[M.o.rf_addr_width] 
      fetch_rf_re[1] 
      pc_fetch_next[M.o.operand_width]
      decode_except_ibus_err[1] 
      fetch_sleep[1]
    end

    let fetch i = 
      let open I in
      let open HardCaml.Signal.Guarded in
      let module R = Regs(struct let clk = i.clk let rst = i.rst end) in
    
      let nop = consti Defines.Opcode.width Defines.Opcode.nop @: zero 26 in
      let reset_pc = consti M.o.operand_width M.o.reset_pc in
      let current_bus_pc = R.g_reg ~cv:reset_pc ~rv:reset_pc ~e:vdd M.o.operand_width in
      let just_took_branch_addr = R.g_reg ~e:vdd 1 in
      let addr_pipelined = R.g_reg ~e:vdd 1 in
      let bus_req = R.g_reg ~e:vdd 1 in
      let insn_buffered = R.g_reg ~e:vdd 1 in
      let next_insn_will_branch = g_wire gnd in
      let fetched_pc = R.g_reg ~e:vdd M.o.operand_width in
      let decode_insn = R.g_reg ~rv:nop ~cv:nop ~e:vdd Defines.insn_width in
      let jump_insn_in_decode = R.g_reg ~e:vdd 1 in

      let execute_waiting_deasserted, execute_waiting_asserted = 
        let r = R.reg ~e:vdd i.execute_waiting in
        ((~: (i.execute_waiting)) &:     r ),
        (     i.execute_waiting   &: (~: r))
      in
      let execute_waiting_asserted_r = R.reg ~e:vdd execute_waiting_asserted in
      let execute_waited_single_cycle = execute_waiting_asserted_r &: (~: (i.execute_waiting)) in

      let fetch_take_exception_branch_r = R.reg ~e:vdd i.fetch_take_exception_branch in
      let just_waited_single_cycle = R.reg ~e:vdd execute_waited_single_cycle in
      let just_waited_single_cycle_r = R.reg ~e:vdd just_waited_single_cycle in

      let padv_r = R.reg_fb ~e:vdd ~w:4 (fun padv_r -> lsbs padv_r @: i.padv) in

      let long_stall = ((padv_r @: i.padv) ==:. 0b10000) &: i.execute_waiting in

      let next_bus_pc = current_bus_pc#q +:. 4 in
      let ibus_adr = mux2 addr_pipelined#q next_bus_pc current_bus_pc#q in

      let pc_fetch_next = ibus_adr in

      let ibus_req = 
        bus_req#q &: (~: (i.stepping &: i.ibus_ack)) |:
        (execute_waiting_deasserted &:
        (~: (insn_buffered#q &: next_insn_will_branch#q))) |:
        fetch_take_exception_branch_r 
      in
      let bus_req_r = R.reg ~e:vdd ibus_req in

      (* Signal rising edge on bus request signal *)
      let first_bus_req_cycle = ibus_req &: (~: bus_req_r) in

      let taking_branch_addr = (i.branch_occur &: i.padv) |: i.fetch_take_exception_branch in

      let buffered_insn_is_jump = insn_buffered#q &: next_insn_will_branch#q in

      let () = compile [
        g_if (i.du_restart) [
          current_bus_pc $== i.du_restart_pc;
          just_took_branch_addr $==. 0;
        
        ] @@ g_elif (i.fetch_take_exception_branch) [
          current_bus_pc $== i.branch_dest;
          just_took_branch_addr $==. 1;
        
        ] @@ g_elif (i.branch_occur &: i.padv) [
          current_bus_pc $== i.branch_dest;
          just_took_branch_addr $==. 1;
        
        ] @@ g_elif (i.ibus_ack &: 
                    (i.padv |: (just_waited_single_cycle_r &:
                      (~: ((padv_r.[0:0] @: i.padv) ==:. 0b00)))) &:
                    (~: execute_waited_single_cycle) &: (~: (i.stepping))) [
          current_bus_pc $== next_bus_pc;
          just_took_branch_addr $==. 0;
        
        ] @@ g_elif (execute_waiting_asserted &: i.ibus_ack &: (~: (just_took_branch_addr#q))) [
          current_bus_pc $== next_bus_pc;
        
        ] @@ g_elif (just_took_branch_addr#q) [
          just_took_branch_addr $==. 0;
        
        ] @@ g_elif (long_stall) [
          current_bus_pc $== fetched_pc#q +:. 4;
        ] []
      ] in

      let insn_from_branch_on_input = R.reg ~e:vdd just_took_branch_addr#q in
      (*let insn_from_branch_in_pipeline = R.reg ~e:vdd insn_from_branch_on_input in*)

      let decode_except_ibus_err = R.reg_fb ~e:vdd ~w:1 
        (fun d ->
          mux2 ((i.padv |: i.fetch_take_exception_branch) &: i.branch_occur |: i.du_stall) gnd @@
          mux2 bus_req#q i.ibus_err d)
      in

      let will_go_to_sleep = 
        (i.ibus_dat ==:. 0) &: i.padv &: i.ibus_ack &: ibus_req &: 
        (((~: (jump_insn_in_decode#q)) &: (~: (just_took_branch_addr#q))) |: 
          insn_from_branch_on_input)
      in
      let sleep = R.reg_fb ~e:vdd ~w:1 
        (fun d ->
          mux2 i.fetch_take_exception_branch gnd @@
          mux2 will_go_to_sleep vdd d)
      in

      let () = compile [
        g_if (i.stepping &: i.ibus_ack) [ 
          bus_req $==.0; 
        ] @@ g_elif (i.du_stall) [ 
          bus_req $==.0; 
        ] @@ g_elif (i.ibus_err |: decode_except_ibus_err) [ 
          bus_req $==.0; 
        ] @@ g_elif (sleep) [ 
          bus_req $==.0; 
        ] @@ g_elif (i.execute_waiting) [ 
          bus_req $==.0; 
        ] @@ g_elif (buffered_insn_is_jump) [ 
          bus_req $==.0; 
        ] [ 
          bus_req $==.1; 
        ]
      ] in

      let () = compile [
        g_if (i.ibus_err |: decode_except_ibus_err |: i.fetch_take_exception_branch) [
          addr_pipelined $==. 0;
        ] @@ g_elif (first_bus_req_cycle) [
          addr_pipelined $==. 1;
        ] @@ g_elif (taking_branch_addr) [
          addr_pipelined $==. 0;
        ] @@ g_elif (just_took_branch_addr#q) [
          addr_pipelined $==. 1;
        ] @@ g_elif (just_waited_single_cycle) [
          addr_pipelined $==. 1;
        ] @@ g_elif (~: (bus_req#q)) [
          addr_pipelined $==. 0;
        ] []
      ] in

      let insn_buffer = 
        R.reg ~e:(execute_waiting_asserted &: i.ibus_ack &: (~: (just_took_branch_addr#q)))
        i.ibus_dat 
      in
      let push_buffered_jump_through_pipeline = R.reg ~e:vdd
        (buffered_insn_is_jump &: execute_waiting_deasserted)
      in

      let () = compile [
        g_if (sleep |: (i.du_stall &: (~: (i.execute_waiting)))) [
          decode_insn $== nop;
        ] @@ g_elif (i.fetch_take_exception_branch &: (~: (i.du_stall))) [
          decode_insn $== nop;
        ] @@ g_elif ((i.padv |: i.stepping) &: i.ibus_ack &: (ibus_req |: i.stepping) &:
              (((~: (jump_insn_in_decode#q)) &: (~: (just_took_branch_addr#q))) |:
               (insn_from_branch_on_input)) &:
              (~: (execute_waited_single_cycle |: just_waited_single_cycle))) [
          decode_insn $== i.ibus_dat;
          fetched_pc  $== current_bus_pc#q;
        ] @@ g_elif (just_waited_single_cycle_r &: (~: (i.execute_waiting))) [
          decode_insn $== i.ibus_dat;
          fetched_pc  $== current_bus_pc#q;
        ] @@ g_elif (execute_waiting_deasserted &: insn_buffered#q) [
          decode_insn $== insn_buffer;
          fetched_pc  $== fetched_pc#q +:. 4;
        ] @@ g_elif ((jump_insn_in_decode#q |: i.branch_occur) &: i.padv) [
       (* About to jump - remove this instruction from the pipeline *)
          decode_insn $== nop;
        ] @@ g_elif (i.fetch_take_exception_branch) [
          decode_insn $== nop;
        ] @@ g_elif (push_buffered_jump_through_pipeline) [
          decode_insn $== nop;
        ] []
      ] in

      let () = compile [
        g_if (sleep) [
          jump_insn_in_decode $==. 0;
        ] @@ g_elif ((~: (jump_insn_in_decode#q)) &: next_insn_will_branch#q &: i.ibus_ack) [
          jump_insn_in_decode $==. 1;
        ] [
          jump_insn_in_decode $==. 0;
        ]
      ] in

      (* Pick out opcode of next instruction to go to decode stage *)
      let next_insn_opcode = 
          mux2 insn_buffered#q
            (sel insn_buffer Defines.Opcode.select)
            (sel i.ibus_dat Defines.Opcode.select)
      in

      let cmap = List.map (fun (a,b) -> consti Defines.Opcode.width a, b) in
      let () = compile [ (* XXX check optimization *)
        g_when ((i.ibus_ack &: (~: (just_took_branch_addr#q))) |: insn_buffered#q) [
          g_switch (next_insn_opcode) (cmap [
            Defines.Opcode.j, [
              next_insn_will_branch $==. 1;
            ];
            Defines.Opcode.jal, [
              next_insn_will_branch $==. 1;
            ];
            Defines.Opcode.jr, [
              next_insn_will_branch $==. 1;
            ];
            Defines.Opcode.jalr, [
              next_insn_will_branch $==. 1;
            ];
            Defines.Opcode.bnf, [
              next_insn_will_branch $== ((~: (i.flag |: i.flag_set)) |: i.flag_clear);
            ];
            Defines.Opcode.bf, [
              next_insn_will_branch $== ((~: ((~: (i.flag)) |: i.flag_clear)) |: i.flag_set);
            ];
            Defines.Opcode.systrapsync, [
              next_insn_will_branch $==. 1;
            ];
            Defines.Opcode.rfe, [
              next_insn_will_branch $==. 1;
            ];
          ]);
        ];
      ] in

      let () = compile [
        g_if (execute_waiting_asserted &: i.ibus_ack &: (~: (just_took_branch_addr#q))) [
          insn_buffered $==. 1;
        ] @@ g_elif (execute_waiting_deasserted) [
          insn_buffered $==. 0;
        ] @@ g_elif (i.fetch_take_exception_branch) [
          insn_buffered $==. 0;
        ] @@ g_elif (long_stall) [
          insn_buffered $==. 0;
        ] [];
      ] in

      let fetch_ready = 
        (i.ibus_ack |: insn_buffered#q) &:
        (~: (just_took_branch_addr#q)) &:
        (~: (just_waited_single_cycle)) &:
        (~: (i.du_stall)) |:
        push_buffered_jump_through_pipeline 
      in

      let fetch_rfa_adr = 
        mux2 insn_buffered#q
          (sel insn_buffer Defines.ra_select)
          (sel i.ibus_dat Defines.ra_select) 
      in
      let fetch_rfb_adr = 
        mux2 insn_buffered#q
          (sel insn_buffer Defines.rb_select)
          (sel i.ibus_dat Defines.rb_select) 
      in
      let fetch_rf_re = 
        (i.ibus_ack |: execute_waiting_deasserted) &: (i.padv |: i.stepping) 
      in
      let fetch_sleep = sleep in
      
      O.{
        ibus_adr;
        ibus_req;
        decode_insn = decode_insn#q; 
        fetched_pc = fetched_pc#q;
        fetch_ready;
        fetch_rfa_adr;
        fetch_rfb_adr; 
        fetch_rf_re;
        pc_fetch_next;
        decode_except_ibus_err;
        fetch_sleep;
      }

    module Inst = M.Inst(I)(O)
    let fetch_inst = Inst.inst "fetch" fetch
  end

end

module Pronto_espresso = struct

  (* ****************************************************************************
    This Source Code Form is subject to the terms of the
    Open Hardware Description License, v. 1.0. If a copy
    of the OHDL was not distributed with this file, You
    can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

    Description: mor1kx pronto espresso fetch unit

    Fetch insn, advance PC (or take new branch address) on padv_i.

    What we might want to do is have a 1-insn buffer here, so when the current
    insn is fetched, but the main pipeline doesn't want it yet

    indicate ibus errors

    Copyright (C) 2012 Authors

    Author(s): Julius Baxter <juliusbaxter@gmail.com>
              Andy Ray <andy.ray@ujamjar.com>

  ***************************************************************************** *)

  module Make(M : Utils.Module_cfg_signal) = struct
    open M.Bits

    module I = interface 
      clk[1] 
      rst[1]
      ibus_err[1]
      ibus_ack[1]
      ibus_dat[Defines.insn_width]
      ic_enable
      padv[1]
      branch_occur[1]
      branch_dest[M.o.operand_width]
      ctrl_insn_done[1]
      du_restart[1]
      du_restart_pc[M.o.operand_width]
      fetch_take_exception_branch[1]
      execute_waiting[1]
      du_stall[1]
      stepping[1]
      flag[1]
      flag_clear[1] 
      flag_set[1]
      spr_bus_addr[16]
      spr_bus_we[1]
      spr_bus_stb[1]
      spr_bus_dat[M.o.operand_width]
    end

    module O = interface 
      ibus_adr[M.o.operand_width]
      ibus_req[1]
      ibus_burst[1]
      decode_insn[Defines.insn_width]
      fetched_pc[M.o.operand_width]
      fetch_ready[1]
      fetch_rfa_adr[M.o.rf_addr_width]
      fetch_rfb_adr[M.o.rf_addr_width]
      fetch_rf_re[1]
      pc_fetch_next[M.o.operand_width]
      decode_except_ibus_err[1]
      fetch_sleep[1]
      fetch_quick_branch[1]
      spr_bus_dat_ic[M.o.operand_width]
      spr_bus_ack_ic[1]
    end

    type mini_cache_ctl = 
      {
        tag : M.Bits.t;
        valid : M.Bits.t;
        wr : M.Bits.t;
      }

    module Mc_o = interface
      insn[Defines.insn_width]
      hit[1]
      hit_ungated[1]
      fetch_quick_branch[1]
    end

    let mk_mini_cache ~i ~pc ~took_branch ~will_go_to_sleep = 
      let open I in
      let module Spr = Spr.Make(M.Bits) in
      let module R = Regs(struct let clk = i.clk let rst = i.rst end) in
      let number_mini_cache_words = 1 lsl M.o.icache_block_width in
      let mini_cache_tag_end = M.o.icache_block_width+2 in

      let pc_word_sel = pc.[M.o.icache_block_width+1:2] in
      let pc_word_sel_1h = binary_to_onehot pc_word_sel in

      let pc_tag = pc.[M.o.operand_width-1:mini_cache_tag_end] in

      let mini_cache_fill_condition = i.ibus_ack &: (~: (i.ibus_err)) &: (~: (will_go_to_sleep)) in

      let invalidate = 
        i.spr_bus_stb &: i.spr_bus_we &: 
        (i.spr_bus_addr ==: Spr.Ic.(const icbir)) 
      in

      let mini_cache_v = Array.init number_mini_cache_words (fun j ->
        let c0 = invalidate(* | !ic_enable*) |: i.du_stall in
        let c1 = mini_cache_fill_condition &: pc_word_sel_1h.[j:j] in
        {
          tag = R.reg_fb ~e:vdd ~w:(width pc_tag) 
            (fun d -> (pmux [ c0, d; c1, pc_tag ] d));
          valid = R.reg_fb ~e:vdd ~w:1 
            (pmux [ c0, gnd; c1, vdd; ]);
          wr = (~: c0) &: c1;
        }
      ) in

      let mini_cache = Array.init number_mini_cache_words 
        (fun j -> R.reg ~e:mini_cache_v.(j).wr i.ibus_dat) 
      in

      let insn = mux pc_word_sel (Array.to_list mini_cache) in

      let cmux f = Array.map f mini_cache_v |> Array.to_list |> mux pc_word_sel in
      let hit_ungated = 
        cmux (fun x -> x.valid) &: (cmux (fun x -> x.tag) ==: pc_tag)
      in

      let hit = hit_ungated &: (~: took_branch) &:
        (~: (i.fetch_take_exception_branch)) in

      let fetch_quick_branch = took_branch &: hit in
      
      Mc_o.{
        insn;
        hit;
        hit_ungated;
        fetch_quick_branch;
      }

    let fetch i = 
      let open I in
      let open HardCaml.Signal.Guarded in
      let module R = Regs(struct let clk = i.clk let rst = i.rst end) in
      
      let reset_pc = consti M.o.operand_width M.o.reset_pc in
      let nop = consti Defines.Opcode.width Defines.Opcode.nop @: zero 26 in
      let pc = R.g_reg ~rv:reset_pc ~cv:reset_pc ~e:vdd M.o.operand_width in
      let fetched_pc = R.g_reg ~rv:reset_pc ~cv:reset_pc ~e:vdd M.o.operand_width in

      let have_early_pc_next, next_insn_will_branch, no_rf_read = 
        g_wire gnd, g_wire gnd, g_wire gnd 
      in

      let mini_cache = Mc_o.(map (fun (_,b) -> wire b) t) in

      let fetch_req = wire 1 in
      let jump_insn_in_decode = wire 1 in
      let new_insn_wasnt_ready = wire 1 in
      let took_early_calc_pc = wire 1 in
      let waited_with_early_pc_onto_cache_hit = wire 1 in

      let pc_plus_four = pc#q +:. 4 in

      let padv_r = R.reg ~e:vdd i.padv in

      let new_insn = mux2 mini_cache.Mc_o.hit mini_cache.Mc_o.insn i.ibus_dat in

      let new_insn_ready = mini_cache.Mc_o.hit |: i.ibus_ack in

      let fetch_ready = new_insn_ready |: jump_insn_in_decode |: i.ibus_err in

      let took_branch = R.reg ~e:vdd 
        ((i.branch_occur |: i.fetch_take_exception_branch) &: fetch_ready)
      in
      let took_branch_r = R.reg ~e:vdd took_branch in

      (* Pick out opcode of next instruction to go to decode stage *)
      let next_insn_opcode = sel new_insn Defines.Opcode.select in

      (* Can calculate next PC based on instruction coming in *)
      let early_pc_next = 
        let pci = 
          repeat new_insn.[25:25] 4 @: 
          sel new_insn Defines.jumpbranch_immediate_select @:
          zero 2
        in
        (pci +: pc#q) &: (repeat have_early_pc_next#q M.o.operand_width)
      in

      let pc_fetch_next = mux2 have_early_pc_next#q early_pc_next pc_plus_four in

      let will_go_to_sleep = have_early_pc_next#q &: (early_pc_next ==: pc#q) in

      let sleep = R.reg_fb ~e:vdd ~w:1
        (pmux [
          i.fetch_take_exception_branch |: i.du_stall, gnd;
          will_go_to_sleep &: (~: (i.stepping)), vdd;
        ])
      in

      let took_early_calc_pc_r = R.reg_fb ~e:vdd ~w:2 (fun d -> lsb d @: took_early_calc_pc) in

      (* The pipeline advance signal deasserted for the instruction
         we just put out, and we're still attempting to fetch. This should
         result in a deassert cycle on the request signal out to the bus.
         But, we don't want this to indicate when padv_i was deasserted for
         a branch, because we will know about that, we just want this to
         indicate it was deasserted for other reasons. *)
      let padv_deasserted = padv_r &: (~: (i.padv)) &: fetch_req &: (~: took_branch) in

      let padv_asserted = (~: padv_r) &: i.padv in

      (* This makes us hold the decode stage output for an additional
          cycle when we've already got the next instruction in the
          register output to the decode stage, but the pipeline has
          stalled. *)
      let hold_decode_output = 
        (padv_asserted &:
          mini_cache.Mc_o.hit &: took_branch_r &:
          (~: new_insn_wasnt_ready) &:
          took_early_calc_pc_r.[1:1]) |:
        waited_with_early_pc_onto_cache_hit in

      let cmap l = List.concat (List.map (fun (a,b) -> 
        List.map (fun a -> consti Defines.Opcode.width a,b) a) l) in
      let () = compile [ (* XXX check optimization *)
        g_when (new_insn_ready) [
          g_switch (next_insn_opcode) (cmap [
            [ Defines.Opcode.j; Defines.Opcode.jal ], [
              have_early_pc_next         $==. 1;
              next_insn_will_branch      $==. 1;
              no_rf_read                 $==. 1;
            ];
            [ Defines.Opcode.jr; Defines.Opcode.jalr ], [
              have_early_pc_next         $==. 0;
              next_insn_will_branch      $==. 1;
              no_rf_read                 $==. 0;
            ];
            [ Defines.Opcode.bnf ], [
              have_early_pc_next         $== ((~: (i.flag |: i.flag_set)) |: i.flag_clear);
              next_insn_will_branch      $== ((~: (i.flag |: i.flag_set)) |: i.flag_clear);
              no_rf_read                 $==. 1;
            ];
            [ Defines.Opcode.bf ], [
              have_early_pc_next         $== ((~: ((~: (i.flag)) |: i.flag_clear)) |: i.flag_set);
              next_insn_will_branch      $== ((~: ((~: (i.flag)) |: i.flag_clear)) |: i.flag_set);
              no_rf_read                 $==. 1;
            ];
            [ Defines.Opcode.systrapsync; Defines.Opcode.rfe], [
              have_early_pc_next         $==. 0;
              next_insn_will_branch      $==. 1;
              no_rf_read                 $==. 1;
            ]
          ])
        ]

      ] in

      let () = compile [
        g_if (i.branch_occur &: (~: took_early_calc_pc)) [
             pc         $== i.branch_dest;
        ] @@ g_elif (i.fetch_take_exception_branch &: (~: (i.du_stall))) [
             pc         $== i.branch_dest;
        ] @@ g_elif (new_insn_ready &: (i.padv |: i.stepping) &: (~: hold_decode_output)) [
             pc         $== pc_fetch_next;
             fetched_pc $== pc#q;
        ] @@ g_elif (i.du_restart) [
             pc         $== i.du_restart_pc;
        ] @@ g_elif (i.fetch_take_exception_branch &: i.du_stall) [
             pc         $== i.du_restart_pc;
        ] []
      ] in

      let () = new_insn_wasnt_ready <== R.reg_fb ~e:vdd ~w:1
        (pmux [
          (i.branch_occur &: (~: took_early_calc_pc)), ~: new_insn_ready;
          (new_insn_ready &: (i.padv |: i.stepping) &: (~: padv_deasserted)), gnd;
        ])
      in

      let next_instruction_to_decode_condition = 
        new_insn_ready &:
        (i.padv |: i.stepping) &:
        (~: padv_deasserted) &:
        (~: hold_decode_output) &:
        (~: ((i.branch_occur &: i.padv &: (~: took_early_calc_pc)) |:
          i.fetch_take_exception_branch));
      in

      let decode_insn = R.reg_fb ~rv:nop ~cv:nop ~e:vdd ~w:M.o.operand_width
        (pmux [
          (sleep |: i.du_stall), nop;
          (next_instruction_to_decode_condition), new_insn;
          (* We've just taken a branch, put a nop on the
             instruction to the rest of the pipeline *)
          (i.branch_occur &: i.padv), nop;
          (* Exception was just taken, get rid of whatever
             we're outputting *)
          (i.fetch_take_exception_branch), nop;
          (* This covers the case where, for some reason,
            we don't get the branch_occur_i *)
          (took_early_calc_pc), nop;
          (* If the current instruction in the decode stage is retired
             then let's put a no-op back in the pipeline *)
          (i.ctrl_insn_done &: (~: new_insn_ready)), nop;
        ])
      in

      let () = fetch_req <== R.reg ~rv:vdd ~cv:vdd ~e:vdd 
        (pmux (List.map (fun a -> a,gnd) [
          (* Deassert on ack *)
          fetch_req &: i.stepping &: new_insn_ready;
          (~: fetch_req) &: i.du_stall;
          i.ibus_err;
          sleep;
          next_insn_will_branch#q;
          (* Put the execute wait signal through this register to break any long
             chains of logic from the execute stage (LSU, ALU) which could result
             from using it to just gate the req signal out.
             TODO - actually check the impact of gating fetch_req_o with
                    execute_waiting_i *)
          i.execute_waiting;
          padv_deasserted;
          (* We'll get this ungated signal immediately after we've
             terminated a burst, so we'll know if we really should
             fetch the branch target or whether it's in cache. *)
          mini_cache.Mc_o.hit_ungated;
        ]) vdd)
      in

      let took_early_pc_onto_cache_hit = R.reg_fb ~e:vdd ~w:1 
        (pmux [
          i.padv, took_early_calc_pc &: mini_cache.Mc_o.hit &: 
                  (~: (i.fetch_take_exception_branch));
          i.ctrl_insn_done, gnd;
        ])
      in

      (* This register signifies when:
         a) we had a branch to somewhere where we took the early calculated PC and
            that branch location was a hit in the cache
         b) the subsequent instruction wasn't in the cache, so we put the
            insn out to the decode stage, but wasn't immediately retired by the
            control stage, so we must wait until the next instruction is ready
            before it will be completed by the control stage *)
      let () = waited_with_early_pc_onto_cache_hit <== R.reg_fb ~e:vdd ~w:1
        (pmux [
          took_branch_r |: i.padv, took_early_pc_onto_cache_hit &: (~: fetch_ready);
          i.ctrl_insn_done, gnd
        ])
      in

      let () = jump_insn_in_decode <== R.reg ~e:vdd 
        (pmux [
          sleep, gnd;
          ((~: jump_insn_in_decode) &: next_insn_will_branch#q &: new_insn_ready &: i.padv), vdd;
        ] gnd)
      in

      let () = took_early_calc_pc <== R.reg ~e:vdd
        (pmux [
          sleep, gnd;
          (next_insn_will_branch#q &: have_early_pc_next#q &: i.padv), vdd;
        ] gnd)
      in

      (* A signal to make sure the request out line stays high
       if we've already issued an instruction request and padv_i
       goes low. *)
      let complete_current_req = R.reg_fb ~e:vdd ~w:1
        (fun complete_current_req -> pmux [
          fetch_req &: padv_deasserted &: (~: new_insn_ready), vdd;
          new_insn_ready &: complete_current_req, gnd;
        ] complete_current_req)
      in

      (* mini-cache *)
      let _ = 
        if M.f.instructioncache then
          let mc = mk_mini_cache ~i ~pc:pc#q ~took_branch ~will_go_to_sleep in
          Mc_o.(map2 (fun w d -> w <== d) mini_cache mc) 
        else
          Mc_o.(map (fun w -> w <== (zero (width w))) mini_cache) 
      in

      (* outputs *)
      let ibus_adr = pc#q in
      let ibus_req = (fetch_req &: (~: (i.fetch_take_exception_branch)(* | branch_occur_i*))
                     (* This is needed in the case that:
                        1. a burst just finished and ack in went low because of this
                        2. the instruction we just ACKed is a multicycle insn so the
                        execute_waiting_i goes high, but the bus interface will have
                        already put out the request onto the bus. It causes a bug
                        if we deassert the req from here 1 cycle later, so put this
                        signal into the assign logic so that the first cycle of it
                        causes req to go low, after which fetch_req is deasserted
                        and should handle it *)
                      &: (~: (i.execute_waiting &: fetch_req))
                      &: (~: (mini_cache.Mc_o.hit_ungated))) |:
                      complete_current_req in
      let ibus_burst = gnd in
      let fetched_pc = fetched_pc#q in

      (* Register file control *)
      let rf_sel s = mux2 new_insn_ready (sel new_insn s) (zero M.o.rf_addr_width) in
      let fetch_rfa_adr = rf_sel Defines.ra_select in
      let fetch_rfb_adr = rf_sel Defines.rb_select in
      let fetch_rf_re = new_insn_ready &: (i.padv |: i.stepping) &:
                        (~: (no_rf_read#q |: hold_decode_output)) in

      let decode_except_ibus_err = R.reg_fb ~e:vdd ~w:1
        (pmux [
          (i.padv |: i.fetch_take_exception_branch) &: i.branch_occur |: i.du_stall, gnd;
          fetch_req, i.ibus_err;
        ])
      in

      let fetch_sleep = sleep in
      let fetch_quick_branch = mini_cache.Mc_o.fetch_quick_branch in

      let spr_bus_ack_ic = vdd in
      let spr_bus_dat_ic = zero M.o.operand_width in

      O.{
        ibus_adr;
        ibus_req;
        ibus_burst;
        decode_insn;
        fetched_pc;
        fetch_ready;
        fetch_rfa_adr;
        fetch_rfb_adr;
        fetch_rf_re;
        pc_fetch_next;
        decode_except_ibus_err;
        fetch_sleep;
        fetch_quick_branch;
        spr_bus_dat_ic;
        spr_bus_ack_ic;
      }

    module Inst = M.Inst(I)(O)
    let fetch_inst = Inst.inst "fetch" fetch
  end
end

module Espresso = struct

  (* ****************************************************************************
    This Source Code Form is subject to the terms of the
    Open Hardware Description License, v. 1.0. If a copy
    of the OHDL was not distributed with this file, You
    can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

    Description: mor1kx espresso fetch unit

    Fetch insn, advance PC (or take new branch address) on padv_i.

    What we might want to do is have a 1-insn buffer here, so when the current
    insn is fetched, but the main pipeline doesn't want it yet

    indicate ibus errors

    Copyright (C) 2012 Authors

    Author(s): Julius Baxter <juliusbaxter@gmail.com>
              Andy Ray <andy.ray@ujamjar.com>

  ***************************************************************************** *)

  module Make(M : Utils.Module_cfg_signal) = struct
    open M.Bits

    module I = interface 
      clk[1]
      rst[1]
      ibus_err[1]
      ibus_ack[1]
      ibus_dat[Defines.insn_width]
      padv[1]
      branch_occur[1]
      branch_dest[M.o.operand_width]
      du_restart[1]
      du_restart_pc[M.o.operand_width]
      fetch_take_exception_branch[1]
      execute_waiting[1]
      du_stall[1]
      stepping[1]
    end

    module O = interface 
      ibus_adr[M.o.operand_width]
      ibus_req[1]
      ibus_burst[1]
      decode_insn[Defines.insn_width]
      next_fetch_done[1]
      fetch_rfa_adr[M.o.rf_addr_width]
      fetch_rfb_adr[M.o.rf_addr_width]
      pc_fetch[M.o.operand_width]
      pc_fetch_next[M.o.operand_width]
      decode_except_ibus_err[1]
      fetch_advancing[1]
    end

    let fetch i = 
      let open I in
      let open HardCaml.Signal.Guarded in
      let module R = Regs(struct let clk = i.clk let rst = i.rst end) in

      let reset_pc = consti M.o.operand_width M.o.reset_pc in
      let nop = consti Defines.Opcode.width Defines.Opcode.nop @: zero 26 in

      let next_fetch_done = wire 1 in
      let pc_fetch = wire M.o.operand_width in
      let insn_buffer = wire Defines.insn_width in
      let next_insn_buffered = wire 1 in

      let taking_branch = i.branch_occur &: i.padv in
   
      let bus_access_done = (i.ibus_ack |: i.ibus_err) &: (~: (taking_branch)) in
   
      let pc_fetch_next = pc_fetch +:. 4 in
   
      let bus_access_done_r = R.reg ~e:vdd bus_access_done in
      let branch_occur_r = R.reg ~e:vdd i.branch_occur in

      (* Register rising edge on bus_access_done *)
      let bus_access_done_re_r = R.reg ~e:vdd (bus_access_done &: (~: bus_access_done_r)) in
   
      let bus_access_done_fe = (~: bus_access_done) &: bus_access_done_r in
   
      let fetch_advancing = (i.padv |: i.fetch_take_exception_branch |: i.stepping) &:
                              next_fetch_done
      in
   
      let advancing_into_branch = R.reg ~e:vdd (fetch_advancing &: i.branch_occur) in
   
      let () = next_fetch_done <== 
        ((bus_access_done_r |: next_insn_buffered) &:
           (* Whenever we've just changed the fetch PC to
              take a branch this will gate off any ACKs we
              might get (legit or otherwise) from where we're
              getting our instructions from (bus/cache). *)
           (~: advancing_into_branch))
      in
   
      let branch_occur_re = i.branch_occur &: (~: branch_occur_r) in
   
      (* When this occurs we had the insn burst stream finish just as we
       had a new branch address requested. Because the control logic will
       immediately continue onto the delay slot instruction, the branch target
       is only valid for 1 cycle. The PC out to the bus/cache will then need
       to change 1 cycle after it requested the insn after the delay slot.
       This is annoying for the bus control/cache logic, but should result in
       less cycles wasted fetching something we don't need, and as well reduce
       the number of flops as we don't need to save the target PC which we had
       for only 1 cycle *)
      let awkward_transition_to_branch_target = branch_occur_re &: bus_access_done_fe in
   
      let wait_for_exception_after_ibus_err  = R.reg_fb ~e:vdd ~w:1
        (pmux [
          i.fetch_take_exception_branch, gnd;
          i.ibus_err, vdd;
        ])
      in

      let jal_buffered =
        let opcode = sel insn_buffer Defines.Opcode.select in
        (opcode ==:. Defines.Opcode.jalr) |:
        (opcode ==:. Defines.Opcode.jal) 
      in
      let retain_fetch_pc = jal_buffered &: bus_access_done in
   
      let () = 
        let e = (i.fetch_take_exception_branch |:
                (((bus_access_done &: (~: (i.ibus_err))) |: taking_branch) &:
                  ((~: (i.execute_waiting)) |: (~: next_insn_buffered)) &:
                  (~: retain_fetch_pc)) |:
                awkward_transition_to_branch_target |:
                i.du_restart)
        in
        (* next PC - are we going somewhere else or advancing? *)
        let d = 
          mux2 i.du_restart i.du_restart_pc @@
          mux2 (i.fetch_take_exception_branch |: taking_branch) i.branch_dest @@
          pc_fetch_next;
        in
        pc_fetch <== R.reg ~cv:reset_pc ~rv:reset_pc  ~e d
      in
   
      let fetch_req = R.reg_fb ~rv:vdd ~cv:vdd ~e:vdd ~w:1 
        (fun fetch_req -> pmux [
          i.fetch_take_exception_branch |: i.du_restart, vdd;
            (* Force de-assert of req signal when branching.
              This is to stop (ironically) the case where we've got the
              instruction we're branching to already coming in on the bus,
              which we usually don't assume will happen.
              TODO: fix things so that we don't have to force a penalty to make
              it work properly. *)
          i.padv, (~: (i.branch_occur)) &: (~: (i.du_stall));
          i.du_stall, fetch_req &: (~: bus_access_done);
          ((~: fetch_req) &: (~: (i.execute_waiting)) &:
          (~: wait_for_exception_after_ibus_err) &: (~: retain_fetch_pc) &:
          (~: (i.du_stall)) &: (~: (i.stepping))),
            vdd;
          (bus_access_done &: (i.fetch_take_exception_branch |:
                              i.execute_waiting |: i.ibus_err |: i.stepping)),
            gnd;
        ] fetch_req)
      in

      (* If insn_buffer contains the next insn we need, save that information here *)
      let () = next_insn_buffered <== R.reg_fb ~e:vdd ~w:1
        (pmux [
          i.fetch_take_exception_branch, gnd;
          (* Next instruction is usually buffered when we've got bus ack and
            pipeline advance, except when we're branching (usually throw
            away the fetch when branch is being indicated) *)
          i.padv, i.ibus_ack &: (~: (i.branch_occur));
          (i.ibus_ack &: i.execute_waiting), vdd;
        ])
      in
   
      let () = 
        let e = 
          (i.ibus_ack &: ((~: (i.execute_waiting)) |: (~: next_insn_buffered)) &:
                 (* Don't buffer instruction after delay slot instruction
                    (usually we're receiving it as taking branch is asserted)
                    it could be another jump instruction and having it in
                    the insn_buffer has annoying side-effects. *)
                 (~: taking_branch))
        in
        insn_buffer <== R.reg ~rv:nop ~cv:nop ~e i.ibus_dat
      in

      let ibus_adr = pc_fetch in
      let ibus_req = fetch_req in
      let ibus_burst = gnd in

      let decode_insn = R.reg_fb ~rv:nop ~cv:nop ~e:vdd ~w:Defines.insn_width
        (pmux [
          (* Put a NOP in the pipeline when starting exception - remove any state
             which may be causing the exception *)
          (i.fetch_take_exception_branch |: (i.du_stall &: (~: (i.execute_waiting)))),
            nop;
          ((i.padv &: (
                      bus_access_done_r |:
                      bus_access_done |:
                      next_insn_buffered
                     ) &:
             (~: branch_occur_r)) |:
           (* This case is when we stalled to get the delay-slot instruction
           and we don't get enough padv to push it through the buffer *)
           (i.branch_occur &: i.padv &: bus_access_done_re_r) |:
           (bus_access_done_fe &: i.stepping)),
            insn_buffer;
        ])
      in
   
      (* Early RF address fetch *)
      let fetch_rfa_adr = sel insn_buffer Defines.ra_select  in
      let fetch_rfb_adr = sel insn_buffer Defines.rb_select  in
   
      let decode_except_ibus_err = R.reg_fb ~e:vdd ~w:1
        (pmux [
          ((i.padv |: i.fetch_take_exception_branch) &: i.branch_occur |: i.du_stall), gnd;
          fetch_req, i.ibus_err;
        ])
      in
   
      O.{
        ibus_adr;
        ibus_req;
        ibus_burst;
        decode_insn;
        next_fetch_done;
        fetch_rfa_adr;
        fetch_rfb_adr;
        pc_fetch;
        pc_fetch_next;
        decode_except_ibus_err;
        fetch_advancing;
      }

    module Inst = M.Inst(I)(O)
    let fetch_inst = Inst.inst "fetch" fetch
  end
end

module Cappuccino = struct
  (* ****************************************************************************
    This Source Code Form is subject to the terms of the
    Open Hardware Description License, v. 1.0. If a copy
    of the OHDL was not distributed with this file, You
    can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

    Description: mor1kx fetch/address stage unit

    basically an interface to the ibus/icache subsystem that can react to
    exception and branch signals.

    Copyright (C) 2012 Authors

    Author(s): Julius Baxter <juliusbaxter@gmail.com>
               Stefan Kristiansson <stefan.kristiansson@saunalahti.fi>
               Andy Ray <andy.ray@ujamjar.com>

  ***************************************************************************** *)
  module Make(M : Utils.Module_cfg_signal) = struct
    open M.Bits
    
    module I = interface 
      clk[1]
      rst[1]
      spr_bus_addr[16]
      spr_bus_we[1]
      spr_bus_stb[1]
      spr_bus_dat[M.o.operand_width]
      ic_enable[1]
      immu_enable[1]
      supervisor_mode[1]
      ibus_err[1]
      ibus_ack[1]
      ibus_dat[Defines.insn_width]
      padv[1]
      padv_ctrl[1] 
      decode_branch[1]
      decode_branch_target[M.o.operand_width]
      ctrl_branch_exception[1]
      ctrl_branch_except_pc[M.o.operand_width]
      du_restart[1]
      du_restart_pc[M.o.operand_width]
      decode_op_brcond[1]
      branch_mispredict[1]
      execute_mispredict_target[M.o.operand_width]
      pipeline_flush[1]
      doing_rfe[1]
    end
    
    module O = interface 
      spr_bus_dat_ic[M.o.operand_width]
      spr_bus_ack_ic[1]
      spr_bus_dat_immu[M.o.operand_width]
      spr_bus_ack_immu[1]
      ibus_req[1]
      ibus_adr[M.o.operand_width]
      ibus_burst[1]
      pc_decode[M.o.operand_width]
      decode_insn[Defines.insn_width]
      fetch_valid[1]
      fetch_rfa_adr[M.o.rf_addr_width]
      fetch_rfb_adr[M.o.rf_addr_width]
      fetch_rf_adr_valid[1]
      decode_except_ibus_err[1]
      decode_except_itlb_miss[1]
      decode_except_ipagefault[1]
      fetch_exception_taken[1]
    end
    
    module Ic = Icache.Make(M)
    module Immu = Immu.Make(M)

    type sm = Idle | Read | Tlb_reload | Ic_refill
      deriving(Bounded, Enum)

    let fetch i = 
      let open I in
      let open HardCaml.Signal.Guarded in
      let module R = Regs(struct let clk = i.clk let rst = i.rst end) in

      let nop = consti Defines.Opcode.width Defines.Opcode.nop @: zero 26 in
      let reset_pc = consti M.o.operand_width M.o.reset_pc in

      let state_is, sm, next = R.statemachine ~e:vdd
        (Enum_sm.enum_from_to Bounded_sm.min_bound Bounded_sm.max_bound)
      in
      let s_idle, s_refill = state_is Idle, state_is Ic_refill in

      let ibus_ack = R.g_reg ~e:vdd 1 in
      let exception_while_tlb_reload = R.g_reg ~e:vdd 1 in
      let ibus_adr = R.g_reg ~e:vdd M.o.operand_width in
      let ibus_req = R.g_reg ~e:vdd 1 in
      let ibus_dat = R.g_reg ~e:vdd Defines.insn_width in
      let tlb_reload_ack = R.g_reg ~e:vdd 1 in
      let tlb_reload_data = R.g_reg ~e:vdd M.o.operand_width in

      let ic = Ic.O.(map (fun (_,b) -> wire b) t) in
      let immu = Immu.O.(map (fun (_,b) -> wire b) t) in

      (********************************************************)
      let fetch_valid = wire 1 in
      let ic_access = wire 1 in
      let mispredict_stall = wire 1 in
      let nop_ack = wire 1 in
      let except_ipagefault_clear = wire 1 in
      let pc_fetch = wire M.o.operand_width in
      (********************************************************)

      let ibus_access = 
        ((~: ic_access) |: immu.Immu.O.tlb_reload_busy |: ic.Ic.O.invalidate) &:
        (~: (ic.Ic.O.refill)) |:
        (~: s_idle) &: (~: s_refill) |:
        ibus_ack#q
      in
      let imem_ack = mux2 ibus_access ibus_ack#q ic.Ic.O.cpu_ack in

      let imem_err = R.reg ~e:vdd i.ibus_err in
        
      let bus_access_done = 
        (imem_ack |: imem_err |: nop_ack) &: 
        (~: (immu.Immu.O.busy)) &: (~: (immu.Immu.O.tlb_reload_busy))
      in
      
      let ctrl_branch_exception_edge = 
        let ctrl_branch_exception_r = R.reg ~e:vdd i.ctrl_branch_exception in
        i.ctrl_branch_exception &: (~: ctrl_branch_exception_r);
      in

      let fetch_exception_taken = R.reg_fb ~e:vdd ~w:1
        (fun fetch_exception_taken -> pmux [
          fetch_exception_taken, gnd;
          (i.ctrl_branch_exception &: bus_access_done &: i.padv), vdd;
        ] gnd)
      in

      let flush = R.reg_fb ~e:vdd ~w:1 
        (pmux [
          (bus_access_done &: i.padv |: i.du_restart), gnd;
          i.pipeline_flush, vdd;
        ])
      in
      (* pipeline_flush_i comes on the same edge as branch_except_occur during
         rfe, but on an edge later when an exception occurs, but we always need
         to keep on flushing when the branch signal comes in. *)
      let flushing = i.pipeline_flush |: ctrl_branch_exception_edge |: flush in

      (* used to keep fetch_valid_o high during stall *)
      let stall_fetch_valid = (~: (i.padv)) &: fetch_valid in

      (*  fetch_valid_o generation *)
      let () = fetch_valid <== R.reg ~e:vdd
        (pmux [
          i.pipeline_flush, gnd;
          (bus_access_done &: i.padv &: (~: mispredict_stall) &: (~: (immu.Immu.O.busy)) &:
            (~: (immu.Immu.O.tlb_reload_busy)) |: stall_fetch_valid), vdd;
        ] gnd)
      in

      let except_itlb_miss = 
        immu.Immu.O.tlb_miss &: i.immu_enable &: bus_access_done &:
        (~: mispredict_stall) &: (~: (i.doing_rfe))
      in
      let except_ipagefault = 
        immu.Immu.O.pagefault &: i.immu_enable &: bus_access_done &:
        (~: mispredict_stall) &: (~: (i.doing_rfe)) |:
        immu.Immu.O.tlb_reload_pagefault
      in

      let decode_except_ibus_err = R.reg_fb ~e:vdd ~w:1 
        (fun decode_except_ibus_err -> (pmux [
          i.du_restart, gnd;
          imem_err, vdd;
          (decode_except_ibus_err &: i.ctrl_branch_exception), gnd;
        ] decode_except_ibus_err))
      in

      let decode_except_itlb_miss  = R.reg_fb ~e:vdd ~w:1
        (fun decode_except_itlb_miss -> (pmux [
          i.du_restart, gnd;
          immu.Immu.O.tlb_reload_busy, gnd;
          except_itlb_miss, vdd;
          (decode_except_itlb_miss &: i.ctrl_branch_exception), gnd;
        ] decode_except_itlb_miss))
      in

      let decode_except_ipagefault = R.reg_fb ~e:vdd ~w:1
        (pmux [
          i.du_restart, gnd;
          except_ipagefault, vdd;
          except_ipagefault_clear, gnd;
        ])
      in
      let () = 
        except_ipagefault_clear <== (decode_except_ipagefault &: i.ctrl_branch_exception)
      in

      let addr_valid = 
        bus_access_done &: i.padv &:
        (~: (except_itlb_miss |: except_ipagefault)) |:
        decode_except_itlb_miss &: i.ctrl_branch_exception |:
        decode_except_ipagefault &: i.ctrl_branch_exception |:
        i.doing_rfe
      in

      (* Branch misprediction stall logic *)
      let fetching_brcond = R.reg_fb ~e:vdd ~w:1
        (pmux [
          i.pipeline_flush, gnd;
          (i.decode_op_brcond &: addr_valid), vdd;
          (bus_access_done &: i.padv |: i.du_restart), gnd;
        ])
      in

      let fetching_mispredicted_branch = R.reg_fb ~e:vdd ~w:1
        (pmux [
          i.pipeline_flush, gnd;
          (bus_access_done &: i.padv |: i.du_restart), gnd;
          (fetching_brcond &: i.branch_mispredict &: i.padv), vdd;
        ])
      in

      let () = mispredict_stall <== 
        (fetching_mispredicted_branch |: i.branch_mispredict &: fetching_brcond)
      in

      let imem_dat = 
          mux2 (nop_ack |: except_itlb_miss |: except_ipagefault) nop @@
          mux2 ibus_access ibus_dat#q ic.Ic.O.cpu_dat
      in

      let ic_enable_r = R.reg_fb ~e:vdd ~w:1
        (pmux [
          (i.ic_enable &: (~: (ibus_req#q))), vdd;
          ((~: (i.ic_enable)) &: (~: (ic.Ic.O.refill))), gnd;
        ])
      in
      let ic_enabled = i.ic_enable &: ic_enable_r in

      let pc_addr = R.reg ~rv:reset_pc ~cv:reset_pc ~e:vdd
        (pmux [
          i.du_restart, i.du_restart_pc;
          i.ctrl_branch_exception &: (~: fetch_exception_taken), i.ctrl_branch_except_pc;
          i.branch_mispredict |: fetching_mispredicted_branch, i.execute_mispredict_target;
          i.decode_branch, i.decode_branch_target;
        ] pc_fetch)
      in

      let () = pc_fetch <== R.reg
        ~rv:reset_pc ~cv:reset_pc
        ~e:(addr_valid |: i.du_restart) pc_addr 
      in

      let pc_decode = R.reg ~e:(bus_access_done &: i.padv &: (~: mispredict_stall)) pc_fetch in

      let ic_addr = mux2 (addr_valid |: i.du_restart) pc_addr pc_fetch in
      let ic_addr_match = mux2 i.immu_enable immu.Immu.O.phys_addr pc_fetch in

      let next_ibus_adr = 
        if M.o.icache_block_width = 5 then
          (ibus_adr#q.[31:5] @: (ibus_adr#q.[4:0] +:. 4))  (* 32 byte *)
        else
          (ibus_adr#q.[31:4] @: (ibus_adr#q.[3:0] +:. 4))  (* 16 byte *)
      in

      let () = compile [
        ibus_ack $==. 0;
        exception_while_tlb_reload $==. 0;
        tlb_reload_ack $==. 0;

        sm [
          Idle, [
            ibus_req $==. 0;
            g_if (i.padv &: ibus_access &: (~: (ibus_ack#q)) &: (~: imem_err) &: (~: nop_ack)) [
                g_if (immu.Immu.O.tlb_reload_req) [
                  ibus_adr $== immu.Immu.O.tlb_reload_addr;
                  ibus_req $==. 1;
                  next Tlb_reload;
                ] @@ g_elif (i.immu_enable) [
                  ibus_adr $== immu.Immu.O.phys_addr;
                  g_when Immu.O.((~: (immu.tlb_miss)) &: 
                                 (~: (immu.pagefault)) &: 
                                 (~: (immu.busy))) [
                      ibus_req $==. 1;
                      next Read;
                  ]
                ] @@ g_elif ((~: (i.ctrl_branch_exception)) |: i.doing_rfe) [
                  ibus_adr $== pc_fetch;
                  ibus_req $==. 1;
                  next Read;
                ] []
            ] @@ g_elif (ic.Ic.O.refill_req) [
                ibus_adr $== ic_addr_match;
                ibus_req $==. 1;
                next Ic_refill;
            ] []
          ];

          Ic_refill, [
            ibus_req $==. 1;
            g_when (i.ibus_ack) [
                ibus_adr $== next_ibus_adr;
                g_when (ic.Ic.O.refill_done) [
                  ibus_req $==. 0;
                  next Idle;
                ]
            ]
          ];

          Read, [
            ibus_ack $== i.ibus_ack;
            ibus_dat $== i.ibus_dat;
            g_when (i.ibus_ack |: i.ibus_err) [
                ibus_req $==. 0;
                next Idle;
            ]
          ];

          Tlb_reload, [
            g_when (i.ctrl_branch_exception) [
              exception_while_tlb_reload $==. 1;
            ];

            ibus_adr $== immu.Immu.O.tlb_reload_addr;
            tlb_reload_data $== i.ibus_dat;
            tlb_reload_ack $== (i.ibus_ack &: immu.Immu.O.tlb_reload_req);

            g_when (~: (immu.Immu.O.tlb_reload_req)) [
              next Idle;
            ];

            ibus_req $== immu.Immu.O.tlb_reload_req;
            g_when (i.ibus_ack |: tlb_reload_ack#q) [
              ibus_req $==. 0;
            ];
          ];

        ]
      ] in

      let fetch_rfa_adr = sel imem_dat Defines.ra_select in
      let fetch_rfb_adr = sel imem_dat Defines.rb_select in
      let fetch_rf_adr_valid = bus_access_done &: i.padv in
 
      let ic_refill_allowed = 
        ((~: ((immu.Immu.O.tlb_miss |: immu.Immu.O.pagefault) &: i.immu_enable)) &:
        (~: (i.ctrl_branch_exception)) &: (~: (i.pipeline_flush)) &:
        (~: mispredict_stall) |: (i.doing_rfe)) &:
        (~: (immu.Immu.O.tlb_reload_busy)) &: (~: (immu.Immu.O.busy))
      in
      let ic_req = 
        i.padv &: (~: decode_except_ibus_err) &:
        (~: decode_except_itlb_miss) &: (~: except_itlb_miss) &:
        (~: decode_except_ipagefault) &: (~: except_ipagefault) &:
        ic_access &: ic_refill_allowed
      in

      let () = nop_ack <== Immu.O.(
        i.padv &: (~: bus_access_done) &: (~: (ibus_req#q &: ibus_access)) &:
		    ((i.immu_enable &: (immu.tlb_miss |: immu.pagefault) &: (~: (immu.tlb_reload_busy))) |:
		    ctrl_branch_exception_edge &: (~: (immu.tlb_reload_busy)) |:
		    exception_while_tlb_reload#q &: (~: (immu.tlb_reload_busy)) |:
		    immu.tlb_reload_pagefault |:
		    mispredict_stall)
      )
      in

      (* icache *)
      let () = 
        if M.f.instructioncache then
          let ic_access' = 
            if M.o.icache_limit_width = M.o.operand_width then
              ic_enabled &: (~: (immu.Immu.O.cache_inhibit &: i.immu_enable))
            else if (M.o.icache_limit_width < M.o.operand_width) then
              ic_enabled &:
                (ic_addr_match.[M.o.operand_width-1:M.o.icache_limit_width] ==:. 0) &:
                (~: (immu.Immu.O.cache_inhibit &: i.immu_enable))
            else 
              failwith ("ERROR: OPTION_ICACHE_LIMIT_WIDTH > OPTION_OPERAND_WIDTH");
          in
          let ic' = Ic.icache_inst 
            Ic.I.{
              clk = i.clk;
              rst = i.rst |: imem_err; (* XXX NONONO *)
              ic_access;
              cpu_adr = ic_addr;
              cpu_adr_match = ic_addr_match;
              cpu_req = ic_req;
              wradr = ibus_adr#q;
              wrdat = i.ibus_dat;
              we = i.ibus_ack;
              spr_bus_addr = i.spr_bus_addr;
              spr_bus_we = i.spr_bus_we;
              spr_bus_stb = i.spr_bus_stb;
              spr_bus_dat_i = i.spr_bus_dat;
            }
          in
          let _ = Ic.O.(map2 (fun q d -> q <== d) ic ic') in
          let () = ic_access <== ic_access' in
          ()
        else
          let _ = Ic.O.(map2 (fun q (_,b) -> q <== zero b) ic t) in
          let () = ic_access <== gnd in
          ()
      in

      (* immu *)
      let () = 
        if M.f.immu then
          let immu_enable = 
            i.immu_enable &: (~: (i.pipeline_flush)) &: (~: (mispredict_stall)) 
          in
          let virt_addr = ic_addr in
          let immu_spr_bus_stb = i.spr_bus_stb &: (~: (i.padv_ctrl) |: i.spr_bus_we) in
          let immu' = Immu.immu_inst
            Immu.I.{
              clk = i.clk;
              rst = i.rst;
              enable = immu_enable;
              virt_addr;
              virt_addr_match = pc_fetch;
              supervisor_mode = i.supervisor_mode;
              tlb_reload_ack = tlb_reload_ack#q;
              tlb_reload_data = tlb_reload_data#q;
              tlb_reload_pagefault_clear = except_ipagefault_clear;
              spr_bus_addr = i.spr_bus_addr;
              spr_bus_we = i.spr_bus_we;
              spr_bus_stb = immu_spr_bus_stb;
              spr_bus_dat_i = i.spr_bus_dat;
            }
          in
          let _ = Immu.O.(map2 (fun q d -> q <== d) immu immu') in
          ()
        else
          let _ = Immu.O.(map2 (fun q (_,b) -> q <== zero b) immu t) in
          ()
      in

      let ibus_burst = (~: ibus_access) &: ic.Ic.O.refill &: (~: (ic.Ic.O.refill_done)) in
      let decode_insn = R.reg_fb
        ~rv:nop ~cv:nop ~e:vdd ~w:Defines.insn_width
        (pmux [
          (imem_err |: flushing), nop;
          (bus_access_done &: i.padv &: (~: mispredict_stall)), imem_dat;
        ])
      in

      O.{
        spr_bus_dat_ic = ic.Ic.O.spr_bus_dat_o;
        spr_bus_ack_ic = ic.Ic.O.spr_bus_ack;
        spr_bus_dat_immu = immu.Immu.O.spr_bus_dat_o;
        spr_bus_ack_immu = immu.Immu.O.spr_bus_ack;
        ibus_req = ibus_req#q;
        ibus_adr = ibus_adr#q;
        ibus_burst;
        pc_decode;
        decode_insn;
        fetch_valid;
        fetch_rfa_adr;
        fetch_rfb_adr;
        fetch_rf_adr_valid;
        decode_except_ibus_err;
        decode_except_itlb_miss;
        decode_except_ipagefault;
        fetch_exception_taken;
      }
    
    module Inst = M.Inst(I)(O)
    let fetch_inst = Inst.inst "fetch" fetch
  end
end

