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
    
    module L = Utils.Logic(M.Bits)
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
            (L.sel insn_buffer Defines.Opcode.select)
				    (L.sel i.ibus_dat Defines.Opcode.select)
      in

      let cmap = List.map (fun (a,b) -> consti Defines.Opcode.width a, b) in
      let () = compile [ (* XXX check optimization *)
        g_when ((i.ibus_ack &: (~: (just_took_branch_addr#q))) |: insn_buffered#q) [
          g_switch (next_insn_opcode) (cmap [
            Defines.Opcode.j, [
              next_insn_will_branch	$==. 1;
            ];
            Defines.Opcode.jal, [
              next_insn_will_branch	$==. 1;
            ];
            Defines.Opcode.jr, [
              next_insn_will_branch	$==. 1;
            ];
            Defines.Opcode.jalr, [
              next_insn_will_branch	$==. 1;
            ];
            Defines.Opcode.bnf, [
              next_insn_will_branch	$== ((~: (i.flag |: i.flag_set)) |: i.flag_clear);
            ];
            Defines.Opcode.bf, [
              next_insn_will_branch	$== ((~: ((~: (i.flag)) |: i.flag_clear)) |: i.flag_set);
            ];
            Defines.Opcode.systrapsync, [
              next_insn_will_branch	$==. 1;
            ];
            Defines.Opcode.rfe, [
              next_insn_will_branch	$==. 1;
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

      let fetch_rfa_adr	= 
        mux2 insn_buffered#q
          (L.sel insn_buffer Defines.ra_select)
				  (L.sel i.ibus_dat Defines.ra_select) 
      in
      let fetch_rfb_adr	= 
        mux2 insn_buffered#q
          (L.sel insn_buffer Defines.rb_select)
				  (L.sel i.ibus_dat Defines.rb_select) 
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
  module Make(M : Utils.Module_cfg_signal) = struct
    module L = Utils.Logic(M.Bits)
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

      (*****************************************************)
      let mini_cache_hit_ungated = wire 1 in
      let mini_cache_hit = wire 1 in
      let mini_cache_insn = wire Defines.insn_width in
      let fetch_quick_branch = wire 1 in

      let fetch_req = wire 1 in
      let jump_insn_in_decode = wire 1 in
      let new_insn_wasnt_ready = wire 1 in
      let took_early_calc_pc = wire 1 in
      let waited_with_early_pc_onto_cache_hit = wire 1 in
      (*****************************************************)

      let pc_plus_four = pc#q +:. 4 in

      let padv_r = R.reg ~e:vdd i.padv in

      let new_insn = mux2 mini_cache_hit mini_cache_insn i.ibus_dat in

      let new_insn_ready = mini_cache_hit |: i.ibus_ack in

      let fetch_ready = new_insn_ready |: jump_insn_in_decode |: i.ibus_err in

      let took_branch = R.reg ~e:vdd 
        ((i.branch_occur |: i.fetch_take_exception_branch) &: fetch_ready)
      in
      let took_branch_r = R.reg ~e:vdd took_branch in

      (* Pick out opcode of next instruction to go to decode stage *)
      let next_insn_opcode = L.sel new_insn Defines.Opcode.select in

      (* Can calculate next PC based on instruction coming in *)
      let early_pc_next = 
        let pci = 
          repeat new_insn.[25:25] 4 @: 
          L.sel new_insn Defines.jumpbranch_immediate_select @:
          zero 2
        in
        (pci +: pc#q) &: (repeat have_early_pc_next#q M.o.operand_width)
      in

      let pc_fetch_next = mux2 have_early_pc_next#q early_pc_next pc_plus_four in

      let will_go_to_sleep = have_early_pc_next#q &: (early_pc_next ==: pc#q) in

      let sleep = R.reg_fb ~e:vdd ~w:1
        (L.pmux [
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
          mini_cache_hit &: took_branch_r &:
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
        (L.pmux [
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
        (L.pmux [
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
        (L.pmux (List.map (fun a -> a,gnd) [
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
          mini_cache_hit_ungated;
        ]) vdd)
      in

      let took_early_pc_onto_cache_hit = R.reg_fb ~e:vdd ~w:1 
        (L.pmux [
          i.padv, took_early_calc_pc &: mini_cache_hit &: (~: (i.fetch_take_exception_branch));
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
        (L.pmux [
          took_branch_r |: i.padv, took_early_pc_onto_cache_hit &: (~: fetch_ready);
          i.ctrl_insn_done, gnd
        ])
      in

      let () = jump_insn_in_decode <== R.reg ~e:vdd 
        (L.pmux [
          sleep, gnd;
          ((~: jump_insn_in_decode) &: next_insn_will_branch#q &: new_insn_ready &: i.padv), vdd;
        ] gnd)
      in

      let () = took_early_calc_pc <== R.reg ~e:vdd
        (L.pmux [
          sleep, gnd;
          (next_insn_will_branch#q &: have_early_pc_next#q &: i.padv), vdd;
        ] gnd)
      in

      (* A signal to make sure the request out line stays high
       if we've already issued an instruction request and padv_i
       goes low. *)
      let complete_current_req = R.reg_fb ~e:vdd ~w:1
        (fun complete_current_req -> L.pmux [
          fetch_req &: padv_deasserted &: (~: new_insn_ready), vdd;
          new_insn_ready &: complete_current_req, gnd;
        ] complete_current_req)
      in

      (* XXX mini-cache *)
      let () = mini_cache_hit <== gnd in
      let () = mini_cache_hit_ungated <== gnd in
      let () = mini_cache_insn <== zero Defines.insn_width in
      let () = fetch_quick_branch <== gnd in

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
                      &: (~: mini_cache_hit_ungated)) |:
                      complete_current_req in
      let ibus_burst = gnd in
      let fetched_pc = fetched_pc#q in

      (* Register file control *)
      let rf_sel s = mux2 new_insn_ready (L.sel new_insn s) (zero M.o.rf_addr_width) in
      let fetch_rfa_adr = rf_sel Defines.ra_select in
      let fetch_rfb_adr = rf_sel Defines.rb_select in
      let fetch_rf_re = new_insn_ready &: (i.padv |: i.stepping) &:
                        (~: (no_rf_read#q |: hold_decode_output)) in

      let decode_except_ibus_err = R.reg_fb ~e:vdd ~w:1
        (L.pmux [
          (i.padv |: i.fetch_take_exception_branch) &: i.branch_occur |: i.du_stall, gnd;
          fetch_req, i.ibus_err;
        ])
      in

      let fetch_sleep = sleep in

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
  module Make(M : Utils.Module_cfg_signal) = struct
    open M.Bits
    module I = interface end
    module O = interface end
    let fetch i = O.(map (fun (_,b) -> zero b) t)
    module Inst = M.Inst(I)(O)
    let fetch_inst = Inst.inst "fetch" fetch
  end
end

module Cappuccino = struct
  module Make(M : Utils.Module_cfg_signal) = struct
    open M.Bits
    module I = interface end
    module O = interface end
    let fetch i = O.(map (fun (_,b) -> zero b) t)
    module Inst = M.Inst(I)(O)
    let fetch_inst = Inst.inst "fetch" fetch
  end
end

