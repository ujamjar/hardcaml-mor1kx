(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Description: mor1kx espresso pipeline control unit

  inputs from execute stage

  generate pipeline controls

  manage SPRs

  issue addresses for exceptions to fetch stage
  control branches going to fetch stage

  contains tick timer

  contains PIC logic

  Copyright (C) 2012 Authors

  Author(s): Julius Baxter <juliusbaxter@gmail.com>
             Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module Espresso = struct

  module Make(M : Utils.Module_cfg_signal) = struct

    open M.Bits
    open Option
    open Utils
    module Spr = Spr.Make(M.Bits)

    module I = interface
      clk[1]
      rst[1]
      ctrl_alu_result[M.o.operand_width]
      ctrl_rfb[M.o.operand_width]
      ctrl_flag_set[1] 
      ctrl_flag_clear[1]
      ctrl_opc_insn[Defines.Opcode.width]
      pc_fetch[M.o.operand_width]
      fetch_advancing[1]
      except_ibus_err[1]
      except_illegal[1]
      except_syscall[1] 
      except_dbus[1]
      except_trap[1] 
      except_align[1]
      next_fetch_done[1]
      alu_valid[1] 
      lsu_valid[1]
      op_lsu_load[1] 
      op_lsu_store[1]
      op_jr[1] 
      op_jbr[1]
      irq[32]
      carry_set[1]
      carry_clear[1]
      overflow_set[1]
      overflow_clear[1]
      du_addr[16]
      du_stb[1]
      du_dat_i[M.o.operand_width]
      du_we[1]
      du_stall_i[1]
      spr_bus_dat_dc[M.o.operand_width]
      spr_bus_ack_dc[1]
      spr_bus_dat_ic[M.o.operand_width]
      spr_bus_ack_ic[1]
      spr_bus_dat_dmmu[M.o.operand_width]
      spr_bus_ack_dmmu[1]
      spr_bus_dat_immu[M.o.operand_width]
      spr_bus_ack_immu[1]
      spr_bus_dat_mac[M.o.operand_width]
      spr_bus_ack_mac[1]
      spr_bus_dat_pmu[M.o.operand_width]
      spr_bus_ack_pmu[1]
      spr_bus_dat_pcu[M.o.operand_width]
      spr_bus_ack_pcu[1]
      spr_bus_dat_fpu[M.o.operand_width]
      spr_bus_ack_fpu[1]
      multicore_coreid[M.o.operand_width]
      rf_wb[1]
    end

    module O = interface
      flag[1]
      spr_npc[M.o.operand_width]
      spr_ppc[M.o.operand_width]
      mfspr_dat[M.o.operand_width]
      ctrl_mfspr_we[1]
      carry[1]
      pipeline_flush[1]
      padv_fetch[1]
      padv_decode[1]
      padv_execute[1]
      fetch_take_exception_branch[1]
      exception_taken[1]
      execute_waiting[1]
      stepping[1]
      du_dat_o[M.o.operand_width]
      du_ack[1]
      du_stall_o[1]
      du_restart_pc[M.o.operand_width]
      du_restart[1]
      spr_bus_addr[16]
      spr_bus_we[1]
      spr_bus_stb[1]
      spr_bus_dat[M.o.operand_width]
      spr_sr[16]
      ctrl_branch_occur[1]
      rf_we[1]
      ctrl_branch_target[1]
    end

    module Cfgrs = Cfgrs.Make(M)
    module Pic = Pic.Make(M)
    module Timer = Ticktimer.Make(M)
    module Du = Debugunit.Espresso.Make(M)

    let ctrl i = 
      let open I in
      let open HardCaml.Signal.Guarded in
      let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in

      let reset_pc = consti M.o.operand_width M.o.reset_pc in
      let b = i.ctrl_rfb in

      (* wires *)
      let g_reg_rst j = 
          let r = Spr.mor1kx_sr_reset_value.[j:j] in
          R.g_reg ~cv:r ~rv:r ~e:vdd 1 
      in
      let spr_sr = Array.init Spr.mor1kx_sr_width g_reg_rst in
      let spr_sr_v = of_array (Array.map (fun x -> x#q) spr_sr) in
      let spr_esr = Array.init Spr.mor1kx_sr_width g_reg_rst in
      let spr_write_dat = Array.init Spr.mor1kx_sr_width (fun _ -> wire 1) in 
      let spr_write_dat_v = uresize (of_array spr_write_dat) 32 in
      let spr_access_ack = wire 1 in
      let spr_internal_read_dat = wire M.o.operand_width in
      let exception_r = wire 1 in
      let waiting_for_fetch = wire 1 in
      let doing_rfe = wire 1 in
      let doing_rfe_r = wire 1 in
      let delay_slot_rf_we_done = wire 1 in
      let exception_taken = wire 1 in
      let except_ibus_align = wire 1 in
      let except_ticktimer = wire 1 in
      let except_ticktimer_nonsrmasked = wire 1 in
      let except_range = wire 1 in
      let except_pic = wire 1 in
      let except_pic_nonsrmasked = wire 1 in
      let take_exception= wire 1 in
      let rfete = wire 1 in
      let spr_epcr = wire M.o.operand_width in
      let exception_pc_addr = wire M.o.operand_width in
      let flag = wire 1 in
      let fetch_advance = wire 1 in
      let execute_done = wire 1 in
      let execute_waiting = wire 1 in
      let spr_ppc = wire M.o.operand_width in
      let spr_we = wire 1 in
      let spr_npc = wire M.o.operand_width in

      let du = Du.O.(map (fun (_,b) -> wire b) t) in

      let op_mtspr = i.ctrl_opc_insn ==:. Defines.Opcode.mtspr in
      let op_mfspr = i.ctrl_opc_insn ==:. Defines.Opcode.mfspr in
      let op_rfe = i.ctrl_opc_insn ==:. Defines.Opcode.rfe in

      let ctrl_branch_exception = 
        (exception_r |: (op_rfe |: doing_rfe)) &: (~: exception_taken) 
      in
      let exception_pending = 
        (i.except_ibus_err |: except_ibus_align |:
         i.except_illegal |: i.except_syscall |:
         i.except_dbus |: i.except_align |:
         except_ticktimer |: except_range |:
         except_pic |: i.except_trap) 
      in

      let exn = exception_pending in

      let fetch_take_exception_branch =  
        (take_exception |: op_rfe) &: (~: (du.Du.O.stepping)) 
      in

      let execute_stage_exceptions = 
        i.except_dbus |: i.except_align |: except_range 
      in
      let decode_stage_exceptions = i.except_trap |: i.except_illegal in

      let exception_re = exn &: (~: exception_r) &: (~: exception_taken) in

      let ctrl_branch_except_pc = 
        mux2 ((op_rfe |: doing_rfe) &: (~: rfete)) 
          spr_epcr 
          exception_pc_addr 
      in

      (* Exceptions take precedence *)
      let ctrl_branch_occur = 
        (* instruction is branch, and flag is right *)
        (i.op_jbr &:
         (* is l.j or l.jal *)
         ((~: (reduce (|:) @@ bits i.ctrl_opc_insn.[2:1])) |:
          (* is l.bf/bnf and flag is right *)
          (i.ctrl_opc_insn.[2:2] ==: flag))) |:
        (i.op_jr &: (~: (except_ibus_align))) 
      in

      (* Remember when we're in a delay slot in execute stage. *)
      let execute_delay_slot = R.reg_fb ~e:execute_done ~w:1
        (fun execute_delay_slot ->
          mux2 execute_delay_slot gnd ctrl_branch_occur)
      in

      let ctrl_branch_occur_o = 
        (* Usual branch signaling *)
        ((ctrl_branch_occur |: ctrl_branch_exception) &: fetch_advance) |:
        (* Need to tell the fetch stage to branch
           when it gets the next instruction because
           there was fetch stalls between the branch
           and the delay slot insn *)
        (execute_delay_slot) 
      in

      let ctrl_branch_target = 
        mux2 ctrl_branch_exception ctrl_branch_except_pc @@
        (* jump or branch? *)
        mux2 i.op_jbr i.ctrl_alu_result i.ctrl_rfb
      in

      (* Do writeback when we register our output to the next stage, or if we're doing mfspr *)
      let rf_we = 
        (execute_done &: (~: delay_slot_rf_we_done)) &:
        ((i.rf_wb &: (~: op_mfspr) &: 
            (~: ((i.op_lsu_load |: i.op_lsu_store) &:
            i.except_dbus |: i.except_align))) |:
          (op_mfspr)) in

      let () = except_range <== 
        (if M.f.range then
          spr_sr.(Spr.sr_ove)#q &: 
            (spr_sr.(Spr.sr_ov)#q |: i.overflow_set &: execute_done) &: 
            (~: doing_rfe) 
        else 
          gnd)
      in 

      (* Check for unaligned jump address from register *)
      let () = except_ibus_align <== (i.op_jr &: (reduce (|:) @@ bits i.ctrl_rfb.[1:0])) in

      (* Return from exception to exception (if pending tick or PIC ints) *)
      let () = rfete <== 
        ((spr_esr.(Spr.sr_iee)#q &: except_pic_nonsrmasked) |:
         (spr_esr.(Spr.sr_tee)#q &: except_ticktimer_nonsrmasked)) 
      in

      let () = 
        let sel = concat [
          i.except_ibus_err;
          i.except_illegal;
          i.except_align;
          except_ibus_align;
          i.except_syscall;
          i.except_trap;
          i.except_dbus;
          except_range;
          except_pic_nonsrmasked;
          except_ticktimer_nonsrmasked;
        ] in
        let mux l d = 
          let c x = consti M.o.operand_width (x lsl 8) in
          pmux (List.map (fun (b,v) ->  sel.[9:b] ==:. 1, c v) l) (c d) 
        in 
        exception_pc_addr <== R.reg
          ~rv:reset_pc
          ~cv:reset_pc
          ~e:(exception_re |: (rfete &: execute_done))
          (mux [
            9, Defines.berr_vector;
            8, Defines.illegal_vector;
            7, Defines.align_vector;
            6, Defines.align_vector;
            5, Defines.syscall_vector;
            4, Defines.trap_vector;
            3, Defines.berr_vector;
            2, Defines.range_vector;
            1, Defines.int_vector;
          ] Defines.tt_vector)
      in

      (*let waiting_for_except_fetch = R.reg_fb ~e:vdd ~w:1
        (fun waiting_for_except_fetch -> pmux [
          waiting_for_except_fetch &: i.next_fetch_done, gnd;
          fetch_take_exception_branch, vdd;
        ] waiting_for_except_fetch)
      in*)
      
      let () = fetch_advance <== 
        ((i.next_fetch_done |: i.except_ibus_err) &:
        (~: execute_waiting) &: (~: (du.Du.O.cpu_stall)) &:
        ((~: (du.Du.O.stepping)) |:
        (du.Du.O.stepping &: du.Du.O.pstep.[0:0] &: (~: (i.next_fetch_done)))))
      in

      let padv_fetch = 
        fetch_advance &: (~: exception_pending) &: (~: doing_rfe_r) &: (~: (du.Du.O.cpu_stall))
      in

      let () = take_exception <== R.reg ~e:vdd
        ((exception_pending |: exception_r |: doing_rfe_r) &:
        (fetch_advance |:
          (* Cause exception to always be 'taken' if stepping *)
          (du.Du.O.stepping &: execute_done)) &:
        (* Would like this as only a single pulse *)
        (~: take_exception))
      in

      let padv_decode = R.reg ~e:vdd padv_fetch in

      let execute_go = R.reg ~e:vdd 
        (padv_fetch |: execute_waiting |: (du.Du.O.stepping &: i.next_fetch_done)) 
      in

      let () = execute_done <== (execute_go &: (~: execute_waiting)) in

      (* ALU or LSU stall execution, nothing else can *)
      let execute_valid = 
        (~: ((i.op_lsu_load |: i.op_lsu_store) &: 
        (~: (i.lsu_valid)) |: (~: (i.alu_valid)))) 
      in

      let () = execute_waiting <== ((~: execute_valid)  &: (~: waiting_for_fetch)) in

      let padv_execute = execute_done in

      let spr_addr = mux2 du.Du.O.du_access i.du_addr i.ctrl_alu_result.[15:0] in
      let ctrl_mfspr_we = op_mfspr &: execute_go in
      
      (* Pipeline flush *)
      let pipeline_flush = (execute_done &: op_rfe) |:
                           exception_re |: du.Du.O.cpu_stall
      in

      (* Flag *)
      let () = flag <== R.reg ~e:execute_done 
        (mux2 i.ctrl_flag_clear gnd @@
         mux2 i.ctrl_flag_set vdd flag)
      in

      let execute_waiting_r = R.reg ~e:vdd execute_waiting in


      (*let decode_execute_halt = R.reg_fb ~e:vdd ~w:1
        (fun decode_execute_halt -> 
          let deassert_decode_execute_halt = ctrl_branch_occur &: decode_execute_halt in
          pmux [
            du.Du.O.du_restart_from_stall, gnd;
            decode_execute_halt &: deassert_decode_execute_halt, gnd;
            ((op_rfe |: exn) &: (~: decode_execute_halt) &: (~: exception_taken)), gnd;
          ] decode_execute_halt)
      in*)

      let () = exception_r <== R.reg_fb ~e:vdd ~w:1
        (pmux [
          (exception_taken |: du.Du.O.du_restart_from_stall), gnd;
          (exn &: (~: exception_r)), vdd;
        ])
      in
      
      let () = exception_taken <== R.reg_fb ~e:vdd ~w:1
        (fun exception_taken -> pmux [
          exception_taken, gnd;
          exception_r &: take_exception, vdd;
        ] exception_taken)
      in

      (*let last_branch_insn_pc = R.reg ~e:(fetch_advance &: ctrl_branch_occur) spr_ppc in*)
      let last_branch_target_pc = 
        R.reg ~e:(execute_done &: ctrl_branch_occur &: du.Du.O.stepping) ctrl_branch_target
      in

      let () = waiting_for_fetch <== R.reg_fb ~e:vdd ~w:1
        (pmux [
          i.next_fetch_done, gnd;
          ((~: execute_waiting) &: execute_waiting_r &: (~: (i.next_fetch_done))), vdd;
        ])
      in

      let branched_and_waiting_for_fetch = R.reg_fb ~e:vdd ~w:1
        (fun branched_and_waiting_for_fetch -> pmux [
          exception_re, gnd;
          (padv_fetch &: ctrl_branch_occur_o), vdd;
          branched_and_waiting_for_fetch, ~: (i.next_fetch_done);
        ] branched_and_waiting_for_fetch)
      in

      (* Basically, the fetch stage should always take the rfe immediately *)
      let deassert_doing_rfe =  doing_rfe_r in

      let () = 
        doing_rfe <== (((execute_done &: op_rfe) |: doing_rfe_r) &: (~: deassert_doing_rfe))
      in

      let () = doing_rfe_r <== R.reg_fb ~e:vdd ~w:1
        (pmux [
          deassert_doing_rfe, gnd;
          execute_done, op_rfe;
        ])
      in

      let gen_sr =
        let cond = spr_we &: (spr_addr ==: Spr.Sys.(const sr)) in
        fun ?(b=false) ?(f=false) ?d0 ?d1 j -> 
          let f = if f then vdd else gnd in
          compile [
            g_if fetch_take_exception_branch [
              g_if (op_rfe &: (~: rfete)) [
                spr_sr.(j) $== spr_esr.(j)#q;
              ] [
                g_proc (match d0 with
                        | Some(d0) -> [ g_when f [ spr_sr.(j) $== d0 ] ]
                        | None -> [])
              ]
            ] @@ g_elif execute_done [
              g_proc (match d1 with
                      | Some(d1) -> [ g_when f [ spr_sr.(j) $== d1 ] ]
                      | None -> []);
              g_when ((spr_sr.(j)#q |: du.Du.O.du_access) &: cond) [ 
                g_when f [
                  spr_sr.(j) $== spr_write_dat.(j)
                ]
              ]
            ] [];
            
            (* special case;
              Need to check for DSX being set on exception entry on execute_done
              as the delay slot information is gone after it goes high *)
            g_proc 
              (if b then 
                [ g_when (f &: (exception_r ||: exception_re)) [ spr_sr.(j) $== execute_delay_slot ] ]
              else 
                [])
          ] 
      in

      let () = 
        let d1 = mux2 i.ctrl_flag_set vdd @@
                 mux2 i.ctrl_flag_clear gnd @@
                 spr_sr.(Spr.sr_f)#q in
        gen_sr ~d1 Spr.sr_f;
        let d1 = mux2 i.carry_set vdd @@
                 mux2 i.carry_clear gnd @@
                 spr_sr.(Spr.sr_cy)#q in
        gen_sr ~d1 Spr.sr_cy;
        let d1 = mux2 i.overflow_set vdd @@
                 mux2 i.overflow_clear gnd @@
                 spr_sr.(Spr.sr_ov)#q in
        gen_sr ~f:M.f.overflow ~d1 Spr.sr_ov;

        gen_sr ~d0:vdd Spr.sr_sm;
        gen_sr ~f:M.f.timer ~d0:gnd Spr.sr_tee;
        gen_sr ~f:M.f.pic ~d0:gnd Spr.sr_iee;
        gen_sr ~f:M.f.dmmu ~d0:gnd Spr.sr_dme;
        gen_sr ~f:M.f.immu ~d0:gnd Spr.sr_ime;
        gen_sr ~f:M.f.overflow ~d0:gnd Spr.sr_ove;

        gen_sr ~f:M.f.datacache Spr.sr_dce;
        gen_sr ~f:M.f.instructioncache Spr.sr_ice;
        gen_sr ~f:M.f.fastcontexts Spr.sr_ce;
        gen_sr ~b:true ~f:M.f.dsx Spr.sr_dsx;
        gen_sr Spr.sr_eph;
        compile [ spr_sr.(Spr.sr_lee) $==. 1 ]; (* little endian enable *)
        compile [ spr_sr.(Spr.sr_fo) $==. 1 ]; (* fixed to one *)
      in

      let () = compile [
        g_if (exception_re) [
          g_proc (Array.to_list @@ Array.init Spr.mor1kx_sr_width
            (fun j -> spr_esr.(j) $== spr_sr.(j)#q));
          (*
           A bit odd, but if we had a l.sf instruction on an exception rising
           edge, EPCR will point to the insn past the l.sf but the flag will
           not have been saved to the SR properly. So we must put it in here
           so it can be restored correctly.
           Ditto for the other flags which may have been changed in a similar
           fashion.
           *)
          g_when (execute_done) [
            g_if (i.ctrl_flag_set) [
              spr_esr.(Spr.sr_f) $==. 1;
            ] @@ g_elif (i.ctrl_flag_clear) [
              spr_esr.(Spr.sr_f) $==. 0;
            ] [];
            g_proc (if M.f.overflow then [
              g_if (i.overflow_set) [
                spr_esr.(Spr.sr_ov) $==. 1;
              ] @@ g_elif (i.overflow_clear) [
                spr_esr.(Spr.sr_ov) $==. 0;
              ] []
            ] else []);
            g_if (i.carry_set) [
              spr_esr.(Spr.sr_cy) $==. 1;
            ] @@ g_elif (i.carry_clear) [
              spr_esr.(Spr.sr_cy) $==. 0;
            ] []
          ]
        ] @@ g_elif (spr_we &: (spr_addr ==: Spr.Sys.(const esr0))) [
          g_proc (Array.to_list @@ Array.init Spr.mor1kx_sr_width
            (fun j -> spr_esr.(j) $== spr_write_dat.(j)));
        ] []
      ] in

      (* Exception PC *)
      let () = spr_epcr <== R.reg_fb ~rv:reset_pc ~cv:reset_pc ~e:vdd ~w:M.o.operand_width
        (fun spr_epcr -> pmux [
          (exception_re &: (~: (rfete &: (op_rfe |: deassert_doing_rfe)))),
            (pmux [
              i.except_ibus_err, spr_ppc -:. 4;
              (* EPCR after syscall is address of next not executed insn. *)
              i.except_syscall, spr_npc;
              (except_ticktimer |: except_pic),
                mux2 branched_and_waiting_for_fetch spr_npc @@
                mux2 execute_delay_slot (spr_ppc -:. 4) (spr_ppc +:. 4);
              (* Don't update EPCR on software breakpoint *)
              (execute_stage_exceptions |:
                (decode_stage_exceptions &: (~: (du.Du.O.stall_on_trap &: i.except_trap)))),
                mux2 execute_delay_slot (spr_ppc -:. 4) spr_ppc;
              (~: (du.Du.O.stall_on_trap &: i.except_trap)),
                mux2 execute_delay_slot (spr_ppc -:. 4) spr_ppc;
            ] spr_epcr);
          (spr_we &: (spr_addr ==: Spr.Sys.(const epcr0))), spr_write_dat_v;
        ] spr_epcr)
      in

      (* Exception Effective Address *)
      let spr_eear = R.reg ~e:exception_re
          (mux2 i.except_ibus_err i.pc_fetch i.ctrl_alu_result)
      in

      (* Next PC (NPC) *)
      let () = spr_npc <== R.reg_fb ~rv:reset_pc ~cv:reset_pc ~e:vdd ~w:M.o.operand_width
        (pmux [
          deassert_doing_rfe, mux2 rfete exception_pc_addr spr_epcr;
          du.Du.O.du_restart, du.Du.O.du_restart_pc;
          (du.Du.O.stepping &: i.next_fetch_done), 
            mux2 execute_delay_slot last_branch_target_pc i.pc_fetch;
          (du.Du.O.stepping &: exception_r), exception_pc_addr;
          (* PC we're now executing *)
          fetch_advance,
            mux2 fetch_take_exception_branch exception_pc_addr @@
            mux2 ctrl_branch_occur ctrl_branch_target i.pc_fetch;
        ])
      in

      (* Previous PC (PPC) *)
      let () = spr_ppc <== R.reg ~rv:reset_pc ~cv:reset_pc 
        (* PC we've got in execute stage (about to finish) *)
        ~e:(padv_fetch |: (du.Du.O.stepping &: i.next_fetch_done))
        spr_npc 
      in

      let () = delay_slot_rf_we_done <== R.reg ~e:vdd (rf_we &: execute_delay_slot) in

      let cfgrs = Cfgrs.cfgrs_inst Cfgrs.I.(map (fun _ -> gnd) t) in

      let spr_isr = Array.init 8 (fun _ -> zero 32) in

      let spr_sys_group_read = 
        cases spr_addr 
          (mux2 ((spr_addr >=: Spr.Sys.(const gpr0)) &:
                 (spr_addr <=: Spr.Sys.(const (gpr0+32)))) (* XXX 31? or strictly less than? *)
            b
            (zero M.o.operand_width))
          [
            Spr.Sys.vr, cfgrs.Cfgrs.O.vr;
            Spr.Sys.vr2, cfgrs.Cfgrs.O.vr2.[31:8] @: consti 8 Defines.mor1kx_pipeid_espresso;
            Spr.Sys.avr, cfgrs.Cfgrs.O.avr;
            Spr.Sys.upr, cfgrs.Cfgrs.O.upr;
            Spr.Sys.cpucfgr, cfgrs.Cfgrs.O.cpucfgr;
            Spr.Sys.dmmucfgr, cfgrs.Cfgrs.O.dmmucfgr;
            Spr.Sys.immucfgr, cfgrs.Cfgrs.O.immucfgr;
            Spr.Sys.dccfgr, cfgrs.Cfgrs.O.dccfgr;
            Spr.Sys.iccfgr, cfgrs.Cfgrs.O.iccfgr;
            Spr.Sys.dcfgr, cfgrs.Cfgrs.O.dcfgr;
            Spr.Sys.pccfgr, cfgrs.Cfgrs.O.pccfgr;
            Spr.Sys.npc, spr_npc;
            Spr.Sys.sr, uresize (of_array @@ Array.map (fun d -> d#q) spr_sr) M.o.operand_width;
            Spr.Sys.ppc, spr_ppc;
            Spr.Sys.fpcsr, cfgrs.Cfgrs.O.fpcsr;
            Spr.Sys.epcr0, spr_epcr;
            Spr.Sys.eear0, spr_eear;
            Spr.Sys.esr0, uresize (of_array @@ Array.map (fun d -> d#q) spr_esr) M.o.operand_width;
            Spr.Sys.isr0+0, spr_isr.(0);
            Spr.Sys.isr0+1, spr_isr.(1);
            Spr.Sys.isr0+2, spr_isr.(2);
            Spr.Sys.isr0+3, spr_isr.(3);
            Spr.Sys.isr0+4, spr_isr.(4);
            Spr.Sys.isr0+5, spr_isr.(5);
            Spr.Sys.isr0+6, spr_isr.(6);
            Spr.Sys.isr0+7, spr_isr.(7);
            (* If the multicore feature is activated this address returns the
               core identifier, 0 otherwise *)
            Spr.Sys.coreid, 
              if M.f.multicore then i.multicore_coreid else zero M.o.operand_width;
          ]
      in

      let pic = 
        if M.f.pic then 
          let pic = Pic.pic_inst
            Pic.I.{
              clk = i.clk;
              rst = i.rst;
              irq = i.irq;
              spr_access = gnd; (* XXX ??? *)
              spr_we;
              spr_addr;
              spr_dat_i = spr_write_dat_v;
            }
          in
          let () = except_pic_nonsrmasked <== 
            ((reduce (|:) @@ bits pic.Pic.O.spr_picsr) &:
            (~: op_mtspr) &:
            (* Stops back-to-back branch addresses going to fetch stage *)
            (~: ctrl_branch_occur) &:
            (* Stops issues with PC when branching *)
            (~: execute_delay_slot))
          in
          let () = except_pic <== 
            (spr_sr.(Spr.sr_iee)#q &: except_pic_nonsrmasked &: (~: doing_rfe))
          in
          pic
        else
          let () = except_pic_nonsrmasked <== gnd in
          let () = except_pic <== gnd in
          Pic.O.(map (fun (_,b) -> zero b) t)
      in

      let timer = 
        if M.f.timer then
          let timer = Timer.ticktimer_inst
            Timer.I.{
              clk = i.clk;
              rst = i.rst;
              spr_access = gnd; (* XXX ??? *)
              spr_we;
              spr_addr;
              spr_dat_i = spr_write_dat_v;
            }
          in
          let () = except_ticktimer_nonsrmasked <== 
            (timer.Timer.O.spr_ttmr.[28:28] &:
            ((~: op_mtspr) &: (~: (spr_esr.(Spr.sr_tee)#q &: execute_done))) &:
            (* Stops back-to-back branch addresses to fetch stage. *)
            (~: ctrl_branch_occur) &:
            (* Stops issues with PC when branching *)
            (~: execute_delay_slot))
          in
          let () = except_ticktimer <== 
            (except_ticktimer_nonsrmasked &:
            spr_sr.(Spr.sr_tee)#q &: (~: doing_rfe));
          in
          timer
        else
          let timer = Timer.O.(map (fun (_,b) -> zero b) t) in
          let () = except_ticktimer_nonsrmasked <== gnd in
          let () = except_ticktimer <== gnd in
          timer
      in

      (* SPR access control - allow accesses from either the instructions or from
       the debug interface *)
      let spr_read_access = (op_mfspr |: (du.Du.O.du_access &: (~: (i.du_we)))) in
      let spr_write_access = ((execute_done &: op_mtspr) |: (du.Du.O.du_access &: i.du_we)) in
   
      (* Is the SPR in the design? *)
      let spr_group_present = 
        let spr_addr = spr_addr.[15:11] in
        snd @@ List.fold_left 
          (fun (j,a) b -> j+1, (if b then a |: (spr_addr ==:. j) else a))
          (0,gnd)
          M.([
            true; f.dmmu; f.immu; f.datacache; f.instructioncache; f.mac; 
            f.debugunit; f.perfcounters; f.pmu; f.pic; f.timer; f.fpu;
          ])
      in

      let spr_group = mux2 spr_group_present spr_addr.[14:11] (consti 4 12) in

      let spr_write_dat =
        Array.iteri 
          (fun j q -> q <== mux2 du.Du.O.du_access i.du_dat_i.[j:j] b.[j:j]) 
          spr_write_dat;
        of_array spr_write_dat
      in
      let () = spr_we <== (spr_write_access &: spr_group_present) in
      (*let spr_read = spr_read_access &: spr_group_present in*)
   
      (* Is a SPR bus access needed, or is the requested SPR in this file? *)
      let spr_bus_access = 
        (* Any of the units we don't have in this file *)
        (* System group *)
        ~: ((spr_addr.[15:11] ==:. 0x0) |:
            (* Debug Group *)
            (spr_addr.[15:11] ==:. 0x6) |:
            (* PIC Group *)
            (spr_addr.[15:11] ==:. 0x9) |:
            (* Tick Group *)
            (spr_addr.[15:11] ==:. 0xa))
      in

      let _ = 
        if M.f.debugunit then
          let du' = Du.debugunit_inst
            Du.I.{
              clk = i.clk;
              rst = i.rst;
              except_trap = i.except_trap;
              next_fetch_done = i.next_fetch_done;
              execute_done;
              execute_delay_slot;
              last_branch_target_pc;
              ctrl_branch_occur;
              du_stb_i = i.du_stb;
              du_stall_i = i.du_stall_i;
              du_we = i.du_we;
              du_addr = i.du_addr;
              du_dat_i = i.du_dat_i;
              op_rfe;
              spr_epcr;
              spr_group_present;
              spr_access_ack_i = spr_access_ack;
              spr_internal_read_dat_i = spr_internal_read_dat;
              spr_npc;
              spr_addr;
              spr_we;
              spr_write_dat;
            }
          in
          Du.O.(map2 (fun q d -> q <== d) du du')
        else
          Du.O.(map2 (fun q (_,b) -> q <== zero b) du t)
      in

      let mk_spr_grp f ack dat = if f then ack,dat else gnd, zero 32 in

      let () = 
        let l = [
        (*  0 *) vdd, spr_sys_group_read;
        (*  1 *) mk_spr_grp M.f.dmmu i.spr_bus_ack_dmmu i.spr_bus_dat_dmmu;
        (*  2 *) mk_spr_grp M.f.immu i.spr_bus_ack_immu i.spr_bus_dat_immu;
        (*  3 *) mk_spr_grp M.f.datacache i.spr_bus_ack_dc i.spr_bus_dat_dc;
        (*  4 *) mk_spr_grp M.f.instructioncache i.spr_bus_ack_ic i.spr_bus_dat_ic;
        (*  5 *) mk_spr_grp M.f.mac i.spr_bus_ack_mac i.spr_bus_dat_mac;
        (*  6 *) Du.O.(du.spr_access_ack_o, du.spr_internal_read_dat_o);
        (*  7 *) mk_spr_grp M.f.perfcounters i.spr_bus_ack_pcu i.spr_bus_dat_pcu;
        (*  8 *) mk_spr_grp M.f.pmu i.spr_bus_ack_pmu i.spr_bus_dat_pmu;
        (*  9 *) pic.Pic.O.spr_bus_ack, pic.Pic.O.spr_dat_o;
        (* 10 *) timer.Timer.O.spr_bus_ack, timer.Timer.O.spr_dat_o;
        (* 11 *) mk_spr_grp M.f.fpu i.spr_bus_ack_fpu i.spr_bus_dat_fpu;
        (* 12 *) gnd, zero 32;
        ] in
        let ack, dat = List.map fst l, List.map snd l in
        spr_access_ack <== mux spr_group ack;
        spr_internal_read_dat <== mux spr_group dat
      in

      (* Generate data to the register file for mfspr operations *)
      let mfspr_dat = spr_internal_read_dat in
      (* A bus out to other units that live outside of the control unit *)
      let spr_bus_addr = spr_addr in
      let spr_bus_we = spr_write_access &: spr_group_present &: spr_bus_access in
      let spr_bus_stb = (spr_read_access |: spr_write_access) &:
                             spr_group_present &: spr_bus_access in
      let spr_bus_dat = spr_write_dat in
      let carry = spr_sr.(Spr.sr_cy)#q in

      O.{
        flag;
        spr_npc;
        spr_ppc;
        mfspr_dat;
        ctrl_mfspr_we;
        carry;
        pipeline_flush;
        padv_fetch;
        padv_decode;
        padv_execute;
        fetch_take_exception_branch;
        exception_taken;
        execute_waiting;
        stepping = du.Du.O.stepping;
        du_dat_o = du.Du.O.du_dat_o;
        du_ack = du.Du.O.du_ack;
        du_stall_o = du.Du.O.du_stall_o;
        du_restart_pc = du.Du.O.du_restart_pc;
        du_restart = du.Du.O.du_restart;
        spr_bus_addr;
        spr_bus_we;
        spr_bus_stb;
        spr_bus_dat;
        spr_sr = spr_sr_v;
        ctrl_branch_occur = ctrl_branch_occur_o;
        rf_we;
        ctrl_branch_target;
      }

    module Inst = M.Inst(I)(O)
    let ctrl_inst = Inst.inst "ctrl" ctrl
  
  end

end

