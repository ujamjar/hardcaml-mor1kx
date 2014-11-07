(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Description: mor1kx decode unit

  Completely combinatorial.

  Outputs:
   - ALU operation
   - indication of other type of op - LSU/SPR
   - immediates
   - register file addresses
   - exception decodes:  illegal, system call

  Copyright (C) 2012 Julius Baxter <juliusbaxter@gmail.com>
  Copyright (C) 2013 Stefan Kristiansson <stefan.kristiansson@saunalahti.fi>
  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

open HardCaml.Signal.Comb
open Defines
open Utils
open Option

(* XXX Option.options...functorise? *)
let rf_addr_width = 5
let operand_width = 32

module I = interface
  insn[insn_width]
end

module O = interface
  opc_alu[Alu_opc.width] opc_alu_secondary[Alu_opc.width]
  imm16[imm_width] immediate[operand_width] immediate_sel[1]
  immjbr_upper[10]
  rfd_adr[rf_addr_width] rfa_adr[rf_addr_width] rfb_adr[rf_addr_width] rf_wb[1]
  op_jbr[1] op_jr[1] op_jal[1] op_bf[1] op_bnf[1] op_brcond[1] op_branch[1]
  op_alu[1] op_lsu_load[1] op_lsu_store[1] op_lsu_atomic[1]
  lsu_length[2] lsu_zext[1]
  op_mfspr[1] op_mtspr[1] op_rfe[1] op_setflag[1] op_add[1]
  op_mul[1] op_mul_signed[1] op_mul_unsigned[1]
  op_div[1] op_div_signed[1] op_div_unsigned[1]
  op_shift[1] op_ffl1[1]
  op_movhi[1]
  adder_do_sub[1] adder_do_carry[1]
  except_illegal[1] except_syscall[1] except_trap[1]
  opc_insn[Opcode.width]
end

let expand insn select =
  let opc = sel insn select in
  opc, (binary_to_onehot opc |> bits |> List.rev |> Array.of_list)

let alu_opc_secondary_invalid f = 
  let a = Array.init (1 lsl Alu_opc.secondary_width) (fun _ -> vdd) in
  let b x = if x then gnd else vdd in
  let open Alu_opc in
  List.iter (fun i -> a.(i) <- gnd) [ secondary_shrt_sll; secondary_shrt_srl ];
  a.(secondary_shrt_sra) <- b f.sra;
  a.(secondary_shrt_ror) <- b f.ror;
  a

let alu_opc_invalid f secondary_invalid = 
  let a = Array.init (1 lsl Alu_opc.width) (fun _ -> vdd) in
  let b x = if x then gnd else vdd in
  let open Alu_opc in
  List.iter (fun i -> a.(i) <- gnd) [ add; sub; _or; xor; _and ];
  a.(cmov) <- b f.cmov;
  a.(ffl1) <- b (f.ffl1 = Ffl1_none);
  a.(div) <- b (f.divider = Divider_none);
  a.(divu) <- b (f.divider = Divider_none);
  a.(addc) <- b f.addc;
  a.(mul) <- b (f.multiplier = Multiplier_none);
  a.(mulu) <- b (f.multiplier = Multiplier_none);
  a.(extbh) <- b f.ext;
  a.(extw) <- b f.ext;
  a.(shrt) <- secondary_invalid;
  a

let systrap_opc_invalid f = 
  let a = Array.init (1 lsl systrapsync_opc_width) (fun _ -> vdd) in
  let b x = if x then gnd else vdd in
  a.(systrapsync_opc_syscall) <- b f.syscall;
  a.(systrapsync_opc_trap) <- b f.trap;
  a.(systrapsync_opc_msync) <- b f.msync; (* XXX mistake in rtl ??? *)
  a.(systrapsync_opc_psync) <- b f.psync;
  a.(systrapsync_opc_csync) <- b f.csync;
  a

let opc_invalid f alu_invalid sys_invalid = 
  let a = Array.init (1 lsl Opcode.width) (fun _ -> vdd) in
  let b x = if x then gnd else vdd in
  let open Opcode in
  List.iter (fun i -> a.(i) <- gnd) [ 
    j; jal; bnf; bf; movhi; rfe; jr; jalr; lwz; lws; lbz; lbs; lhz; lhs;
    addi; andi; ori; xori; mfspr; sfimm; mtspr; sw; sb; sh; sf; nop;
  ];
  a.(swa) <- b f.atomic;
  a.(lwa) <- b f.atomic;
  a.(cust1) <- b f.cust1;
  a.(cust2) <- b f.cust2;
  a.(cust3) <- b f.cust3;
  a.(cust4) <- b f.cust4;
  a.(cust5) <- b f.cust5;
  a.(cust6) <- b f.cust6;
  a.(cust7) <- b f.cust7;
  a.(cust8) <- b f.cust8;
  a.(ld) <- b (not (operand_width = 64));
  a.(sd) <- b (not (operand_width = 64));
  a.(addic) <- b f.addc;
  a.(maci) <- b f.mac;
  a.(mac) <- b f.mac;
  a.(muli) <- b (f.multiplier = Multiplier_none);
  a.(alu) <- alu_invalid;
  a.(systrapsync) <- sys_invalid;
  a

let decode_op_lsu_load f insn opc =
  let c = 
    (select insn 31 30 ==:. 0b10) &:
    (~: (reduce (&:) (bits (select insn 28 26)))) &:
    (~: (bit insn 29))
  in
  if f.atomic then c |: (opc.(Opcode.lwa)) else c

let decode_op_lsu_store f opc = 
  let c = Opcode.(opc.(sw) |: opc.(sb) |: opc.(sh)) in
  if f.atomic then c |: opc.(Opcode.swa) else c

let decode_op_lsu_atomic f opc = 
  if f.atomic then Opcode.(opc.(lwa) |: opc.(swa)) else gnd

let opcl opc l = reduce (|:) (List.map (Array.get opc) l)

let decode_lsu_length opc = 
  mux2 (opcl opc Opcode.([ sb; lbz; lbs ])) (consti 2 0b00)
    (mux2 (opcl opc Opcode.([ sh; lhz; lhs ])) (consti 2 0b01) 
      (consti 2 0b10))

let decode o f i = 
  let open I in
  let opc_insn, opc = expand i.insn Opcode.select in
  let op_lsu_load = decode_op_lsu_load f i.insn opc in
  let op_lsu_store = decode_op_lsu_store f opc in
  let op_lsu_atomic = decode_op_lsu_atomic f opc in
  let lsu_length = decode_lsu_length opc in
  let lsu_zext = bit opc_insn 0 in

  let op_mtspr = opc.(Opcode.mtspr) in
  let op_setflag = opcl opc Opcode.([ sf; sfimm ]) in
  let op_alu = opcl opc Opcode.([ alu; ori; andi; xori ]) in

  let op_jbr = opc_insn <:. Opcode.nop in
  let op_jr = opcl opc Opcode.([ jr; jalr ]) in
  let op_jal = opcl opc Opcode.([ jalr; jal ]) in
  let op_bf = opc.(Opcode.bf) in
  let op_bnf = opc.(Opcode.bnf) in
  let op_brcond = op_bf |: op_bnf in
  let op_branch = op_jbr |: op_jr |: op_jal in
  let op_mfspr = opc.(Opcode.mfspr) in
  let op_rfe = opc.(Opcode.rfe) in

  let opc_alu, aluop = expand i.insn Alu_opc.select in
  let op_add = Opcode.((opc.(alu) &: (opcl aluop Alu_opc.([addc; add; sub]))) |: 
                        opc.(addic) |: opc.(addi))
  in
  let op_mul_signed = Opcode.((opc.(alu) &: aluop.(Alu_opc.mul)) |: opc.(muli)) in
  let op_mul_unsigned = opc.(Opcode.alu) &: aluop.(Alu_opc.mulu) in
  let op_mul = op_mul_unsigned |: op_mul_signed in
  let op_div_signed = opc.(Opcode.alu) &: aluop.(Alu_opc.div) in
  let op_div_unsigned = opc.(Opcode.alu) &: aluop.(Alu_opc.divu) in
  let op_div = op_div_signed |: op_div_unsigned in
  let op_shift = Opcode.((opc.(alu) &: aluop.(Alu_opc.shrt)) |: opc.(shrti)) in
  let op_ffl1 = opc.(Opcode.alu) &: aluop.(Alu_opc.ffl1) in
  let op_movhi = opc.(Opcode.movhi) in

  let rf_wb = Opcode.(opcl opc [ jal; movhi; jalr; lwa ]) |:
                      (select i.insn 31 30 ==:. 0b10 &: (~: (opc.(Opcode.sfimm)))) |:
                      (select i.insn 31 30 ==:. 0b11 &: 
                          (~: (opc.(Opcode.sf) |: op_mtspr |: op_lsu_store)))
  in
  let rfa_adr = sel i.insn ra_select in
  let rfb_adr = sel i.insn rb_select in
  let rfd_adr = sel i.insn rd_select in

  let imm16 = mux2 (op_mtspr |: op_lsu_store) 
    (select i.insn 25 21 @: select i.insn 10 0)
    (sel i.insn imm_select)
  in
  let immjbr_upper = select i.insn 25 16 in
  let imm_sext = sresize imm16 32 in
  let imm_sext_sel = 
    (select opc_insn 5 4 ==:. 0b10 &: 
      (~: (opc.(Opcode.ori))) &: (~: (opc.(Opcode.andi)))) |:
    (opcl opc Opcode.([swa; lwa; sw; sh; sb]))
  in
  let imm_zext = uresize imm16 32 in
  let imm_zext_sel = 
    (select opc_insn 5 4 ==:. 0b10 &: 
      (opc.(Opcode.ori) |: opc.(Opcode.andi))) |:
    (opc.(Opcode.mtspr))
  in
  let imm_high = imm16 @: zero 16 in
  let imm_high_sel = op_movhi in
  let immediate = 
    mux2 imm_sext_sel imm_sext
      (mux2 imm_zext_sel imm_zext imm_high)
  in
  let immediate_sel = imm_sext_sel |: imm_zext_sel |: imm_high_sel in
  let opc_alu = 
    mux2 opc.(Opcode.ori) (consti Alu_opc.width Alu_opc._or)
      (mux2 opc.(Opcode.andi) (consti Alu_opc.width Alu_opc._and)
        (mux2 opc.(Opcode.xori) (consti Alu_opc.width Alu_opc.xor)
          opc_alu))
  in

  let opc_alu_secondary, secondary = expand i.insn Alu_opc.secondary_select in
  let opc_alu_secondary = 
    mux2 op_setflag
      (sel i.insn comp_opc_select)
      (ue opc_alu_secondary)
  in

  let opc_systrapsync, systrapsync = expand i.insn systrapsync_opc_select in
  let except_syscall = opc.(Opcode.systrapsync) &: systrapsync.(systrapsync_opc_syscall) in
  let except_trap = opc.(Opcode.systrapsync) &: systrapsync.(systrapsync_opc_trap) in

  (* detect invalid instruction *)
  let is_invalid_op iv op = 
    Array.init (Array.length iv) (fun i -> iv.(i) &: op.(i)) |> 
      Array.to_list |> reduce (|:)
  in
  let invalid_sys = is_invalid_op (systrap_opc_invalid f) systrapsync in
  let invalid_alu_secondary = is_invalid_op (alu_opc_secondary_invalid f) secondary in
  let invalid_alu_op = is_invalid_op (alu_opc_invalid f invalid_alu_secondary) aluop in
  let except_illegal = is_invalid_op (opc_invalid f invalid_alu_op invalid_sys) opc in
  let adder_do_sub = (opc.(Opcode.alu) &: aluop.(Alu_opc.sub)) |: op_setflag in
  let adder_do_carry = 
    if f.addc then
      (opc.(Opcode.alu) &: aluop.(Alu_opc.addc)) |: opc.(Opcode.addic)
    else gnd
  in

  O.({
    opc_alu; opc_alu_secondary;
    imm16; immediate; immediate_sel;
    immjbr_upper;
    rfd_adr; rfa_adr; rfb_adr; rf_wb;
    op_jbr; op_jr; op_jal; op_bf; op_bnf; op_brcond; op_branch;
    op_alu; op_lsu_load; op_lsu_store; op_lsu_atomic;
    lsu_length; lsu_zext;
    op_mfspr; op_mtspr; op_rfe; op_setflag; op_add;
    op_mul; op_mul_signed; op_mul_unsigned;
    op_div; op_div_signed; op_div_unsigned;
    op_shift; op_ffl1;
    op_movhi;
    adder_do_sub; adder_do_carry;
    except_illegal; except_syscall; except_trap;
    opc_insn;
  })

