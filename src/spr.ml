(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Description: SPR definitions

  Copyright (C) 2012 Authors

  Author(s): Julius Baxter <juliusbaxter@gmail.com>
             Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

open HardCaml.Signal.Comb

let base x = x.[15:11]
let offset x = x.[10:0]

(* Addresses *)
let sys_base       = consti 5 0
let vr_addr        = sys_base @: consti 11 0
let upr_addr       = sys_base @: consti 11 1
let cpucfgr_addr   = sys_base @: consti 11 2
let dmmucfgr_addr  = sys_base @: consti 11 3
let immucfgr_addr  = sys_base @: consti 11 4
let dccfgr_addr    = sys_base @: consti 11 5
let iccfgr_addr    = sys_base @: consti 11 6
let dcfgr_addr     = sys_base @: consti 11 7
let pccfgr_addr    = sys_base @: consti 11 8
let vr2_addr       = sys_base @: consti 11 9
let avr_addr       = sys_base @: consti 11 10
let evbar_addr     = sys_base @: consti 11 11
let aecr_addr      = sys_base @: consti 11 12
let aesr_addr      = sys_base @: consti 11 13
let npc_addr       = sys_base @: consti 11 16
let sr_addr        = sys_base @: consti 11 17
let ppc_addr       = sys_base @: consti 11 18
let fpcsr_addr     = sys_base @: consti 11 20
let isr0_addr      = sys_base @: consti 11 21
let epcr0_addr     = sys_base @: consti 11 32
let eear0_addr     = sys_base @: consti 11 48
let esr0_addr      = sys_base @: consti 11 64
let coreid_addr    = sys_base @: consti 11 128
let numcores_addr  = sys_base @: consti 11 129
let gpr0_addr      = sys_base @: consti 11 1024

let dmmu_base      = consti 5 1
let dmmucr_addr    = dmmu_base @: consti 11 0
let dmmupr_addr    = dmmu_base @: consti 11 1
let dtlbeir_addr   = dmmu_base @: consti 11 2
let datbmr0_addr   = dmmu_base @: consti 11 4
let datbtr0_addr   = dmmu_base @: consti 11 8
let dtlbw0mr0_addr = dmmu_base @: consti 11 512
let dtlbw0tr0_addr = dmmu_base @: consti 11 640
let dtlbw1mr0_addr = dmmu_base @: consti 11 768
let dtlbw1tr0_addr = dmmu_base @: consti 11 896
let dtlbw2mr0_addr = dmmu_base @: consti 11 1024
let dtlbw2tr0_addr = dmmu_base @: consti 11 1152
let dtlbw3mr0_addr = dmmu_base @: consti 11 1280
let dtlbw3tr0_addr = dmmu_base @: consti 11 1408

let immu_base      = consti 5 2
let immucr_addr    = immu_base @: consti 11 0
let immupr_addr    = immu_base @: consti 11 1
let itlbeir_addr   = immu_base @: consti 11 2
let iatbmr0_addr   = immu_base @: consti 11 4
let iatbtr0_addr   = immu_base @: consti 11 8
let itlbw0mr0_addr = immu_base @: consti 11 512
let itlbw0tr0_addr = immu_base @: consti 11 640
let itlbw1mr0_addr = immu_base @: consti 11 768
let itlbw1tr0_addr = immu_base @: consti 11 896
let itlbw2mr0_addr = immu_base @: consti 11 1024
let itlbw2tr0_addr = immu_base @: consti 11 1152
let itlbw3mr0_addr = immu_base @: consti 11 1280
let itlbw3tr0_addr = immu_base @: consti 11 1408

let dc_base        = consti 5 3
let dccr_addr      = dc_base @: consti 11 0
let dcbpr_addr     = dc_base @: consti 11 1
let dcbfr_addr     = dc_base @: consti 11 2
let dcbir_addr     = dc_base @: consti 11 3
let dcbwr_addr     = dc_base @: consti 11 4
let dcblr_addr     = dc_base @: consti 11 5

let ic_base        = consti 5 4
let iccr_addr      = ic_base @: consti 11 0
let icbpr_addr     = ic_base @: consti 11 1
let icbir_addr     = ic_base @: consti 11 2
let icblr_addr     = ic_base @: consti 11 3

let mac_base       = consti 5 5
let maclo_addr     = mac_base @: consti 11 1
let machi_addr     = mac_base @: consti 11 2

let du_base        = consti 5 6
let dvr0_addr      = du_base @: consti 11 0
let dcr0_addr      = du_base @: consti 11 8
let dmr1_addr      = du_base @: consti 11 16
let dmr2_addr      = du_base @: consti 11 17
let dcwr0_addr     = du_base @: consti 11 18
let dsr_addr       = du_base @: consti 11 20
let drr_addr       = du_base @: consti 11 21

let pc_base        = consti 5 7
let pccr0_addr     = pc_base @: consti 11 0
let pcmr0_addr     = pc_base @: consti 11 8

let pm_base        = consti 5 8
let pmr_addr       = pm_base @: consti 11 0

let pic_base       = consti 5 9
let picmr_addr     = pic_base @: consti 11 0
let picsr_addr     = pic_base @: consti 11 2

let tt_base        = consti 5 10
let ttmr_addr      = tt_base @: consti 11 0
let ttcr_addr      = tt_base @: consti 11 1

let fpu_base       = consti 5 11

(* Register bit defines *)

(* Supervision Register *)
let sr_sm       = 0     (* Supervisor mode *)
let sr_tee      = 1     (* Timer exception enable *)
let sr_iee      = 2     (* Interrupt exception enable *)
let sr_dce      = 3     (* Data cache enable *)
let sr_ice      = 4     (* Instruction cache enable *)
let sr_dme      = 5     (* Data MMU enable *)
let sr_ime      = 6     (* Instruction MMU enable *)
let sr_lee      = 7     (* Little-endian enable *)
let sr_ce       = 8     (* CID enable *)
let sr_f        = 9     (* Flag *)
let sr_cy       = 10    (* Carry flag *)
let sr_ov       = 11    (* Overflow flag *)
let sr_ove      = 12    (* Overflow exception enable *)
let sr_dsx      = 13    (* Delay slot exception *)
let sr_eph      = 14    (* Exception prefix high *)
let sr_fo       = 15    (* Fixed to one *)
let sr_sumra    = 16    (* SPR user read mode access *)
let sr_reserved = 27,17 (* Reserved *)
let sr_cid      = 31,28 (* Context ID *)

(* Version register - DEPRECATED *)
let vr_rev      = 5,0   (* Revision *)
let vr_uvrp     = 6     (* Updated Version Registers Present *)
let vr_reserved = 15,7  (* Reserved *)
let vr_cfg      = 23,16 (* Configuration Template *)
let vr_ver      = 31,24 (* Version *)


(* Unit Present register *)
let upr_up        = 0
let upr_dcp       = 1
let upr_icp       = 2
let upr_dmp       = 3
let upr_imp       = 4
let upr_mp        = 5
let upr_dup       = 6
let upr_pcup      = 7
let upr_picp      = 8
let upr_pmp       = 9
let upr_ttp       = 10
let upr_reserved  = 23,11
let upr_cup       = 31,24

(* CPU Configuration register *)
let cpucfgr_nsgf     = 3,0 (* Number of shadow GPRs *)
let cpucfgr_cfg      = 4
let cpucfgr_ob32s    = 5
let cpucfgr_ob64s    = 6
let cpucfgr_of32s    = 7
let cpucfgr_of64s    = 8
let cpucfgr_ov64s    = 9
let cpucfgr_nd       = 10 (* No delay-slot implementation *)
let cpucfgr_avrp     = 11 (* Arch. version registers *)
let cpucfgr_evbarp   = 12 (* Exception vector base addr reg *)
let cpucfgr_isrp     = 13 (* Implementation specific regs *)
let cpucfgr_aecsrp   = 14 (* Arith. exception regs *)
let cpucfgr_reserved = 31,15

(* Version register 2 (new with OR1K 1.0) *)
let vr2_ver   = 23,0
let vr2_cpuid = 31,24

(* Architecture Version register *)
let avr_reserved = 7,0
let avr_rev      = 15,8
let avr_min      = 23,16
let avr_maj      = 31,24

(* Exception Vector Base Address register *)
let evbar_reserved = 12,0
let evbar_evba     = 31,13

(* Arithmetic Exception Control register *)
let aecr_cyadde    = 0
let aecr_ovadde    = 1
let aecr_cymule    = 2
let aecr_ovmule    = 3
let aecr_dbze      = 4
let aecr_cymacadde = 5
let aecr_ovmacadde = 6
let aecr_reserved  = 31,7

(* Arithmetic Exception Status register *)
let aesr_cyadde    = 0
let aesr_ovadde    = 1
let aesr_cymule    = 2
let aesr_ovmule    = 3
let aesr_dbze      = 4
let aesr_cymacadde = 5
let aesr_ovmacadde = 6
let aesr_reserved  = 31,7

(* Tick timer registers *)
let ttmr_tp   = 27,0 (* Time period *)
let ttmr_ip   = 28   (* Interrupt pending *)
let ttmr_ie   = 29   (* Interrupt enable *)
let ttmr_m    = 31,30 (* Mode *)
(* Tick timer mode values *)
let ttmr_m_dis = 0b00  (* Disabled *)
let ttmr_m_rst = 0b01  (* Restart-on-match mode *)
let ttmr_m_stp = 0b10  (* Stop-on-match mode *)
let ttmr_m_cnt = 0b11  (* Continue counting mode *)

(* Data Cache Configuration register *)
let dccfgr_ncw    = 2,0 (* Number of Cache Ways *)
let dccfgr_ncs    = 6,3 (* Number of Cache Sets *)
let dccfgr_cbs    = 7   (* Cache Block Size *)
let dccfgr_cws    = 8   (* Cache Write Strategy *)
let dccfgr_ccri   = 9   (* Cache Control Register Implemented *)
let dccfgr_cbiri  = 10  (* Cache Block Invalidate Register Implemented *)
let dccfgr_cbpri  = 11  (* Cache Block Prefetch Register Implemented *)
let dccfgr_cblri  = 12  (* Cache Block Lock Register Implemented *)
let dccfgr_cbfri  = 13  (* Cache Block Flush Register Implemented *)
let dccfgr_cbwbri = 14  (* Cache Block Write-Back Register Implemented *)

(* Instruction Cache Configuration register *)
let iccfgr_ncw   = 2,0 (* Number of Cache Ways *)
let iccfgr_ncs   = 6,3 (* Number of Cache Sets *)
let iccfgr_cbs   = 7   (* Cache Block Size *)
let iccfgr_ccri  = 9   (* Cache Control Register Implemented *)
let iccfgr_cbiri = 10  (* Cache Block Invalidate Register Implemented *)
let iccfgr_cbpri = 11  (* Cache Block Prefetch Register Implemented *)
let iccfgr_cblri = 12  (* Cache Block Lock Register Implemented *)

(* Data MMU Configuration register *)
let dmmufgr_ntw   = 1,0 (* Number of TLB ways *)
let dmmufgr_nts   = 4,2 (* Number of TLB sets *)
let dmmufgr_nae   = 7,5 (* Number of ATB entries *)
let dmmufgr_cri   = 8   (* Control Register Implemented *)
let dmmufgr_pri   = 9   (* Protection Register Implemented *)
let dmmufgr_teiri = 10  (* TLB Entry Invalidate Register Implemented *)
let dmmufgr_htr   = 11  (* Hardware TLB Reload *)

(* Instruction MMU Configuration register *)
let immufgr_ntw   = 1,0 (* Number of TLB ways *)
let immufgr_nts   = 4,2 (* Number of TLB sets *)
let immufgr_nae   = 7,5 (* Number of ATB entries *)
let immufgr_cri   = 8   (* Control Register Implemented *)
let immufgr_pri   = 9   (* Protection Register Implemented *)
let immufgr_teiri = 10  (* TLB Entry Invalidate Register Implemented *)
let immufgr_htr   = 11  (* Hardware TLB Reload *)

(* Debug Mode Register 1 *)
let dmr1_st = 22
let dmr1_bt = 23

(* Debug Stop Register *)
let dsr_rste  = 0
let dsr_busee = 1
let dsr_dpfe  = 2
let dsr_ipfe  = 3
let dsr_tte   = 4
let dsr_ae    = 5
let dsr_iie   = 6
let dsr_inte  = 7
let dsr_dme   = 8
let dsr_ime   = 9
let dsr_re    = 10
let dsr_sce   = 11
let dsr_fpe   = 12
let dsr_te    = 13

let drr_rste  = 0
let drr_busee = 1
let drr_dpfe  = 2
let drr_ipfe  = 3
let drr_tte   = 4
let drr_ae    = 5
let drr_iie   = 6
let drr_ie    = 7
let drr_dme   = 8
let drr_ime   = 9
let drr_re    = 10
let drr_sce   = 11
let drr_fpe   = 12
let drr_te    = 13

(* Implementation-specific SPR defines *)
let mor1kx_sr_width = 16
let mor1kx_sr_reset_value = consti mor1kx_sr_width 0x8001

