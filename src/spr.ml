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

module Make(B : HardCaml.Comb.S) = struct

  open B

  let base x = x.[15:11]
  let offset x = x.[10:0]

  let base_width = 5
  let offset_width = 11

  (* Addresses *)
  module Sys = struct
    let base      = 0
    let const x   = consti base_width base @: consti offset_width x
    let vr        = 0
    let upr       = 1
    let cpucfgr   = 2
    let dmmucfgr  = 3
    let immucfgr  = 4
    let dccfgr    = 5
    let iccfgr    = 6
    let dcfgr     = 7
    let pccfgr    = 8
    let vr2       = 9
    let avr       = 10
    let evbar     = 11
    let aecr      = 12
    let aesr      = 13
    let npc       = 16
    let sr        = 17
    let ppc       = 18
    let fpcsr     = 20
    let isr0      = 21
    let epcr0     = 32
    let eear0     = 48
    let esr0      = 64
    let coreid    = 128
    let numcores  = 129
    let gpr0      = 1024
  end

  module Dmmu = struct
    let base      = 1
    let const x   = consti base_width base @: consti offset_width x
    let dmmucr    = 0
    let dmmupr    = 1
    let dtlbeir   = 2
    let datbmr0   = 4
    let datbtr0   = 8
    let dtlbw0mr0 = 512
    let dtlbw0tr0 = 640
    let dtlbw1mr0 = 768
    let dtlbw1tr0 = 896
    let dtlbw2mr0 = 1024
    let dtlbw2tr0 = 1152
    let dtlbw3mr0 = 1280
    let dtlbw3tr0 = 1408
  end

  module Immu = struct
    let base      = 2
    let const x   = consti base_width base @: consti offset_width x
    let immucr    = 0
    let immupr    = 1
    let itlbeir   = 2
    let iatbmr0   = 4
    let iatbtr0   = 8
    let itlbw0mr0 = 512
    let itlbw0tr0 = 640
    let itlbw1mr0 = 768
    let itlbw1tr0 = 896
    let itlbw2mr0 = 1024
    let itlbw2tr0 = 1152
    let itlbw3mr0 = 1280
    let itlbw3tr0 = 1408
  end

  module Dc = struct
    let base      = 3
    let const x   = consti base_width base @: consti offset_width x
    let dccr      = 0
    let dcbpr     = 1
    let dcbfr     = 2
    let dcbir     = 3
    let dcbwr     = 4
    let dcblr     = 5
  end

  module Ic = struct
    let base      = 4
    let const x   = consti base_width base @: consti offset_width x
    let iccr      = 0
    let icbpr     = 1
    let icbir     = 2
    let icblr     = 3
  end

  module Mac = struct
    let base      = 5
    let const x   = consti base_width base @: consti offset_width x
    let maclo     = 1
    let machi     = 2
  end

  module Du = struct
    let base      = 6
    let const x   = consti base_width base @: consti offset_width x
    let dvr0      = 0
    let dcr0      = 8
    let dmr1      = 16
    let dmr2      = 17
    let dcwr0     = 18
    let dsr       = 20
    let drr       = 21
  end

  module Pc = struct
    let base      = 7
    let const x   = consti base_width base @: consti offset_width x
    let pccr0     = 0
    let pcmr0     = 8
  end

  module Pm = struct
    let base      = 8
    let const x   = consti base_width base @: consti offset_width x
    let pmr       = 0
  end

  module Pic = struct
    let base      = 9
    let const x   = consti base_width base @: consti offset_width x
    let picmr     = 0
    let picsr     = 2
  end

  module Tt = struct
    let base      = 10
    let const x   = consti base_width base @: consti offset_width x
    let ttmr      = 0
    let ttcr      = 1
  end

  module Fpu = struct
    let base      = 11
    let const x   = consti base_width base @: consti offset_width x
  end

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

end

