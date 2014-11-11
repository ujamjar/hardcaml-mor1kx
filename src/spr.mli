(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module Make(B : HardCaml.Comb.S) : sig

  val base : B.t -> B.t
  val offset : B.t -> B.t

  (* Addresses *)
  val sys_base : B.t
  val vr_addr : B.t
  val upr_addr : B.t
  val cpucfgr_addr : B.t
  val dmmucfgr_addr : B.t
  val immucfgr_addr : B.t
  val dccfgr_addr : B.t
  val iccfgr_addr : B.t
  val dcfgr_addr : B.t
  val pccfgr_addr : B.t
  val vr2_addr : B.t
  val avr_addr : B.t
  val evbar_addr : B.t
  val aecr_addr : B.t
  val aesr_addr : B.t
  val npc_addr : B.t
  val sr_addr : B.t
  val ppc_addr : B.t
  val fpcsr_addr : B.t
  val isr0_addr : B.t
  val epcr0_addr : B.t
  val eear0_addr : B.t
  val esr0_addr : B.t
  val coreid_addr : B.t
  val numcores_addr : B.t
  val gpr0_addr : B.t

  val dmmu_base : B.t
  val dmmucr_addr : B.t
  val dmmupr_addr : B.t
  val dtlbeir_addr : B.t
  val datbmr0_addr : B.t
  val datbtr0_addr : B.t
  val dtlbw0mr0_addr : B.t
  val dtlbw0tr0_addr : B.t
  val dtlbw1mr0_addr : B.t
  val dtlbw1tr0_addr : B.t
  val dtlbw2mr0_addr : B.t
  val dtlbw2tr0_addr : B.t
  val dtlbw3mr0_addr : B.t
  val dtlbw3tr0_addr : B.t

  val immu_base : B.t
  val immucr_addr : B.t
  val immupr_addr : B.t
  val itlbeir_addr : B.t
  val iatbmr0_addr : B.t
  val iatbtr0_addr : B.t
  val itlbw0mr0_addr : B.t
  val itlbw0tr0_addr : B.t
  val itlbw1mr0_addr : B.t
  val itlbw1tr0_addr : B.t
  val itlbw2mr0_addr : B.t
  val itlbw2tr0_addr : B.t
  val itlbw3mr0_addr : B.t
  val itlbw3tr0_addr : B.t

  val dc_base : B.t
  val dccr_addr : B.t
  val dcbpr_addr : B.t
  val dcbfr_addr : B.t
  val dcbir_addr : B.t
  val dcbwr_addr : B.t
  val dcblr_addr : B.t

  val ic_base : B.t
  val iccr_addr : B.t
  val icbpr_addr : B.t
  val icbir_addr : B.t
  val icblr_addr : B.t

  val mac_base : B.t
  val maclo_addr : B.t
  val machi_addr : B.t

  val du_base : B.t
  val dvr0_addr : B.t
  val dcr0_addr : B.t
  val dmr1_addr : B.t
  val dmr2_addr : B.t
  val dcwr0_addr : B.t
  val dsr_addr : B.t
  val drr_addr : B.t

  val pc_base : B.t
  val pccr0_addr : B.t
  val pcmr0_addr : B.t

  val pm_base : B.t
  val pmr_addr : B.t

  val pic_base : B.t
  val picmr_addr : B.t
  val picsr_addr : B.t

  val tt_base : B.t
  val ttmr_addr : B.t
  val ttcr_addr : B.t

  val fpu_base : B.t

  (* Register bit defines *)

  (* Supervision Register *)
  val sr_sm : int
  val sr_tee : int
  val sr_iee : int
  val sr_dce : int
  val sr_ice : int
  val sr_dme : int
  val sr_ime : int
  val sr_lee : int
  val sr_ce : int
  val sr_f : int
  val sr_cy : int
  val sr_ov : int
  val sr_ove : int
  val sr_dsx : int
  val sr_eph : int
  val sr_fo : int
  val sr_sumra : int
  val sr_reserved : int * int
  val sr_cid : int * int

  (* Version register - DEPRECATED *)
  val vr_rev : int * int
  val vr_uvrp : int
  val vr_reserved : int * int
  val vr_cfg : int * int
  val vr_ver : int * int

  (* Unit Present register *)
  val upr_up : int
  val upr_dcp : int
  val upr_icp : int
  val upr_dmp : int
  val upr_imp : int
  val upr_mp : int
  val upr_dup : int
  val upr_pcup : int
  val upr_picp : int
  val upr_pmp : int
  val upr_ttp : int
  val upr_reserved : int * int
  val upr_cup : int * int

  (* CPU Configuration register *)
  val cpucfgr_nsgf : int * int
  val cpucfgr_cfg : int
  val cpucfgr_ob32s : int
  val cpucfgr_ob64s : int
  val cpucfgr_of32s : int
  val cpucfgr_of64s : int
  val cpucfgr_ov64s : int
  val cpucfgr_nd : int
  val cpucfgr_avrp : int
  val cpucfgr_evbarp : int
  val cpucfgr_isrp : int
  val cpucfgr_aecsrp : int
  val cpucfgr_reserved : int * int

  (* Version register 2 (new with OR1K 1.0) *)
  val vr2_ver : int * int
  val vr2_cpuid : int * int

  (* Architecture Version register *)
  val avr_reserved : int * int
  val avr_rev : int * int
  val avr_min : int * int
  val avr_maj : int * int

  (* Exception Vector Base Address register *)
  val evbar_reserved : int * int
  val evbar_evba : int * int

  (* Arithmetic Exception Control register *)
  val aecr_cyadde : int
  val aecr_ovadde : int
  val aecr_cymule : int
  val aecr_ovmule : int
  val aecr_dbze : int
  val aecr_cymacadde : int
  val aecr_ovmacadde : int
  val aecr_reserved : int * int

  (* Arithmetic Exception Status register *)
  val aesr_cyadde : int
  val aesr_ovadde : int
  val aesr_cymule : int
  val aesr_ovmule : int
  val aesr_dbze : int
  val aesr_cymacadde : int
  val aesr_ovmacadde : int
  val aesr_reserved : int * int

  (* Tick timer registers *)
  val ttmr_tp : int * int
  val ttmr_ip : int
  val ttmr_ie : int
  val ttmr_m : int * int

  (* Tick timer mode values *)
  val ttmr_m_dis : int
  val ttmr_m_rst : int
  val ttmr_m_stp : int
  val ttmr_m_cnt : int

  (* Data Cache Configuration register *)
  val dccfgr_ncw : int * int
  val dccfgr_ncs : int * int
  val dccfgr_cbs : int
  val dccfgr_cws : int
  val dccfgr_ccri : int
  val dccfgr_cbiri : int
  val dccfgr_cbpri : int
  val dccfgr_cblri : int
  val dccfgr_cbfri : int
  val dccfgr_cbwbri : int

  (* Instruction Cache Configuration register *)
  val iccfgr_ncw : int * int
  val iccfgr_ncs : int * int
  val iccfgr_cbs : int
  val iccfgr_ccri : int
  val iccfgr_cbiri : int
  val iccfgr_cbpri : int
  val iccfgr_cblri : int

  (* Data MMU Configuration register *)
  val dmmufgr_ntw : int * int
  val dmmufgr_nts : int * int
  val dmmufgr_nae : int * int
  val dmmufgr_cri : int
  val dmmufgr_pri : int
  val dmmufgr_teiri : int
  val dmmufgr_htr : int

  (* Instruction MMU Configuration register *)
  val immufgr_ntw : int * int
  val immufgr_nts : int * int
  val immufgr_nae : int * int
  val immufgr_cri : int
  val immufgr_pri : int
  val immufgr_teiri : int
  val immufgr_htr : int

  (* Debug Mode Register 1 *)
  val dmr1_st : int
  val dmr1_bt : int

  (* Debug Stop Register *)
  val dsr_rste : int
  val dsr_busee : int
  val dsr_dpfe : int
  val dsr_ipfe : int
  val dsr_tte : int
  val dsr_ae : int
  val dsr_iie : int
  val dsr_inte : int
  val dsr_dme : int
  val dsr_ime : int
  val dsr_re : int
  val dsr_sce : int
  val dsr_fpe : int
  val dsr_te : int
  val drr_rste : int
  val drr_busee : int
  val drr_dpfe : int
  val drr_ipfe : int
  val drr_tte : int
  val drr_ae : int
  val drr_iie : int
  val drr_ie : int
  val drr_dme : int
  val drr_ime : int
  val drr_re : int
  val drr_sce : int
  val drr_fpe : int
  val drr_te : int

  (* Implementation-specific SPR defines *)
  val mor1kx_sr_width : int
  val mor1kx_sr_reset_value : B.t

end

