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

  val base_width : int
  val offset_width : int

  (* Addresses *)
  module Sys : sig
    val base : int
    val const : int -> B.t
    val vr : int
    val upr : int
    val cpucfgr : int
    val dmmucfgr : int
    val immucfgr : int
    val dccfgr : int
    val iccfgr : int
    val dcfgr : int
    val pccfgr : int
    val vr2 : int
    val avr : int
    val evbar : int
    val aecr : int
    val aesr : int
    val npc : int
    val sr : int
    val ppc : int
    val fpcsr : int
    val isr0 : int
    val epcr0 : int
    val eear0 : int
    val esr0 : int
    val coreid : int
    val numcores : int
    val gpr0 : int
  end

  module Dmmu : sig
    val base : int
    val const : int -> B.t
    val dmmucr : int
    val dmmupr : int
    val dtlbeir : int
    val datbmr0 : int
    val datbtr0 : int
    val dtlbw0mr0 : int
    val dtlbw0tr0 : int
    val dtlbw1mr0 : int
    val dtlbw1tr0 : int
    val dtlbw2mr0 : int
    val dtlbw2tr0 : int
    val dtlbw3mr0 : int
    val dtlbw3tr0 : int
  end

  module Immu : sig
    val base : int
    val const : int -> B.t
    val immucr : int
    val immupr : int
    val itlbeir : int
    val iatbmr0 : int
    val iatbtr0 : int
    val itlbw0mr0 : int
    val itlbw0tr0 : int
    val itlbw1mr0 : int
    val itlbw1tr0 : int
    val itlbw2mr0 : int
    val itlbw2tr0 : int
    val itlbw3mr0 : int
    val itlbw3tr0 : int
  end

  module Dc : sig
    val base : int
    val const : int -> B.t
    val dccr : int
    val dcbpr : int
    val dcbfr : int
    val dcbir : int
    val dcbwr : int
    val dcblr : int
  end

  module Ic : sig
    val base : int
    val const : int -> B.t
    val iccr : int
    val icbpr : int
    val icbir : int
    val icblr : int
  end

  module Mac : sig
    val base : int
    val const : int -> B.t
    val maclo : int
    val machi : int
  end

  module Du : sig
    val base : int
    val const : int -> B.t
    val dvr0 : int
    val dcr0 : int
    val dmr1 : int
    val dmr2 : int
    val dcwr0 : int
    val dsr : int
    val drr : int
  end

  module Pc : sig
    val base : int
    val const : int -> B.t
    val pccr0 : int
    val pcmr0 : int
  end

  module Pm : sig
    val base : int
    val const : int -> B.t
    val pmr : int
  end

  module Pic : sig
    val base : int
    val const : int -> B.t
    val picmr : int
    val picsr : int
  end

  module Tt : sig
    val base : int
    val const : int -> B.t
    val ttmr : int
    val ttcr : int
  end

  module Fpu : sig
    val base : int
    val const : int -> B.t
  end

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

