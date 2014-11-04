val base : int -> int
val offset : int -> int

(* Addresses *)
val spr_sys_base : HardCaml.Signal.Comb.t
val spr_vr_addr : HardCaml.Signal.Comb.t
val spr_upr_addr : HardCaml.Signal.Comb.t
val spr_cpucfgr_addr : HardCaml.Signal.Comb.t
val spr_dmmucfgr_addr : HardCaml.Signal.Comb.t
val spr_immucfgr_addr : HardCaml.Signal.Comb.t
val spr_dccfgr_addr : HardCaml.Signal.Comb.t
val spr_iccfgr_addr : HardCaml.Signal.Comb.t
val spr_dcfgr_addr : HardCaml.Signal.Comb.t
val spr_pccfgr_addr : HardCaml.Signal.Comb.t
val spr_vr2_addr : HardCaml.Signal.Comb.t
val spr_avr_addr : HardCaml.Signal.Comb.t
val spr_evbar_addr : HardCaml.Signal.Comb.t
val spr_aecr_addr : HardCaml.Signal.Comb.t
val spr_aesr_addr : HardCaml.Signal.Comb.t
val spr_npc_addr : HardCaml.Signal.Comb.t
val spr_sr_addr : HardCaml.Signal.Comb.t
val spr_ppc_addr : HardCaml.Signal.Comb.t
val spr_fpcsr_addr : HardCaml.Signal.Comb.t
val spr_isr0_addr : HardCaml.Signal.Comb.t
val spr_epcr0_addr : HardCaml.Signal.Comb.t
val spr_eear0_addr : HardCaml.Signal.Comb.t
val spr_esr0_addr : HardCaml.Signal.Comb.t
val spr_coreid_addr : HardCaml.Signal.Comb.t
val spr_numcores_addr : HardCaml.Signal.Comb.t
val spr_gpr0_addr : HardCaml.Signal.Comb.t

val spr_dmmu_base : HardCaml.Signal.Comb.t
val spr_dmmucr_addr : HardCaml.Signal.Comb.t
val spr_dmmupr_addr : HardCaml.Signal.Comb.t
val spr_dtlbeir_addr : HardCaml.Signal.Comb.t
val spr_datbmr0_addr : HardCaml.Signal.Comb.t
val spr_datbtr0_addr : HardCaml.Signal.Comb.t
val spr_dtlbw0mr0_addr : HardCaml.Signal.Comb.t
val spr_dtlbw0tr0_addr : HardCaml.Signal.Comb.t
val spr_dtlbw1mr0_addr : HardCaml.Signal.Comb.t
val spr_dtlbw1tr0_addr : HardCaml.Signal.Comb.t
val spr_dtlbw2mr0_addr : HardCaml.Signal.Comb.t
val spr_dtlbw2tr0_addr : HardCaml.Signal.Comb.t
val spr_dtlbw3mr0_addr : HardCaml.Signal.Comb.t
val spr_dtlbw3tr0_addr : HardCaml.Signal.Comb.t

val spr_immu_base : HardCaml.Signal.Comb.t
val spr_immucr_addr : HardCaml.Signal.Comb.t
val spr_immupr_addr : HardCaml.Signal.Comb.t
val spr_itlbeir_addr : HardCaml.Signal.Comb.t
val spr_iatbmr0_addr : HardCaml.Signal.Comb.t
val spr_iatbtr0_addr : HardCaml.Signal.Comb.t
val spr_itlbw0mr0_addr : HardCaml.Signal.Comb.t
val spr_itlbw0tr0_addr : HardCaml.Signal.Comb.t
val spr_itlbw1mr0_addr : HardCaml.Signal.Comb.t
val spr_itlbw1tr0_addr : HardCaml.Signal.Comb.t
val spr_itlbw2mr0_addr : HardCaml.Signal.Comb.t
val spr_itlbw2tr0_addr : HardCaml.Signal.Comb.t
val spr_itlbw3mr0_addr : HardCaml.Signal.Comb.t
val spr_itlbw3tr0_addr : HardCaml.Signal.Comb.t

val spr_dc_base : HardCaml.Signal.Comb.t
val spr_dccr_addr : HardCaml.Signal.Comb.t
val spr_dcbpr_addr : HardCaml.Signal.Comb.t
val spr_dcbfr_addr : HardCaml.Signal.Comb.t
val spr_dcbir_addr : HardCaml.Signal.Comb.t
val spr_dcbwr_addr : HardCaml.Signal.Comb.t
val spr_dcblr_addr : HardCaml.Signal.Comb.t

val spr_ic_base : HardCaml.Signal.Comb.t
val spr_iccr_addr : HardCaml.Signal.Comb.t
val spr_icbpr_addr : HardCaml.Signal.Comb.t
val spr_icbir_addr : HardCaml.Signal.Comb.t
val spr_icblr_addr : HardCaml.Signal.Comb.t

val spr_mac_base : HardCaml.Signal.Comb.t
val spr_maclo_addr : HardCaml.Signal.Comb.t
val spr_machi_addr : HardCaml.Signal.Comb.t

val spr_du_base : HardCaml.Signal.Comb.t
val spr_dvr0_addr : HardCaml.Signal.Comb.t
val spr_dcr0_addr : HardCaml.Signal.Comb.t
val spr_dmr1_addr : HardCaml.Signal.Comb.t
val spr_dmr2_addr : HardCaml.Signal.Comb.t
val spr_dcwr0_addr : HardCaml.Signal.Comb.t
val spr_dsr_addr : HardCaml.Signal.Comb.t
val spr_drr_addr : HardCaml.Signal.Comb.t

val spr_pc_base : HardCaml.Signal.Comb.t
val spr_pccr0_addr : HardCaml.Signal.Comb.t
val spr_pcmr0_addr : HardCaml.Signal.Comb.t

val spr_pm_base : HardCaml.Signal.Comb.t
val spr_pmr_addr : HardCaml.Signal.Comb.t

val spr_pic_base : HardCaml.Signal.Comb.t
val spr_picmr_addr : HardCaml.Signal.Comb.t
val spr_picsr_addr : HardCaml.Signal.Comb.t

val spr_tt_base : HardCaml.Signal.Comb.t
val spr_ttmr_addr : HardCaml.Signal.Comb.t
val spr_ttcr_addr : HardCaml.Signal.Comb.t

val spr_fpu_base : HardCaml.Signal.Comb.t

(* Register bit defines *)

(* Supervision Register *)
val spr_sr_sm : int
val spr_sr_tee : int
val spr_sr_iee : int
val spr_sr_dce : int
val spr_sr_ice : int
val spr_sr_dme : int
val spr_sr_ime : int
val spr_sr_lee : int
val spr_sr_ce : int
val spr_sr_f : int
val spr_sr_cy : int
val spr_sr_ov : int
val spr_sr_ove : int
val spr_sr_dsx : int
val spr_sr_eph : int
val spr_sr_fo : int
val spr_sr_sumra : int
val spr_sr_reserved : int * int
val spr_sr_cid : int * int

(* Version register - DEPRECATED *)
val spr_vr_rev : int * int
val spr_vr_uvrp : int
val spr_vr_reserved : int * int
val spr_vr_cfg : int * int
val spr_vr_ver : int * int

(* Unit Present register *)
val spr_upr_up : int
val spr_upr_dcp : int
val spr_upr_icp : int
val spr_upr_dmp : int
val spr_upr_imp : int
val spr_upr_mp : int
val spr_upr_dup : int
val spr_upr_pcup : int
val spr_upr_picp : int
val spr_upr_pmp : int
val spr_upr_ttp : int
val spr_upr_reserved : int * int
val spr_upr_cup : int * int

(* CPU Configuration register *)
val spr_cpucfgr_nsgf : int * int
val spr_cpucfgr_cfg : int
val spr_cpucfgr_ob32s : int
val spr_cpucfgr_ob64s : int
val spr_cpucfgr_of32s : int
val spr_cpucfgr_of64s : int
val spr_cpucfgr_ov64s : int
val spr_cpucfgr_nd : int
val spr_cpucfgr_avrp : int
val spr_cpucfgr_evbarp : int
val spr_cpucfgr_isrp : int
val spr_cpucfgr_aecsrp : int
val spr_cpucfgr_reserved : int * int

(* Version register 2 (new with OR1K 1.0) *)
val spr_vr2_ver : int * int
val spr_vr2_cpuid : int * int

(* Architecture Version register *)
val spr_avr_reserved : int * int
val spr_avr_rev : int * int
val spr_avr_min : int * int
val spr_avr_maj : int * int

(* Exception Vector Base Address register *)
val spr_evbar_reserved : int * int
val spr_evbar_evba : int * int

(* Arithmetic Exception Control register *)
val spr_aecr_cyadde : int
val spr_aecr_ovadde : int
val spr_aecr_cymule : int
val spr_aecr_ovmule : int
val spr_aecr_dbze : int
val spr_aecr_cymacadde : int
val spr_aecr_ovmacadde : int
val spr_aecr_reserved : int * int

(* Arithmetic Exception Status register *)
val spr_aesr_cyadde : int
val spr_aesr_ovadde : int
val spr_aesr_cymule : int
val spr_aesr_ovmule : int
val spr_aesr_dbze : int
val spr_aesr_cymacadde : int
val spr_aesr_ovmacadde : int
val spr_aesr_reserved : int * int

(* Tick timer registers *)
val spr_ttmr_tp : int * int
val spr_ttmr_ip : int
val spr_ttmr_ie : int
val spr_ttmr_m : int * int

(* Tick timer mode values *)
val spr_ttmr_m_dis : int
val spr_ttmr_m_rst : int
val spr_ttmr_m_stp : int
val spr_ttmr_m_cnt : int

(* Data Cache Configuration register *)
val spr_dccfgr_ncw : int * int
val spr_dccfgr_ncs : int * int
val spr_dccfgr_cbs : int
val spr_dccfgr_cws : int
val spr_dccfgr_ccri : int
val spr_dccfgr_cbiri : int
val spr_dccfgr_cbpri : int
val spr_dccfgr_cblri : int
val spr_dccfgr_cbfri : int
val spr_dccfgr_cbwbri : int

(* Instruction Cache Configuration register *)
val spr_iccfgr_ncw : int * int
val spr_iccfgr_ncs : int * int
val spr_iccfgr_cbs : int
val spr_iccfgr_ccri : int
val spr_iccfgr_cbiri : int
val spr_iccfgr_cbpri : int
val spr_iccfgr_cblri : int

(* Data MMU Configuration register *)
val spr_dmmufgr_ntw : int * int
val spr_dmmufgr_nts : int * int
val spr_dmmufgr_nae : int * int
val spr_dmmufgr_cri : int
val spr_dmmufgr_pri : int
val spr_dmmufgr_teiri : int
val spr_dmmufgr_htr : int

(* Instruction MMU Configuration register *)
val spr_immufgr_ntw : int * int
val spr_immufgr_nts : int * int
val spr_immufgr_nae : int * int
val spr_immufgr_cri : int
val spr_immufgr_pri : int
val spr_immufgr_teiri : int
val spr_immufgr_htr : int

(* Debug Mode Register 1 *)
val spr_dmr1_st : int
val spr_dmr1_bt : int

(* Debug Stop Register *)
val spr_dsr_rste : int
val spr_dsr_busee : int
val spr_dsr_dpfe : int
val spr_dsr_ipfe : int
val spr_dsr_tte : int
val spr_dsr_ae : int
val spr_dsr_iie : int
val spr_dsr_inte : int
val spr_dsr_dme : int
val spr_dsr_ime : int
val spr_dsr_re : int
val spr_dsr_sce : int
val spr_dsr_fpe : int
val spr_dsr_te : int
val spr_drr_rste : int
val spr_drr_busee : int
val spr_drr_dpfe : int
val spr_drr_ipfe : int
val spr_drr_tte : int
val spr_drr_ae : int
val spr_drr_iie : int
val spr_drr_ie : int
val spr_drr_dme : int
val spr_drr_ime : int
val spr_drr_re : int
val spr_drr_sce : int
val spr_drr_fpe : int
val spr_drr_te : int

(* Implementation-specific SPR defines *)
val mor1kx_spr_sr_width : int
val mor1kx_spr_sr_reset_value : HardCaml.Signal.Comb.t
