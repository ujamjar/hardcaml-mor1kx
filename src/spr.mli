val base : int -> int
val offset : int -> int

(* Addresses *)
val sys_base : HardCaml.Signal.Comb.t
val vr_addr : HardCaml.Signal.Comb.t
val upr_addr : HardCaml.Signal.Comb.t
val cpucfgr_addr : HardCaml.Signal.Comb.t
val dmmucfgr_addr : HardCaml.Signal.Comb.t
val immucfgr_addr : HardCaml.Signal.Comb.t
val dccfgr_addr : HardCaml.Signal.Comb.t
val iccfgr_addr : HardCaml.Signal.Comb.t
val dcfgr_addr : HardCaml.Signal.Comb.t
val pccfgr_addr : HardCaml.Signal.Comb.t
val vr2_addr : HardCaml.Signal.Comb.t
val avr_addr : HardCaml.Signal.Comb.t
val evbar_addr : HardCaml.Signal.Comb.t
val aecr_addr : HardCaml.Signal.Comb.t
val aesr_addr : HardCaml.Signal.Comb.t
val npc_addr : HardCaml.Signal.Comb.t
val sr_addr : HardCaml.Signal.Comb.t
val ppc_addr : HardCaml.Signal.Comb.t
val fpcsr_addr : HardCaml.Signal.Comb.t
val isr0_addr : HardCaml.Signal.Comb.t
val epcr0_addr : HardCaml.Signal.Comb.t
val eear0_addr : HardCaml.Signal.Comb.t
val esr0_addr : HardCaml.Signal.Comb.t
val coreid_addr : HardCaml.Signal.Comb.t
val numcores_addr : HardCaml.Signal.Comb.t
val gpr0_addr : HardCaml.Signal.Comb.t

val dmmu_base : HardCaml.Signal.Comb.t
val dmmucr_addr : HardCaml.Signal.Comb.t
val dmmupr_addr : HardCaml.Signal.Comb.t
val dtlbeir_addr : HardCaml.Signal.Comb.t
val datbmr0_addr : HardCaml.Signal.Comb.t
val datbtr0_addr : HardCaml.Signal.Comb.t
val dtlbw0mr0_addr : HardCaml.Signal.Comb.t
val dtlbw0tr0_addr : HardCaml.Signal.Comb.t
val dtlbw1mr0_addr : HardCaml.Signal.Comb.t
val dtlbw1tr0_addr : HardCaml.Signal.Comb.t
val dtlbw2mr0_addr : HardCaml.Signal.Comb.t
val dtlbw2tr0_addr : HardCaml.Signal.Comb.t
val dtlbw3mr0_addr : HardCaml.Signal.Comb.t
val dtlbw3tr0_addr : HardCaml.Signal.Comb.t

val immu_base : HardCaml.Signal.Comb.t
val immucr_addr : HardCaml.Signal.Comb.t
val immupr_addr : HardCaml.Signal.Comb.t
val itlbeir_addr : HardCaml.Signal.Comb.t
val iatbmr0_addr : HardCaml.Signal.Comb.t
val iatbtr0_addr : HardCaml.Signal.Comb.t
val itlbw0mr0_addr : HardCaml.Signal.Comb.t
val itlbw0tr0_addr : HardCaml.Signal.Comb.t
val itlbw1mr0_addr : HardCaml.Signal.Comb.t
val itlbw1tr0_addr : HardCaml.Signal.Comb.t
val itlbw2mr0_addr : HardCaml.Signal.Comb.t
val itlbw2tr0_addr : HardCaml.Signal.Comb.t
val itlbw3mr0_addr : HardCaml.Signal.Comb.t
val itlbw3tr0_addr : HardCaml.Signal.Comb.t

val dc_base : HardCaml.Signal.Comb.t
val dccr_addr : HardCaml.Signal.Comb.t
val dcbpr_addr : HardCaml.Signal.Comb.t
val dcbfr_addr : HardCaml.Signal.Comb.t
val dcbir_addr : HardCaml.Signal.Comb.t
val dcbwr_addr : HardCaml.Signal.Comb.t
val dcblr_addr : HardCaml.Signal.Comb.t

val ic_base : HardCaml.Signal.Comb.t
val iccr_addr : HardCaml.Signal.Comb.t
val icbpr_addr : HardCaml.Signal.Comb.t
val icbir_addr : HardCaml.Signal.Comb.t
val icblr_addr : HardCaml.Signal.Comb.t

val mac_base : HardCaml.Signal.Comb.t
val maclo_addr : HardCaml.Signal.Comb.t
val machi_addr : HardCaml.Signal.Comb.t

val du_base : HardCaml.Signal.Comb.t
val dvr0_addr : HardCaml.Signal.Comb.t
val dcr0_addr : HardCaml.Signal.Comb.t
val dmr1_addr : HardCaml.Signal.Comb.t
val dmr2_addr : HardCaml.Signal.Comb.t
val dcwr0_addr : HardCaml.Signal.Comb.t
val dsr_addr : HardCaml.Signal.Comb.t
val drr_addr : HardCaml.Signal.Comb.t

val pc_base : HardCaml.Signal.Comb.t
val pccr0_addr : HardCaml.Signal.Comb.t
val pcmr0_addr : HardCaml.Signal.Comb.t

val pm_base : HardCaml.Signal.Comb.t
val pmr_addr : HardCaml.Signal.Comb.t

val pic_base : HardCaml.Signal.Comb.t
val picmr_addr : HardCaml.Signal.Comb.t
val picsr_addr : HardCaml.Signal.Comb.t

val tt_base : HardCaml.Signal.Comb.t
val ttmr_addr : HardCaml.Signal.Comb.t
val ttcr_addr : HardCaml.Signal.Comb.t

val fpu_base : HardCaml.Signal.Comb.t

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
val mor1kx_sr_reset_value : HardCaml.Signal.Comb.t
