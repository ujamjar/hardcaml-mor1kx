open HardCaml.Signal.Comb

let base x = x / (1 lsl 11)
let offset x = x mod (1 lsl 11)

(* Addresses *)
let spr_sys_base       = consti 5 0
let spr_vr_addr        = spr_sys_base @: consti 11 0
let spr_upr_addr       = spr_sys_base @: consti 11 1
let spr_cpucfgr_addr   = spr_sys_base @: consti 11 2
let spr_dmmucfgr_addr  = spr_sys_base @: consti 11 3
let spr_immucfgr_addr  = spr_sys_base @: consti 11 4
let spr_dccfgr_addr    = spr_sys_base @: consti 11 5
let spr_iccfgr_addr    = spr_sys_base @: consti 11 6
let spr_dcfgr_addr     = spr_sys_base @: consti 11 7
let spr_pccfgr_addr    = spr_sys_base @: consti 11 8
let spr_vr2_addr       = spr_sys_base @: consti 11 9
let spr_avr_addr       = spr_sys_base @: consti 11 10
let spr_evbar_addr     = spr_sys_base @: consti 11 11
let spr_aecr_addr      = spr_sys_base @: consti 11 12
let spr_aesr_addr      = spr_sys_base @: consti 11 13
let spr_npc_addr       = spr_sys_base @: consti 11 16
let spr_sr_addr        = spr_sys_base @: consti 11 17
let spr_ppc_addr       = spr_sys_base @: consti 11 18
let spr_fpcsr_addr     = spr_sys_base @: consti 11 20
let spr_isr0_addr      = spr_sys_base @: consti 11 21
let spr_epcr0_addr     = spr_sys_base @: consti 11 32
let spr_eear0_addr     = spr_sys_base @: consti 11 48
let spr_esr0_addr      = spr_sys_base @: consti 11 64
let spr_coreid_addr    = spr_sys_base @: consti 11 128
let spr_numcores_addr  = spr_sys_base @: consti 11 129
let spr_gpr0_addr      = spr_sys_base @: consti 11 1024

let spr_dmmu_base      = consti 5 1
let spr_dmmucr_addr    = spr_dmmu_base @: consti 11 0
let spr_dmmupr_addr    = spr_dmmu_base @: consti 11 1
let spr_dtlbeir_addr   = spr_dmmu_base @: consti 11 2
let spr_datbmr0_addr   = spr_dmmu_base @: consti 11 4
let spr_datbtr0_addr   = spr_dmmu_base @: consti 11 8
let spr_dtlbw0mr0_addr = spr_dmmu_base @: consti 11 512
let spr_dtlbw0tr0_addr = spr_dmmu_base @: consti 11 640
let spr_dtlbw1mr0_addr = spr_dmmu_base @: consti 11 768
let spr_dtlbw1tr0_addr = spr_dmmu_base @: consti 11 896
let spr_dtlbw2mr0_addr = spr_dmmu_base @: consti 11 1024
let spr_dtlbw2tr0_addr = spr_dmmu_base @: consti 11 1152
let spr_dtlbw3mr0_addr = spr_dmmu_base @: consti 11 1280
let spr_dtlbw3tr0_addr = spr_dmmu_base @: consti 11 1408

let spr_immu_base      = consti 5 2
let spr_immucr_addr    = spr_immu_base @: consti 11 0
let spr_immupr_addr    = spr_immu_base @: consti 11 1
let spr_itlbeir_addr   = spr_immu_base @: consti 11 2
let spr_iatbmr0_addr   = spr_immu_base @: consti 11 4
let spr_iatbtr0_addr   = spr_immu_base @: consti 11 8
let spr_itlbw0mr0_addr = spr_immu_base @: consti 11 512
let spr_itlbw0tr0_addr = spr_immu_base @: consti 11 640
let spr_itlbw1mr0_addr = spr_immu_base @: consti 11 768
let spr_itlbw1tr0_addr = spr_immu_base @: consti 11 896
let spr_itlbw2mr0_addr = spr_immu_base @: consti 11 1024
let spr_itlbw2tr0_addr = spr_immu_base @: consti 11 1152
let spr_itlbw3mr0_addr = spr_immu_base @: consti 11 1280
let spr_itlbw3tr0_addr = spr_immu_base @: consti 11 1408

let spr_dc_base        = consti 5 3
let spr_dccr_addr      = spr_dc_base @: consti 11 0
let spr_dcbpr_addr     = spr_dc_base @: consti 11 1
let spr_dcbfr_addr     = spr_dc_base @: consti 11 2
let spr_dcbir_addr     = spr_dc_base @: consti 11 3
let spr_dcbwr_addr     = spr_dc_base @: consti 11 4
let spr_dcblr_addr     = spr_dc_base @: consti 11 5

let spr_ic_base        = consti 5 4
let spr_iccr_addr      = spr_ic_base @: consti 11 0
let spr_icbpr_addr     = spr_ic_base @: consti 11 1
let spr_icbir_addr     = spr_ic_base @: consti 11 2
let spr_icblr_addr     = spr_ic_base @: consti 11 3

let spr_mac_base       = consti 5 5
let spr_maclo_addr     = spr_mac_base @: consti 11 1
let spr_machi_addr     = spr_mac_base @: consti 11 2

let spr_du_base        = consti 5 6
let spr_dvr0_addr      = spr_du_base @: consti 11 0
let spr_dcr0_addr      = spr_du_base @: consti 11 8
let spr_dmr1_addr      = spr_du_base @: consti 11 16
let spr_dmr2_addr      = spr_du_base @: consti 11 17
let spr_dcwr0_addr     = spr_du_base @: consti 11 18
let spr_dsr_addr       = spr_du_base @: consti 11 20
let spr_drr_addr       = spr_du_base @: consti 11 21

let spr_pc_base        = consti 5 7
let spr_pccr0_addr     = spr_pc_base @: consti 11 0
let spr_pcmr0_addr     = spr_pc_base @: consti 11 8

let spr_pm_base        = consti 5 8
let spr_pmr_addr       = spr_pm_base @: consti 11 0

let spr_pic_base       = consti 5 9
let spr_picmr_addr     = spr_pic_base @: consti 11 0
let spr_picsr_addr     = spr_pic_base @: consti 11 2

let spr_tt_base        = consti 5 10
let spr_ttmr_addr      = spr_tt_base @: consti 11 0
let spr_ttcr_addr      = spr_tt_base @: consti 11 1

let spr_fpu_base       = consti 5 11

(* Register bit defines *)

(* Supervision Register *)
let spr_sr_sm       = 0     (* Supervisor mode *)
let spr_sr_tee      = 1     (* Timer exception enable *)
let spr_sr_iee      = 2     (* Interrupt exception enable *)
let spr_sr_dce      = 3     (* Data cache enable *)
let spr_sr_ice      = 4     (* Instruction cache enable *)
let spr_sr_dme      = 5     (* Data MMU enable *)
let spr_sr_ime      = 6     (* Instruction MMU enable *)
let spr_sr_lee      = 7     (* Little-endian enable *)
let spr_sr_ce       = 8     (* CID enable *)
let spr_sr_f        = 9     (* Flag *)
let spr_sr_cy       = 10    (* Carry flag *)
let spr_sr_ov       = 11    (* Overflow flag *)
let spr_sr_ove      = 12    (* Overflow exception enable *)
let spr_sr_dsx      = 13    (* Delay slot exception *)
let spr_sr_eph      = 14    (* Exception prefix high *)
let spr_sr_fo       = 15    (* Fixed to one *)
let spr_sr_sumra    = 16    (* SPR user read mode access *)
let spr_sr_reserved = 27,17 (* Reserved *)
let spr_sr_cid      = 31,28 (* Context ID *)

(* Version register - DEPRECATED *)
let spr_vr_rev      = 5,0   (* Revision *)
let spr_vr_uvrp     = 6     (* Updated Version Registers Present *)
let spr_vr_reserved = 15,7  (* Reserved *)
let spr_vr_cfg      = 23,16 (* Configuration Template *)
let spr_vr_ver      = 31,24 (* Version *)


(* Unit Present register *)
let spr_upr_up        = 0
let spr_upr_dcp       = 1
let spr_upr_icp       = 2
let spr_upr_dmp       = 3
let spr_upr_imp       = 4
let spr_upr_mp        = 5
let spr_upr_dup       = 6
let spr_upr_pcup      = 7
let spr_upr_picp      = 8
let spr_upr_pmp       = 9
let spr_upr_ttp       = 10
let spr_upr_reserved  = 23,11
let spr_upr_cup       = 31,24

(* CPU Configuration register *)
let spr_cpucfgr_nsgf     = 3,0 (* Number of shadow GPRs *)
let spr_cpucfgr_cfg      = 4
let spr_cpucfgr_ob32s    = 5
let spr_cpucfgr_ob64s    = 6
let spr_cpucfgr_of32s    = 7
let spr_cpucfgr_of64s    = 8
let spr_cpucfgr_ov64s    = 9
let spr_cpucfgr_nd       = 10 (* No delay-slot implementation *)
let spr_cpucfgr_avrp     = 11 (* Arch. version registers *)
let spr_cpucfgr_evbarp   = 12 (* Exception vector base addr reg *)
let spr_cpucfgr_isrp     = 13 (* Implementation specific regs *)
let spr_cpucfgr_aecsrp   = 14 (* Arith. exception regs *)
let spr_cpucfgr_reserved = 31,15

(* Version register 2 (new with OR1K 1.0) *)
let spr_vr2_ver   = 23,0
let spr_vr2_cpuid = 31,24

(* Architecture Version register *)
let spr_avr_reserved = 7,0
let spr_avr_rev      = 15,8
let spr_avr_min      = 23,16
let spr_avr_maj      = 31,24

(* Exception Vector Base Address register *)
let spr_evbar_reserved = 12,0
let spr_evbar_evba     = 31,13

(* Arithmetic Exception Control register *)
let spr_aecr_cyadde    = 0
let spr_aecr_ovadde    = 1
let spr_aecr_cymule    = 2
let spr_aecr_ovmule    = 3
let spr_aecr_dbze      = 4
let spr_aecr_cymacadde = 5
let spr_aecr_ovmacadde = 6
let spr_aecr_reserved  = 31,7

(* Arithmetic Exception Status register *)
let spr_aesr_cyadde    = 0
let spr_aesr_ovadde    = 1
let spr_aesr_cymule    = 2
let spr_aesr_ovmule    = 3
let spr_aesr_dbze      = 4
let spr_aesr_cymacadde = 5
let spr_aesr_ovmacadde = 6
let spr_aesr_reserved  = 31,7

(* Tick timer registers *)
let spr_ttmr_tp   = 27,0 (* Time period *)
let spr_ttmr_ip   = 28   (* Interrupt pending *)
let spr_ttmr_ie   = 29   (* Interrupt enable *)
let spr_ttmr_m    = 31,30 (* Mode *)
(* Tick timer mode values *)
let spr_ttmr_m_dis = 0b00  (* Disabled *)
let spr_ttmr_m_rst = 0b01  (* Restart-on-match mode *)
let spr_ttmr_m_stp = 0b10  (* Stop-on-match mode *)
let spr_ttmr_m_cnt = 0b11  (* Continue counting mode *)

(* Data Cache Configuration register *)
let spr_dccfgr_ncw    = 2,0 (* Number of Cache Ways *)
let spr_dccfgr_ncs    = 6,3 (* Number of Cache Sets *)
let spr_dccfgr_cbs    = 7   (* Cache Block Size *)
let spr_dccfgr_cws    = 8   (* Cache Write Strategy *)
let spr_dccfgr_ccri   = 9   (* Cache Control Register Implemented *)
let spr_dccfgr_cbiri  = 10  (* Cache Block Invalidate Register Implemented *)
let spr_dccfgr_cbpri  = 11  (* Cache Block Prefetch Register Implemented *)
let spr_dccfgr_cblri  = 12  (* Cache Block Lock Register Implemented *)
let spr_dccfgr_cbfri  = 13  (* Cache Block Flush Register Implemented *)
let spr_dccfgr_cbwbri = 14  (* Cache Block Write-Back Register Implemented *)

(* Instruction Cache Configuration register *)
let spr_iccfgr_ncw   = 2,0 (* Number of Cache Ways *)
let spr_iccfgr_ncs   = 6,3 (* Number of Cache Sets *)
let spr_iccfgr_cbs   = 7   (* Cache Block Size *)
let spr_iccfgr_ccri  = 9   (* Cache Control Register Implemented *)
let spr_iccfgr_cbiri = 10  (* Cache Block Invalidate Register Implemented *)
let spr_iccfgr_cbpri = 11  (* Cache Block Prefetch Register Implemented *)
let spr_iccfgr_cblri = 12  (* Cache Block Lock Register Implemented *)

(* Data MMU Configuration register *)
let spr_dmmufgr_ntw   = 1,0 (* Number of TLB ways *)
let spr_dmmufgr_nts   = 4,2 (* Number of TLB sets *)
let spr_dmmufgr_nae   = 7,5 (* Number of ATB entries *)
let spr_dmmufgr_cri   = 8   (* Control Register Implemented *)
let spr_dmmufgr_pri   = 9   (* Protection Register Implemented *)
let spr_dmmufgr_teiri = 10  (* TLB Entry Invalidate Register Implemented *)
let spr_dmmufgr_htr   = 11  (* Hardware TLB Reload *)

(* Instruction MMU Configuration register *)
let spr_immufgr_ntw   = 1,0 (* Number of TLB ways *)
let spr_immufgr_nts   = 4,2 (* Number of TLB sets *)
let spr_immufgr_nae   = 7,5 (* Number of ATB entries *)
let spr_immufgr_cri   = 8   (* Control Register Implemented *)
let spr_immufgr_pri   = 9   (* Protection Register Implemented *)
let spr_immufgr_teiri = 10  (* TLB Entry Invalidate Register Implemented *)
let spr_immufgr_htr   = 11  (* Hardware TLB Reload *)

(* Debug Mode Register 1 *)
let spr_dmr1_st = 22
let spr_dmr1_bt = 23

(* Debug Stop Register *)
let spr_dsr_rste  = 0
let spr_dsr_busee = 1
let spr_dsr_dpfe  = 2
let spr_dsr_ipfe  = 3
let spr_dsr_tte   = 4
let spr_dsr_ae    = 5
let spr_dsr_iie   = 6
let spr_dsr_inte  = 7
let spr_dsr_dme   = 8
let spr_dsr_ime   = 9
let spr_dsr_re    = 10
let spr_dsr_sce   = 11
let spr_dsr_fpe   = 12
let spr_dsr_te    = 13

let spr_drr_rste  = 0
let spr_drr_busee = 1
let spr_drr_dpfe  = 2
let spr_drr_ipfe  = 3
let spr_drr_tte   = 4
let spr_drr_ae    = 5
let spr_drr_iie   = 6
let spr_drr_ie    = 7
let spr_drr_dme   = 8
let spr_drr_ime   = 9
let spr_drr_re    = 10
let spr_drr_sce   = 11
let spr_drr_fpe   = 12
let spr_drr_te    = 13

(* Implementation-specific SPR defines *)
let mor1kx_spr_sr_width = 16
let mor1kx_spr_sr_reset_value = consti mor1kx_spr_sr_width 0x8001

