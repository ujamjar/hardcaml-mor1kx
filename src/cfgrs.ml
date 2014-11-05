(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Description: mor1kx SPRs indicating configuration and version

  All registers are read only and configured at synthesis time.

  Note that the outputs do not have the usual "_o" prefix on the port names
  as this module is intended to be instantiated without a Verilog-mode
  AUTO_TEMPLATE, and as the module is providing read-only signals, there's
  no confusion about the direction of the ports.

  Copyright (C) 2012 Authors

  Author(s): Julius Baxter <juliusbaxter@gmail.com>

***************************************************************************** *)

open HardCaml.Signal.Comb
open Option
open Spr
open Utils

module O = interface
    vr[32] vr2[32] upr[32] cpucfgr[32] dmmucfgr[32] immucfgr[32]
    dccfgr[32] iccfgr[32] dcfgr[32] pccfgr[32] fpcsr[32] avr[32]
end

let cfgrs o f =
  let vr = mk_spr "vr" [ 
    VI(vr_rev, 0); BI(vr_uvrp, 1); VI(vr_reserved, 0); 
    VI(vr_cfg, 0); VI(vr_ver, 0x10) 
  ] in

  let vr2 = Defines.(mor1kx_cpuid @: mor1kx_version_major @: mor1kx_version_minor @: zero 8) in

  let upr = mk_spr "upr" [
    BI(upr_up, 1); BB(upr_dcp, f.datacache); BB(upr_icp, f.instructioncache);
    BB(upr_dmp, f.dmmu); BB(upr_imp, f.immu); BB(upr_mp, f.mac); BB(upr_dup, f.debugunit);
    BB(upr_pcup, f.perfcounters); BB(upr_picp, f.pic); BB(upr_pmp, f.pmu);
    BB(upr_ttp, f.timer); VI(upr_reserved, 0); VI(upr_cup, 0);
  ] in

  let cpucfgr = mk_spr "cpucfgr" [
   VI(cpucfgr_nsgf, o.rf_num_shadow_gpr); BI(cpucfgr_cfg, 0); BI(cpucfgr_ob32s, 1);
   BI(cpucfgr_ob64s, 0); BI(cpucfgr_of32s, 0); BI(cpucfgr_of64s, 0);
   BI(cpucfgr_ov64s, 0); BB(cpucfgr_nd, not f.delayslot); BI(cpucfgr_avrp, 1);
   BB(cpucfgr_evbarp, f.evbar); BI(cpucfgr_isrp, 1); BB(cpucfgr_aecsrp, f.aecsr);
   VI(cpucfgr_reserved, 0);
  ] in

  let dmmucfgr = mk_spr "dmmucfgr" [
   VI((31,12), 0); BI(dmmufgr_htr, 0); BI(dmmufgr_teiri, 0); BI(dmmufgr_pri, 0);
   BI(dmmufgr_cri, 0); VI(dmmufgr_nae, 0);
   VI(dmmufgr_nts, if f.dmmu then o.dmmu_set_width else 0);
   VI(dmmufgr_ntw, if f.dmmu then o.dmmu_ways-1 else 0);
  ] in

  let immucfgr = mk_spr "immucfgr" [
   VI((31,12), 0); BI(immufgr_htr, 0); BI(immufgr_teiri, 0);
   BI(immufgr_pri, 0); BI(immufgr_cri, 0); VI(immufgr_nae, 0);
   VI(immufgr_nts, if f.immu then o.immu_set_width else 0);
   VI(immufgr_ntw, if f.immu then o.immu_ways-1 else 0);
  ] in
  
  let rlog b x = 
    if b then
      match x with
      | 1  -> 0 | 2  -> 1
      | 4  -> 2 | 8  -> 3
      | 16 -> 4 | 32 -> 5
      | _ -> 0
    else 0
  in

  let dccfgr = mk_spr "dccfgr" [
   VI((31,15), 0); BI(dccfgr_cbwbri, 0); BB(dccfgr_cbfri, f.datacache);
   BI(dccfgr_cblri, 0); BI(dccfgr_cbpri, 0); BB(dccfgr_cbiri, f.datacache);
   BI(dccfgr_ccri, 0); BI(dccfgr_cws, 0);
   BI(dccfgr_cbs, if f.datacache && o.dcache_block_width == 5 then 1 else 0);
   VI(dccfgr_ncs, if f.datacache then o.dcache_set_width else 0);
   VI(dccfgr_ncw, rlog f.datacache o.dcache_ways);
  ] in

  let iccfgr = mk_spr "iccfgr" [
   VI((31,13), 0); BI(8, 0); BI(iccfgr_cblri, 0); BI(iccfgr_cbpri, 0);
   BB(iccfgr_cbiri, f.instructioncache); BI(iccfgr_ccri, 0);
   BI(iccfgr_cbs, if f.instructioncache && o.icache_block_width == 5 then 1 else 0);
   VI(iccfgr_ncs, if f.instructioncache then  o.icache_set_width else 0);
   VI(iccfgr_ncw, rlog f.instructioncache o.icache_ways);
  ] in

  let avr = mk_spr "avr" [ 
    VI(avr_maj, 1); VI(avr_min, 1); VI(avr_rev, 0); VI(avr_reserved, 0) ] 
  in

  let dcfgr = zero 32 in
  let pccfgr = zero 32 in
  let fpcsr = zero 32 in

  O.({
    vr; vr2; upr; cpucfgr; dmmucfgr; immucfgr; dccfgr;
    iccfgr; dcfgr; pccfgr; fpcsr; avr;
  })

