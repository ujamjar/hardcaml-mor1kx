(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

(* ORBIS32 opcodes - top 6 bits *)

val insn_width : int
val rd_select : int * int
val ra_select : int * int
val rb_select : int * int
val imm_width : int
val imm_select : int * int

module Alu_opc : sig

  val width : int
  val select : int * int

  val add : int
  val addc : int
  val sub : int
  val _and : int
  val _or : int
  val xor : int
  val mul : int
  val resv : int
  val shrt : int
  val div : int
  val divu : int
  val mulu : int
  val extbh : int
  val extw : int
  val cmov : int
  val ffl1 : int

  val secondary_width : int
  val secondary_select : int * int

  val secondary_shrt_sll : int
  val secondary_shrt_srl : int
  val secondary_shrt_sra : int
  val secondary_shrt_ror : int

end

val comp_opc_width : int
val comp_opc_select : int * int

val comp_opc_eq : int
val comp_opc_ne : int
val comp_opc_gtu : int
val comp_opc_geu : int
val comp_opc_ltu : int
val comp_opc_leu : int
val comp_opc_gts : int
val comp_opc_ges : int
val comp_opc_lts : int
val comp_opc_les : int

val jumpbranch_immediate_select : int * int

val systrapsync_opc_width : int
val systrapsync_opc_select : int * int
val systrapsync_opc_syscall : int
val systrapsync_opc_trap : int
val systrapsync_opc_msync : int
val systrapsync_opc_psync : int
val systrapsync_opc_csync : int

module Opcode : sig

  val width : int
  val select : int * int

  val j : int
  val jal : int
  val bnf : int
  val bf : int
  val nop : int
  val movhi : int
  val macrc : int

  val systrapsync : int
  val rfe : int

  val jr : int
  val jalr : int
  val maci : int
  val lwa : int
  val cust1 : int
  val cust2 : int
  val cust3 : int
  val cust4 : int

  val ld : int
  val lwz : int
  val lws : int
  val lbz : int
  val lbs : int
  val lhz : int
  val lhs : int

  val addi : int
  val addic : int
  val andi : int
  val ori : int
  val xori : int
  val muli : int
  val mfspr : int
  val shrti : int

  val sfimm : int

  val mtspr : int
  val mac : int
  val msb : int

  val swa : int
  val sd : int
  val sw : int
  val sb : int
  val sh : int

  val alu : int

  val sf : int

  val cust5 : int
  val cust6 : int
  val cust7 : int
  val cust8 : int

end

val vector_select : int * int
val reset_vector : int
val berr_vector : int
val dpf_vector : int
val ipf_vector : int
val tt_vector : int
val align_vector : int
val illegal_vector : int
val int_vector : int
val dtlb_vector : int
val itlb_vector : int
val range_vector : int
val syscall_vector : int
val fp_vector : int
val trap_vector : int

val mor1kx_cpuid : int
val mor1kx_version_major : int
val mor1kx_version_minor : int
val mor1kx_pipeid_cappuccino : int
val mor1kx_pipeid_espresso : int
val mor1kx_pipeid_prontoespresso : int


