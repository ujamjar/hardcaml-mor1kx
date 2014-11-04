(* ORBIS32 opcodes - top 6 bits *)

val insn_width : int
val rd_select : int * int
val ra_select : int * int
val rb_select : int * int
val imm_width : int
val imm_select : int * int
val alu_opc_width : int
val alu_opc_select : int * int

val alu_opc_add : HardCaml.Signal.Comb.t
val alu_opc_addc : HardCaml.Signal.Comb.t
val alu_opc_sub : HardCaml.Signal.Comb.t
val alu_opc_and : HardCaml.Signal.Comb.t
val alu_opc_or : HardCaml.Signal.Comb.t
val alu_opc_xor : HardCaml.Signal.Comb.t
val alu_opc_mul : HardCaml.Signal.Comb.t
val alu_opc_resv : HardCaml.Signal.Comb.t
val alu_opc_shrt : HardCaml.Signal.Comb.t
val alu_opc_div : HardCaml.Signal.Comb.t
val alu_opc_divu : HardCaml.Signal.Comb.t
val alu_opc_mulu : HardCaml.Signal.Comb.t
val alu_opc_extbh : HardCaml.Signal.Comb.t
val alu_opc_extw : HardCaml.Signal.Comb.t
val alu_opc_cmov : HardCaml.Signal.Comb.t
val alu_opc_ffl1 : HardCaml.Signal.Comb.t

val alu_opc_secondary_width : int
val alu_opc_secondary_select : int * int

val alu_opc_secondary_shrt_sll : HardCaml.Signal.Comb.t
val alu_opc_secondary_shrt_srl : HardCaml.Signal.Comb.t
val alu_opc_secondary_shrt_sra : HardCaml.Signal.Comb.t
val alu_opc_secondary_shrt_ror : HardCaml.Signal.Comb.t

val comp_opc_width : int
val comp_opc_select : int * int

val comp_opc_eq : HardCaml.Signal.Comb.t
val comp_opc_ne : HardCaml.Signal.Comb.t
val comp_opc_gtu : HardCaml.Signal.Comb.t
val comp_opc_geu : HardCaml.Signal.Comb.t
val comp_opc_ltu : HardCaml.Signal.Comb.t
val comp_opc_leu : HardCaml.Signal.Comb.t
val comp_opc_gts : HardCaml.Signal.Comb.t
val comp_opc_ges : HardCaml.Signal.Comb.t
val comp_opc_lts : HardCaml.Signal.Comb.t
val comp_opc_les : HardCaml.Signal.Comb.t

val jumpbranch_immediate_select : int * int

val systrapsync_opc_width : int
val systrapsync_opc_select : int * int
val systrapsync_opc_syscall : HardCaml.Signal.Comb.t
val systrapsync_opc_trap : HardCaml.Signal.Comb.t
val systrapsync_opc_msync : HardCaml.Signal.Comb.t
val systrapsync_opc_psync : HardCaml.Signal.Comb.t
val systrapsync_opc_csync : HardCaml.Signal.Comb.t

val opcode_width : int
val opcode_select : int * int

val opcode_j : HardCaml.Signal.Comb.t
val opcode_jal : HardCaml.Signal.Comb.t
val opcode_bnf : HardCaml.Signal.Comb.t
val opcode_bf : HardCaml.Signal.Comb.t
val opcode_nop : HardCaml.Signal.Comb.t
val opcode_movhi : HardCaml.Signal.Comb.t
val opcode_macrc : HardCaml.Signal.Comb.t

val opcode_systrapsync : HardCaml.Signal.Comb.t
val opcode_rfe : HardCaml.Signal.Comb.t

val opcode_jr : HardCaml.Signal.Comb.t
val opcode_jalr : HardCaml.Signal.Comb.t
val opcode_maci : HardCaml.Signal.Comb.t
val opcode_lwa : HardCaml.Signal.Comb.t
val opcode_cust1 : HardCaml.Signal.Comb.t
val opcode_cust2 : HardCaml.Signal.Comb.t
val opcode_cust3 : HardCaml.Signal.Comb.t
val opcode_cust4 : HardCaml.Signal.Comb.t

val opcode_ld : HardCaml.Signal.Comb.t
val opcode_lwz : HardCaml.Signal.Comb.t
val opcode_lws : HardCaml.Signal.Comb.t
val opcode_lbz : HardCaml.Signal.Comb.t
val opcode_lbs : HardCaml.Signal.Comb.t
val opcode_lhz : HardCaml.Signal.Comb.t
val opcode_lhs : HardCaml.Signal.Comb.t

val opcode_addi : HardCaml.Signal.Comb.t
val opcode_addic : HardCaml.Signal.Comb.t
val opcode_andi : HardCaml.Signal.Comb.t
val opcode_ori : HardCaml.Signal.Comb.t
val opcode_xori : HardCaml.Signal.Comb.t
val opcode_muli : HardCaml.Signal.Comb.t
val opcode_mfspr : HardCaml.Signal.Comb.t
val opcode_shrti : HardCaml.Signal.Comb.t

val opcode_sfimm : HardCaml.Signal.Comb.t

val opcode_mtspr : HardCaml.Signal.Comb.t
val opcode_mac : HardCaml.Signal.Comb.t
val opcode_msb : HardCaml.Signal.Comb.t

val opcode_swa : HardCaml.Signal.Comb.t
val opcode_sd : HardCaml.Signal.Comb.t
val opcode_sw : HardCaml.Signal.Comb.t
val opcode_sb : HardCaml.Signal.Comb.t
val opcode_sh : HardCaml.Signal.Comb.t

val opcode_alu : HardCaml.Signal.Comb.t

val opcode_sf : HardCaml.Signal.Comb.t

val opcode_cust5 : HardCaml.Signal.Comb.t
val opcode_cust6 : HardCaml.Signal.Comb.t
val opcode_cust7 : HardCaml.Signal.Comb.t
val opcode_cust8 : HardCaml.Signal.Comb.t

val reset_vector : HardCaml.Signal.Comb.t
val berr_vector : HardCaml.Signal.Comb.t
val dpf_vector : HardCaml.Signal.Comb.t
val ipf_vector : HardCaml.Signal.Comb.t
val tt_vector : HardCaml.Signal.Comb.t
val align_vector : HardCaml.Signal.Comb.t
val illegal_vector : HardCaml.Signal.Comb.t
val int_vector : HardCaml.Signal.Comb.t
val dtlb_vector : HardCaml.Signal.Comb.t
val itlb_vector : HardCaml.Signal.Comb.t
val range_vector : HardCaml.Signal.Comb.t
val syscall_vector : HardCaml.Signal.Comb.t
val fp_vector : HardCaml.Signal.Comb.t
val trap_vector : HardCaml.Signal.Comb.t

val mor1kx_cpuid : HardCaml.Signal.Comb.t
val mor1kx_version_major : HardCaml.Signal.Comb.t
val mor1kx_version_minor : HardCaml.Signal.Comb.t
val mor1kx_pipeid_cappuccino : HardCaml.Signal.Comb.t
val mor1kx_pipeid_espresso : HardCaml.Signal.Comb.t
val mor1kx_pipeid_prontoespresso : HardCaml.Signal.Comb.t

