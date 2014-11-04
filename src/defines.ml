open HardCaml.Signal.Comb

(* ORBIS32 opcodes - top 6 bits *)

let insn_width = 32
let rd_select = 25,21
let ra_select = 20,16
let rb_select = 15,11
let imm_width = 16
let imm_select = 15,0
let alu_opc_width = 4
let alu_opc_select = 3,0

let alu_opc_add    = consti alu_opc_width 0x0
let alu_opc_addc   = consti alu_opc_width 0x1
let alu_opc_sub    = consti alu_opc_width 0x2
let alu_opc_and    = consti alu_opc_width 0x3
let alu_opc_or     = consti alu_opc_width 0x4
let alu_opc_xor    = consti alu_opc_width 0x5
let alu_opc_mul    = consti alu_opc_width 0x6
let alu_opc_resv   = consti alu_opc_width 0x7
let alu_opc_shrt   = consti alu_opc_width 0x8
let alu_opc_div    = consti alu_opc_width 0x9
let alu_opc_divu   = consti alu_opc_width 0xa
let alu_opc_mulu   = consti alu_opc_width 0xb
let alu_opc_extbh  = consti alu_opc_width 0xc
let alu_opc_extw   = consti alu_opc_width 0xd
let alu_opc_cmov   = consti alu_opc_width 0xe
let alu_opc_ffl1   = consti alu_opc_width 0xf

let alu_opc_secondary_width = 3
let alu_opc_secondary_select = 8,6

let alu_opc_secondary_shrt_sll = consti alu_opc_secondary_width 0x0
let alu_opc_secondary_shrt_srl = consti alu_opc_secondary_width 0x1
let alu_opc_secondary_shrt_sra = consti alu_opc_secondary_width 0x2
let alu_opc_secondary_shrt_ror = consti alu_opc_secondary_width 0x3

let comp_opc_width = 4
let comp_opc_select = 24,21
let comp_opc_eq  = consti comp_opc_width 0x0
let comp_opc_ne  = consti comp_opc_width 0x1
let comp_opc_gtu = consti comp_opc_width 0x2
let comp_opc_geu = consti comp_opc_width 0x3
let comp_opc_ltu = consti comp_opc_width 0x4
let comp_opc_leu = consti comp_opc_width 0x5
let comp_opc_gts = consti comp_opc_width 0xA
let comp_opc_ges = consti comp_opc_width 0xB
let comp_opc_lts = consti comp_opc_width 0xC
let comp_opc_les = consti comp_opc_width 0xD

let jumpbranch_immediate_select = 25,0

let systrapsync_opc_width = 3
let systrapsync_opc_select = 25,23
let systrapsync_opc_syscall = consti systrapsync_opc_width 0x0
let systrapsync_opc_trap = consti systrapsync_opc_width 0x2
let systrapsync_opc_msync = consti systrapsync_opc_width 0x4
let systrapsync_opc_psync = consti systrapsync_opc_width 0x5
let systrapsync_opc_csync = consti systrapsync_opc_width 0x6

let opcode_width = 6
let opcode_select = 31,26

let opcode_j       = consti 6 0x0
let opcode_jal     = consti 6 0x1
let opcode_bnf     = consti 6 0x3
let opcode_bf      = consti 6 0x4
let opcode_nop     = consti 6 0x5
let opcode_movhi   = consti 6 0x6
let opcode_macrc   = consti 6 0x6

let opcode_systrapsync = consti 6 0x8
let opcode_rfe         = consti 6 0x9

let opcode_jr      = consti 6 0x11
let opcode_jalr    = consti 6 0x12
let opcode_maci    = consti 6 0x13
let opcode_lwa     = consti 6 0x1b
let opcode_cust1   = consti 6 0x1c
let opcode_cust2   = consti 6 0x1d
let opcode_cust3   = consti 6 0x1e
let opcode_cust4   = consti 6 0x1f

let opcode_ld      = consti 6 0x20
let opcode_lwz     = consti 6 0x21
let opcode_lws     = consti 6 0x22
let opcode_lbz     = consti 6 0x23
let opcode_lbs     = consti 6 0x24
let opcode_lhz     = consti 6 0x25
let opcode_lhs     = consti 6 0x26

let opcode_addi    = consti 6 0x27
let opcode_addic   = consti 6 0x28
let opcode_andi    = consti 6 0x29
let opcode_ori     = consti 6 0x2a
let opcode_xori    = consti 6 0x2b
let opcode_muli    = consti 6 0x2c
let opcode_mfspr   = consti 6 0x2d
let opcode_shrti   = consti 6 0x2e

let opcode_sfimm   = consti 6 0x2f

let opcode_mtspr   = consti 6 0x30
let opcode_mac     = consti 6 0x31
let opcode_msb     = consti 6 0x31

let opcode_swa     = consti 6 0x33
let opcode_sd      = consti 6 0x34
let opcode_sw      = consti 6 0x35
let opcode_sb      = consti 6 0x36
let opcode_sh      = consti 6 0x37

let opcode_alu     = consti 6 0x38

let opcode_sf      = consti 6 0x39

let opcode_cust5   = consti 6 0x3c
let opcode_cust6   = consti 6 0x3d
let opcode_cust7   = consti 6 0x3e
let opcode_cust8   = consti 6 0x3f

(* OR1K SPR defines *)
(*`INCLUDE "MOR1KX-SPRS.V"*)

(* Exception addresses *)
let reset_vector    = consti 5 0x01
let berr_vector     = consti 5 0x02
let dpf_vector      = consti 5 0x03
let ipf_vector      = consti 5 0x04
let tt_vector       = consti 5 0x05
let align_vector    = consti 5 0x06
let illegal_vector  = consti 5 0x07
let int_vector      = consti 5 0x08
let dtlb_vector     = consti 5 0x09
let itlb_vector     = consti 5 0x0A
let range_vector    = consti 5 0x0B
let syscall_vector  = consti 5 0x0C
let fp_vector       = consti 5 0x0D
let trap_vector     = consti 5 0x0E

(* Whether we'll allow things using AYNC reset to have it:
   define OR_ASYNC_RST or posedge rst *)
(*let or_async_rst *)

(* Implementation version defines *)
let mor1kx_cpuid = consti 8 0x01
(* mor1kx breaks up the VR2 version register to be 3 8-bit fields
   MSB is major version, middle byte is minor version number
   and final byte is the pipeline identifier *)
let mor1kx_version_major = consti 8 3
let mor1kx_version_minor = consti 8 0

(* mor1kx implementation-specific register definitions *)
let mor1kx_pipeid_cappuccino = consti 8 1
let mor1kx_pipeid_espresso   = consti 8 2
let mor1kx_pipeid_prontoespresso = consti 8 3

