open HardCaml.Signal.Comb

(* ORBIS32 opcodes - top 6 bits *)

let insn_width = 32
let rd_select = 25,21
let ra_select = 20,16
let rb_select = 15,11
let imm_width = 16
let imm_select = 15,0

module Alu_opc = struct

  let width = 4
  let select = 3,0

  let add    = 0x0
  let addc   = 0x1
  let sub    = 0x2
  let _and    = 0x3
  let _or     = 0x4
  let xor    = 0x5
  let mul    = 0x6
  let resv   = 0x7
  let shrt   = 0x8
  let div    = 0x9
  let divu   = 0xa
  let mulu   = 0xb
  let extbh  = 0xc
  let extw   = 0xd
  let cmov   = 0xe
  let ffl1   = 0xf

  let secondary_width = 3
  let secondary_select = 8,6

  let secondary_shrt_sll = 0x0
  let secondary_shrt_srl = 0x1
  let secondary_shrt_sra = 0x2
  let secondary_shrt_ror = 0x3

end

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
let systrapsync_opc_syscall = 0x0
let systrapsync_opc_trap    = 0x2
let systrapsync_opc_msync   = 0x4
let systrapsync_opc_psync   = 0x5
let systrapsync_opc_csync   = 0x6

module Opcode = struct

  let width = 6
  let select = 31,26

  let j       = 0x0
  let jal     = 0x1
  let bnf     = 0x3
  let bf      = 0x4
  let nop     = 0x5
  let movhi   = 0x6
  let macrc   = 0x6

  let systrapsync = 0x8
  let rfe         = 0x9

  let jr      = 0x11
  let jalr    = 0x12
  let maci    = 0x13
  let lwa     = 0x1b
  let cust1   = 0x1c
  let cust2   = 0x1d
  let cust3   = 0x1e
  let cust4   = 0x1f

  let ld      = 0x20
  let lwz     = 0x21
  let lws     = 0x22
  let lbz     = 0x23
  let lbs     = 0x24
  let lhz     = 0x25
  let lhs     = 0x26

  let addi    = 0x27
  let addic   = 0x28
  let andi    = 0x29
  let ori     = 0x2a
  let xori    = 0x2b
  let muli    = 0x2c
  let mfspr   = 0x2d
  let shrti   = 0x2e

  let sfimm   = 0x2f

  let mtspr   = 0x30
  let mac     = 0x31
  let msb     = 0x31

  let swa     = 0x33
  let sd      = 0x34
  let sw      = 0x35
  let sb      = 0x36
  let sh      = 0x37

  let alu     = 0x38

  let sf      = 0x39

  let cust5   = 0x3c
  let cust6   = 0x3d
  let cust7   = 0x3e
  let cust8   = 0x3f

end

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

