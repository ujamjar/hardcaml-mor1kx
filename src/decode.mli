(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module Make(M : Utils.Module_cfg) : sig

  module I : interface
    insn 
  end

  module O : interface
    opc_alu opc_alu_secondary
    imm16 immediate immediate_sel
    immjbr_upper
    rfd_adr rfa_adr rfb_adr rf_wb
    op_jbr op_jr op_jal op_bf op_bnf op_brcond op_branch
    op_alu op_lsu_load op_lsu_store op_lsu_atomic
    lsu_length lsu_zext
    op_mfspr op_mtspr op_rfe op_setflag op_add
    op_mul op_mul_signed op_mul_unsigned
    op_div op_div_signed op_div_unsigned
    op_shift op_ffl1
    op_movhi
    adder_do_sub adder_do_carry
    except_illegal except_syscall except_trap
    opc_insn
  end

  val decode : M.Bits.t I.t -> M.Bits.t O.t
  val decode_inst : M.Bits.t I.t -> M.Bits.t O.t

end
