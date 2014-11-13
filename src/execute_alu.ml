(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Description: mor1kx execute stage ALU

  Inputs are opcodes, the immediate field, operands from RF, instruction
  opcode

  Copyright (C) 2012 Julius Baxter <juliusbaxter@gmail.com>
  Copyright (C) 2012-2014 Stefan Kristiansson <stefan.kristiansson@saunalahti.fi>
  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

open Defines
open Utils

module Make(M : Utils.Module_cfg_signal) = struct

  open M
  open Bits
  module L = Utils.Logic(M.Bits)
  open L

  let operand_width = M.o.Option.operand_width

  module I = interface
    clk[1] rst[1]
    padv_decode[1] padv_execute[1] padv_ctrl[1]
    opc_alu[Alu_opc.width] opc_alu_secondary[Alu_opc.width]
    imm16[Defines.imm_width] immediate[operand_width] immediate_sel[1]
    decode_immediate[operand_width] decode_immediate_sel[1] decode_valid[1] decode_op_mul[1]
    op_alu[1] op_add[1] op_mul[1] op_mul_signed[1] op_mul_unsigned[1] op_div[1]
    op_div_signed[1] op_div_unsigned[1] op_shift[1] op_ffl1[1] op_setflag[1] op_mtspr[1]
    op_mfspr[1] op_movhi[1] op_jbr[1] op_jr[1]
    immjbr_upper[10] pc_execute[operand_width]
    adder_do_sub[1] adder_do_carry[1]
    decode_rfa[operand_width] decode_rfb[operand_width]
    rfa[operand_width] rfb[operand_width]
    flag[1] carry[1]
  end

  module O = interface
    flag_set[1] flag_clear[1] carry_set[1] carry_clear[1] overflow_set[1] overflow_clear[1]
    alu_result[operand_width] alu_valid[1]
    mul_result[operand_width] adder_result[operand_width]
    redundant[1]
  end

  (* Adder implementation *)

  type adder = 
    {
      add_carryin : t;
      add_carryout : t;
      add_result : t;
      add_result_sign : t;
      add_signed_overflow : t;
      add_unsigned_overflow : t;
    }

  let adder i a b = 
    let open I in
    let b_neg = ~: b in
    let add_carryin = i.adder_do_sub |: i.adder_do_carry &: i.carry in
    let b_mux = mux2 i.adder_do_sub b_neg b in
    let add_carryout, add_result = 
      let add = ue a +: ue b_mux +: uresize add_carryin (operand_width+1) in
      msb add, lsbs add
    in
    let add_result_sign = msb add_result in
    let add_signed_overflow = (msb a ==: msb b_mux) &: (msb a ^: add_result_sign) in
    let add_unsigned_overflow = add_carryout in
    { add_carryin; add_carryout; add_result; add_result_sign; 
      add_signed_overflow; add_unsigned_overflow }

  (* Multiplier implementations *)

  type mul = 
    {
      mul_valid : t;
      mul_result : t;
      mul_unsigned_overflow : t;
      mul_signed_overflow : t;
    }

  let def_mul = 
    { 
      mul_valid=vdd; mul_result=zero operand_width; 
      mul_unsigned_overflow=gnd; mul_signed_overflow=gnd 
    }

  let multiplier_dp i e0 e1 e2 a b = 
    let open I in
    let module R = Regs(struct let clk = i.clk let rst = i.rst end) in
    let mul_opa, mul_opb = R.reg ~e:e0 a,  R.reg ~e:e0 b in
    let mul_result1 = sel_bottom (R.reg ~e:e1 (mul_opa *: mul_opb)) operand_width in
    R.reg ~e:e2 mul_result1

  let multiplier_signed_overflow a b m = 
    let a, b, mul = msb a, msb b, msb m.mul_result in
    let mul_signed_overflow = ((a ==: b) &: mul) |: ((a ^: b) &: (~: mul)) in
    { m with mul_signed_overflow }

  let multiplier_threestage i a b = 
    let open I in
    let module R = Regs(struct let clk = i.clk let rst = i.rst end) in
    let mul_result = multiplier_dp i i.op_mul vdd vdd a b in
    let mul_valid_shr = R.reg_fb ~e:vdd ~w:3 
      (fun mul_valid_shr ->
        mux2 i.decode_valid (uresize i.op_mul 3) (lsbs mul_valid_shr @: gnd))
    in
    let mul_valid = msb mul_valid_shr &: (~: (i.decode_valid)) in 
    multiplier_signed_overflow a b { def_mul with mul_valid; mul_result }

  let multiplier_pipelined i a b = 
    let open I in
    let mul_result = multiplier_dp i (i.decode_op_mul &: i.padv_decode) i.padv_execute vdd a b in
    { def_mul with mul_result }

  let multiplier_serial i a b = 
    let open I in
    let open Utils in
    let open HardCaml.Signal.Guarded in
    let module R = Regs(struct let clk = i.clk let rst = i.rst end) in

    let mul_a = mux2 (i.op_mul_signed &: msb a) (~: a +:. 1) a in
    let mul_b = mux2 (i.op_mul_signed &: msb b) (~: b +:. 1) a in

    let ow = operand_width in
    let prod = R.g_reg ~e:vdd (ow * 2) in
    let mul_done = R.g_reg ~e:vdd 1 in
    let cnt = R.g_reg ~e:vdd 6 in
    let () = compile [
      g_if (cnt#q <>:. 0) [
        cnt $== cnt#q -:. 1;
        g_if (lsb prod#q) [
          (prod,ow-1) $==\ (ue (prod#q.[ow*2-1:ow])) +: mul_a;
        ] [
          (prod,ow-1) $==\ ue (prod#q.[ow*2-1:ow]);
        ];
        (prod,0) $==\ prod#q.[ow-1:1];
        g_when (cnt#q ==:. 1) [
          mul_done $==. 1;
        ]
      ] @@ g_elif (i.decode_valid &: i.op_mul) [
        prod $== (zero 32 @: mul_b);
        mul_done $==. 0;
        cnt $==. 32;
      ] @@ g_elif i.decode_valid [
        mul_done $==. 0;
      ] []
    ] in

    let mul_valid = mul_done#q &: ~: (i.decode_valid) in
    let mul_result = 
      let prod = sel_bottom prod#q ow in
      mux2 i.op_mul_signed
        (mux2 (msb a &: msb b) ((~: prod) +:. 1) prod)
        prod
    in
    let mul_unsigned_overflow = 
      if operand_width = 64 then gnd
      else reduce (|:) @@ bits @@ sel_top prod#q ow
    in
    multiplier_signed_overflow a b { def_mul with mul_result; mul_valid; mul_unsigned_overflow }

  let multiplier_sim i a b =
    let full_result = a *: b in
    let mul_result = sel_bottom full_result operand_width in
    let mul_unsigned_overflow = 
      if operand_width=64 then gnd
      else reduce (|:) @@ bits @@ sel_top full_result operand_width
    in
    multiplier_signed_overflow a b { def_mul with mul_result; mul_unsigned_overflow }

  let multiplier_none = def_mul 

  (* divider *)

  type div = 
    {
      div_result : t;
      div_valid : t;
      div_by_zero : t;
    }

  let def_div = { div_result=zero operand_width; div_valid=vdd; div_by_zero=gnd }

  let divider_serial i a b = 
    let open I in
    let module R = Regs(struct let clk = i.clk let rst = i.rst end) in
    let div_count = R.g_reg ~e:vdd 6 in
    let div_n = R.g_reg ~e:vdd operand_width in
    let div_r = R.g_reg ~e:vdd operand_width in
    let div_d = R.g_reg ~e:vdd operand_width in
    let div_neg = R.g_reg ~e:vdd 1 in
    let div_done = R.g_reg ~e:vdd 1 in
    let div_by_zero = R.g_reg ~e:vdd 1 in
    let div_sub = ((sel_bottom div_r#q (operand_width-1)) @: (msb div_n#q)) -: div_d#q in
    let open HardCaml.Signal.Guarded in
    let () = compile [

      g_if (i.decode_valid &: i.op_div) [
        div_done $==. 0;
        div_count $==. operand_width;
      ] @@ g_elif (div_count#q ==:. 1) [
        div_done $==. 1;
      ] @@ g_elif (~: (div_done#q)) [
        div_count $== div_count#q -:. 1;
      ] [];

      g_if (i.decode_valid &: i.op_div) [

        div_n $== a;
        div_d $== b;
        div_r $==. 0;
        div_neg $==. 0;
        div_by_zero $== (~: (reduce (|:) @@ bits b));

        g_when i.op_div_signed [
          g_when ((msb a) ^: (msb b)) [ div_neg $==. 1; ];
          g_when (msb a) [ div_n $== (~: a) +:. 1; ];
          g_when (msb b) [ div_d $== (~: b) +:. 1; ];
        ]

      ] [
        g_when (~: (div_done#q)) [
          g_if (~: (msb div_sub)) [
            div_r $== sel_bottom div_sub operand_width;
            div_n $== sel_bottom div_n#q (operand_width-1) @: vdd;
          ] [
            div_r $== sel_bottom div_r#q (operand_width-1) @: (msb div_n#q);
            div_n $== sel_bottom div_n#q (operand_width-1) @: gnd;
          ]
        ]
      ];

    ] in
    {
      div_valid = div_done#q &: (~: (i.decode_valid));
      div_result = mux2 div_neg#q ((~: (div_n#q)) +:. 1) div_n#q;
      div_by_zero = div_by_zero#q;
    }

  let divider_sim = def_div (* we dont have an appropriate divider for this at the moment *)
  let divider_none = def_div

  (* ffl1 *)

  type ffl1 = 
    {
      ffl1_result : t;
      ffl1_valid : t;
    }

  let def_ffl1 = { ffl1_result=zero operand_width; ffl1_valid=vdd }

  let ffl1 i a = 
    let open I in
    let op = bit i.opc_alu_secondary 2 in
    let rec ffl1 i = function
      | [] -> zero 6
      | h::t -> mux2 h (consti 6 i) (ffl1 (i+1) t)
    in
    uresize (mux2 op (ffl1 1 @@ bits a) (ffl1 1 @@ (List.rev @@ bits a))) operand_width

  let ffl1_none = def_ffl1
  let ffl1_comb i a = { def_ffl1 with ffl1_result = ffl1 i a }
  let ffl1_registered i a = 
    let open I in
    let module R = Regs(struct let clk = i.clk let rst = i.rst end) in
    { ffl1_result = R.reg ~e:i.decode_valid (ffl1 i a); 
      ffl1_valid = ~: (i.decode_valid) }

  (* shifter *)

  type shift = 
    {
      shift_result : t;
      shift_valid : t;
    }

  let def_shift = { shift_result=zero operand_width; shift_valid=vdd }

  let shifter_barrel f i a b = 
    let open I in
    let opc_alu_shr = i.opc_alu_secondary.[Alu_opc.secondary_width-1:0] in
    let op_sll = opc_alu_shr ==:. Alu_opc.secondary_shrt_sll in
    (*let op_srl = opc_alu_shr ==:. Alu_opc.secondary_shrt_srl in*)
    let op_sra = opc_alu_shr ==:. Alu_opc.secondary_shrt_sra in
    let op_ror = opc_alu_shr ==:. Alu_opc.secondary_shrt_ror in

    let shift_lsw = mux2 op_sll (reverse a) a in
    let shift_msw = 
      let msbs = repeat (msb a) operand_width in
      let zero = zero operand_width in
      match f.Option.sra, f.Option.ror with
      | true, true -> mux2 op_sra msbs (mux2 op_ror a zero)
      | true, false -> mux2 op_sra msbs zero
      | false, true -> mux2 op_ror a msbs
      | false, false -> zero
    in
    
    let shift_right = log_shift srl (shift_msw @: shift_lsw) (sel_bottom b 5) in
    let shift_result = mux2 op_sll (reverse shift_right) shift_right in
    let shift_result = sel_bottom shift_result operand_width in

    { def_shift with shift_result }

  let shifter_serial f i a b = 
    let open I in
    let module R = Regs(struct let clk = i.clk let rst = i.rst end) in
    let opc_alu_shr = i.opc_alu_secondary.[Alu_opc.secondary_width-1:0] in
    let cnt = R.g_reg ~e:vdd 1 in
    let go = R.g_reg ~e:vdd 5 in
    let result = R.g_reg ~e:vdd operand_width in
    let loop_limit = cnt#q ==: b.[4:0] in
    let open HardCaml.Signal.Guarded in
    let () = compile [
      g_when i.decode_valid [
        go $== i.op_shift;
      ];

      g_if (i.decode_valid &: i.op_shift) [
        cnt $==. 0;
        result $== a;
      ] @@ g_elif (go#q &: (~: loop_limit)) [
        cnt $== cnt#q +:. 1;
        g_when (opc_alu_shr ==:. Alu_opc.secondary_shrt_srl) [
          result $== (vdd @: drop_bottom result#q 1);
        ];
        g_when (opc_alu_shr ==:. Alu_opc.secondary_shrt_sll) [
          result $== (drop_top result#q 1 @: gnd);
        ];
        g_proc (if f.Option.ror then [] else [
          g_when (opc_alu_shr ==:. Alu_opc.secondary_shrt_ror) [
            result $== (lsb result#q @: drop_bottom result#q 1);
          ];
        ]);
        g_proc (if f.Option.sra then [] else [
          g_when (opc_alu_shr ==:. Alu_opc.secondary_shrt_sra) [
            result $== (msb a @: drop_bottom result#q 1);
          ];
        ]);
      ] [];

    ] in
    { shift_result = result#q; shift_valid = loop_limit &: go#q &: (~: (i.decode_valid)); }

  (* execute unit *)

  let execute_alu ~calculate_branch_dest i = 
    let open I in

    let a, b = 
      if calculate_branch_dest then
        (mux2 (i.op_jbr |: i.op_jr) i.pc_execute i.rfa),
        (mux2 i.immediate_sel 
          i.immediate
          (mux2 i.op_jbr 
            (repeat (msb i.immjbr_upper) 4 @: i.immjbr_upper @: i.imm16 @: zero 2)
            i.rfb))
      else
        i.rfa, (mux2 i.immediate_sel i.immediate i.rfb)
    in

    let decode_a, decode_b = 
      i.decode_rfa, (mux2 i.decode_immediate_sel i.decode_immediate i.decode_rfb)
    in
  
    (* add/sub inputs *)
    let adder = adder i a b in

    (* multiplier *)
    let multiplier = 
      match f.Option.multiplier with
      | Option.Multiplier_none -> multiplier_none
      | Option.Multiplier_simulation -> multiplier_sim i a b
      | Option.Multiplier_pipelined -> multiplier_pipelined i decode_a decode_b
      | Option.Multiplier_threestage -> multiplier_threestage i a b
      | Option.Multiplier_serial -> multiplier_serial i a b
    in

    (* divider *)
    let divider = 
      match f.Option.divider with
      | Option.Divider_none -> divider_none
      | Option.Divider_simulation -> divider_sim
      | Option.Divider_serial -> divider_serial i i.rfa i.rfb
    in

    (* ffl1 *)
    let ffl1 = 
      match f.Option.ffl1 with
      | Option.Ffl1_none -> ffl1_none
      | Option.Ffl1_comb -> ffl1_comb i a
      | Option.Ffl1_registered -> ffl1_registered i a
    in

    let shifter = 
      match o.Option.shifter with
      | Option.Shifter_barrel -> shifter_barrel f i a b
      | Option.Shifter_serial -> shifter_serial f i a b
    in

    (* comparisons *)
    let a_eq_b = a ==: b in
    let a_lts_b = ~: (adder.add_result_sign ==: adder.add_signed_overflow) in
    let a_ltu_b = ~: (adder.add_carryout) in

    let flag_set = 
      cases i.opc_alu_secondary gnd [
        comp_opc_eq, a_eq_b;
        comp_opc_ne, ~: a_eq_b;
        comp_opc_gtu, ~: (a_eq_b |: a_ltu_b);
        comp_opc_gts, ~: (a_eq_b |: a_lts_b);
        comp_opc_geu, ~: a_ltu_b;
        comp_opc_ges, ~: a_lts_b;
        comp_opc_ltu, a_ltu_b;
        comp_opc_lts, a_lts_b;
        comp_opc_leu, a_eq_b |: a_ltu_b;
        comp_opc_les, a_eq_b |: a_lts_b;
      ]
    in
    let flag_set = flag_set &: i.op_setflag in
    let flag_clear = (~: flag_set) &: i.op_setflag in

    (* logic operations *)
    (* note; rtl version is perhaps a bit better than this 
    *       it tries to map directly into luts XXX fixme *)
    let logic_result, op_logic = 
      let _and = a &: b in
      let _or = a |: b in
      let xor = a ^: b in
      let c0 = i.op_alu in
      let c1 = i.op_mfspr |: i.op_mtspr in
      
      let logic_result = 
        cases i.opc_alu (zero operand_width) [
          Alu_opc._and, _and; Alu_opc._or, _or; Alu_opc.xor, xor;
        ]
      in
      let logic_result = mux2 c0 logic_result (zero operand_width) in
      let logic_result = mux2 c1 _or logic_result in
      
      let op_logic = cases i.opc_alu gnd [
          Alu_opc._and, vdd; Alu_opc._or, vdd; Alu_opc.xor, vdd;
      ] in
      let op_logic = mux2 c0 op_logic gnd in
      let op_logic = mux2 c1 vdd op_logic in
      logic_result, op_logic
    in

    let cmov_result, op_cmov = 
      if f.Option.cmov then 
        (mux2 i.flag a b), (i.op_alu &: (i.opc_alu ==:. Alu_opc.cmov))
      else 
        (zero operand_width), gnd
    in

    assert (width logic_result = operand_width);
    assert (width cmov_result = operand_width);
    assert (width multiplier.mul_result = operand_width);
    assert (width shifter.shift_result = operand_width);
    assert (width divider.div_result = operand_width);
    assert (width ffl1.ffl1_result = operand_width);
    assert (width adder.add_result = operand_width);

    (* result mux *)
    let alu_result = 
      mux2 op_logic logic_result @@
      mux2 op_cmov cmov_result @@
      mux2 i.op_movhi i.immediate @@
      mux2 i.op_mul multiplier.mul_result @@
      mux2 i.op_shift shifter.shift_result @@
      mux2 i.op_div divider.div_result @@
      mux2 i.op_ffl1 ffl1.ffl1_result @@
      adder.add_result
    in

    (* carry and overflow flags *)
    let overflow_set = 
      if f.Option.overflow then
        i.op_add &: adder.add_signed_overflow |: 
        i.op_mul_signed &: multiplier.mul_signed_overflow |:
        i.op_div_signed &: divider.div_by_zero
      else gnd
    in

    let overflow_clear = 
      if f.Option.overflow then
        i.op_add &: ~: (adder.add_signed_overflow) |: 
        i.op_mul_signed &: ~: (multiplier.mul_signed_overflow) |:
        i.op_div_signed &: ~: (divider.div_by_zero)
      else gnd
    in

    let carry_set = 
      if f.Option.carry_flag then
        i.op_add &: adder.add_unsigned_overflow |: 
        i.op_mul_unsigned &: multiplier.mul_unsigned_overflow |:
        i.op_div_unsigned &: divider.div_by_zero
      else gnd
    in

    let carry_clear = 
      if f.Option.carry_flag then
        i.op_add &: ~: (adder.add_unsigned_overflow) |: 
        i.op_mul_unsigned &: ~: (multiplier.mul_unsigned_overflow) |:
        i.op_div_unsigned &: ~: (divider.div_by_zero)
      else gnd
    in

    (* stall logic *)
    let alu_stall = 
      i.op_div   &: ~: (divider.div_valid) |:
      i.op_mul   &: ~: (multiplier.mul_valid) |:
      i.op_shift &: ~: (shifter.shift_valid) |:
      i.op_ffl1  &: ~: (ffl1.ffl1_valid) 
    in

    let alu_valid = ~: alu_stall in

    O.({
      flag_set;
      flag_clear;
      carry_set;
      carry_clear;
      overflow_set;
      overflow_clear;
      alu_result;
      alu_valid;
      mul_result = multiplier.mul_result;
      adder_result = adder.add_result;
      redundant = List.fold_left (&:) gnd @@ List.map lsb @@ I.to_list i;
    })

  module Inst = M.Inst(I)(O)
  let execute_alu_inst ~calculate_branch_dest = Inst.inst "execute_alu" 
    (execute_alu ~calculate_branch_dest)

end

