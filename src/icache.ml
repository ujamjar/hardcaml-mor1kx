(******************************************************************************
 This Source Code Form is subject to the terms of the
 Open Hardware Description License, v. 1.0. If a copy
 of the OHDL was not distributed with this file, You
 can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

 Description: Instruction cache implementation

 Copyright (C) 2012-2014
    Stefan Kristiansson <stefan.kristiansson@saunalahti.fi>
    Stefan Wallentowitz <stefan.wallentowitz@tum.de>
    Andy Ray <andy.ray@ujamjar.com>

 ******************************************************************************)

module Make(M : Utils.Module_cfg_signal) = struct

  open M.Bits
  open HardCaml.Signal.Guarded 
  open Utils
  open Option
  module Sel = Utils.Sel(M.Bits)
  open Sel

  let insn_width = Defines.insn_width

  module I = interface
      clk[1]
      rst[1]
      ic_access[1]
      cpu_adr[M.o.operand_width]
      cpu_adr_match[M.o.operand_width]
      cpu_req[1]
      wradr[M.o.operand_width]
      wrdat[insn_width]
      we[1]
      spr_bus_addr[16]
      spr_bus_we[1]
      spr_bus_stb[1]
      spr_bus_dat_i[M.o.operand_width]
  end


  module O = interface
      refill[1]
      refill_req[1]
      refill_done[1]
      invalidate[1]
      cpu_ack[1]
      cpu_dat[insn_width]
      spr_bus_dat_o[M.o.operand_width]
      spr_bus_ack[1]
  end

  type sm = Idle | Read | Refill | Invalidate
    deriving(Enum, Bounded)

  module Way = interface
    raddr waddr din
    check_tag check_match check_valid
    hit
    tag_din tag_way_out
  end

  let icache i = 
    let open I in
    let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in

    let way_width = M.o.icache_block_width + M.o.icache_set_width in
    (*
    * Tag memory layout
    *            +---------------------------------------------------------+
    * (index) -> | LRU | wayN valid | wayN tag |...| way0 valid | way0 tag |
    *            +---------------------------------------------------------+
    *)

    (* The tag is the part left of the index *)
    let tag_width = (M.o.icache_limit_width - way_width) in

    (* The tag memory contains entries with OPTION_ICACHE_WAYS parts of
      each TAGMEM_WAY_WIDTH. Each of those is tag and a valid flag. *)
    let tagmem_way_width = tag_width + 1 in
    let tagmem_way_valid = tagmem_way_width - 1 in

    (* Additionally, the tag memory entry contains an LRU value. The
      width of this is actually 0 for OPTION_ICACHE_LIMIT_WIDTH==1 *)
    let tag_lru_width = M.o.icache_ways*(M.o.icache_ways-1) / 2 in

    (* We have signals for the LRU which are not used for one way
      caches. To avoid signal width [-1:0] this generates [0:0]
      vectors for them, which are removed automatically then. *)
    let tag_lru_width_bits = if M.o.icache_ways >= 2 then tag_lru_width else 1 in

    (* Compute the total sum of the entry elements *)
    let tagmem_width = tagmem_way_width * M.o.icache_ways + tag_lru_width in

    (* For convenience we define the position of the LRU in the tag memory entries *)
    let tag_lru_msb = tagmem_width - 1 in
    let tag_lru_lsb = tag_lru_msb - tag_lru_width + 1 in

    (************************************************)

    let state_is, sm, next = R.statemachine ~e:vdd 
      (Enum_sm.enum_from_to Bounded_sm.min_bound Bounded_sm.max_bound)
    in
    let read = state_is Read in
    let refill = state_is Refill in
    let invalidate = state_is Invalidate in

    let tag_dout = wire tagmem_width in
    let way_dout = Array.init M.o.icache_ways (fun _ -> wire M.o.operand_width) in

    let lru = wire M.o.icache_ways in
    let next_lru_history = wire tag_lru_width_bits in
    let refill_hit = wire 1 in

    let invalidate_adr = R.g_reg ~e:vdd (way_width-M.o.icache_block_width) in
    let tag_save_lru = R.g_reg ~e:vdd M.o.icache_ways in
    let tag_way_save = Array.init M.o.icache_ways (fun _ -> R.g_reg ~e:vdd tagmem_way_width) in
    let refill_valid = R.g_reg ~e:vdd (1 lsl (M.o.icache_block_width-2)) in
    let refill_valid_r = R.g_reg ~e:vdd (1 lsl (M.o.icache_block_width-2)) in
    let spr_bus_ack_o = R.g_reg ~e:vdd 1 in

    let tag_way_in = Array.init M.o.icache_ways (fun _ -> g_wire (zero tagmem_way_width)) in
    let tag_lru_in = g_wire (zero tag_lru_width_bits) in
    let tag_we = g_wire gnd in
    let way_we = g_wire (zero M.o.icache_ways) in
    let access = g_wire (zero M.o.icache_ways) in

    (************************************************)

    let tag_rindex = i.cpu_adr.[way_width-1:M.o.icache_block_width] in

    (*
    * The tag mem is written during reads to write the lru info and during
    * refill and invalidate
    *)
    let tag_windex = 
      mux2 read i.cpu_adr_match.[way_width-1:M.o.icache_block_width] @@
      mux2 invalidate invalidate_adr#q @@
      i.wradr.[way_width-1:M.o.icache_block_width] in
    let tag_tag = i.cpu_adr_match.[M.o.icache_limit_width-1:way_width] in
    let tag_wtag = i.wradr.[M.o.icache_limit_width-1:way_width] in

    let current_lru_history, tag_lru_out = 
      if M.o.icache_ways >= 2 then
        tag_dout.[tag_lru_msb:tag_lru_lsb],
        tag_dout.[tag_lru_msb:tag_lru_lsb] 
      else
        gnd, gnd
    in

    let way = Array.init M.o.icache_ways (fun idx ->
      let tag_way_out = tag_dout.[(idx+1)*tagmem_way_width-1:idx*tagmem_way_width] in
      let check_tag = tag_way_out.[tag_width-1:0] in
      let check_match = check_tag ==: tag_tag in
      let check_valid = tag_way_out.[tagmem_way_valid:tagmem_way_valid] in
      let hit = check_valid &: check_match in
      Way.{
        raddr = i.cpu_adr.[way_width-1:2];
        waddr = i.wradr.[way_width-1:2];
        din = i.wrdat;
        check_tag;
        check_match;
        check_valid;
        hit;
        tag_din = tag_way_in.(idx)#q;
        tag_way_out;
      })
    in

    let way_reduce f x = Array.map x way |> Array.to_list |> reduce f in
    let way_vector x = Array.map x way |> Array.to_list |> List.rev |> concat in

    let hit = way_reduce (|:) (fun x -> x.Way.hit) in

    let cpu_dat = 
      let c = Array.mapi (fun i x -> Way.(x.hit |: (refill_hit &: tag_save_lru#q.[i:i]),
                                          way_dout.(i))) way in
      Array.fold_left (fun x (c,d) -> mux2 c d x) (zero insn_width) c
    in

    let next_refill_adr = 
      if M.o.icache_block_width = 5 then  
        (i.wradr.[31:5] @: i.wradr.[4:0] +: consti 5 4) 
      else
        (i.wradr.[31:4] @: i.wradr.[3:0] +: consti 4 4)
    in

    let index a d = mux a (List.rev @@ bits d) in
    let refill_done = index next_refill_adr.[M.o.icache_block_width-1:2] refill_valid#q in
    let () = refill_hit <== 
      ((index i.cpu_adr_match.[M.o.icache_block_width-1:2] refill_valid_r#q) &:
      (i.cpu_adr_match.[M.o.icache_limit_width-1:M.o.icache_block_width] ==:
        i.wradr.[M.o.icache_limit_width-1:M.o.icache_block_width]) &:
      refill)
    in

    let invalidate_o = i.spr_bus_stb &: i.spr_bus_we &: (i.spr_bus_addr ==: M.Spr.icbir_addr) in

    let g_for len g = 
      g_proc @@ Array.to_list @@ Array.init len (fun i -> g i)
    in

    (* Cache FSM *)
    let () = compile [
      refill_valid_r $== refill_valid#q;
      spr_bus_ack_o $==. 0;
      sm [
        Idle, [
          g_when (i.cpu_req) [
          next Read;
          ];
        ];

        Read, [
          g_if (i.ic_access) [
            g_if (hit) [
              next Read;
            ] @@ g_elif (i.cpu_req) [
              refill_valid $==. 0;
              refill_valid_r $==. 0;

              (* Store the LRU information for correct replacement
                on refill. Always one when only one way. *)
              tag_save_lru $== (if M.o.icache_ways=1 then vdd else lru);

              g_for (Array.length way) (fun i ->
                tag_way_save.(i) $== way.(i).Way.tag_way_out);

              next Refill;
            ] [];
          ] [
            next Idle;
          ];
        ];

        Refill, [
          g_when (i.we) [
            (*refill_valid[wradr_i[M.o.icache_block_width-1:2]] <= 1;*)
          refill_valid $== (refill_valid#q |: 
                            (binary_to_onehot i.wradr.[M.o.icache_block_width-1:2]));

            g_when (refill_done) [
              next Idle;
            ];
          ];
        ];

        Invalidate, [
          g_when (~: invalidate_o) [
            next Idle;
          ];
          spr_bus_ack_o $==. 1;
        ];

      ];

      g_when (invalidate_o &: (~: refill)) [
        invalidate_adr $== i.spr_bus_dat_i.[way_width-1:M.o.icache_block_width];
        spr_bus_ack_o $==. 1;
        next Invalidate;
      ]

    ] in

    
    let () = compile [
      (* Default is to keep data, don't write and don't access *)
      tag_lru_in $== tag_lru_out;
      g_for (Array.length tag_way_in) (fun i ->
        tag_way_in.(i) $== way.(i).Way.tag_way_out);

      tag_we $==. 0;
      way_we $==. 0;
      access $==. 0;

      sm [
        Read, [
          g_when (hit) [
              (* We got a hit. The LRU module gets the access
                information. Depending on this we update the LRU
                history in the tag. *)
              access $== way_vector (fun x -> x.Way.hit);

              (* This is the updated LRU history after hit *)
              tag_lru_in $== next_lru_history;

              tag_we $==. 1;
          ]
        ];

        Refill, [
          g_when (i.we) [
            (* Write the data to the way that is replaced (which is the LRU) *)
            way_we $== tag_save_lru#q;

            (* Access pattern *)
            access $== tag_save_lru#q;

            (* Invalidate the way on the first write *)
            g_when (refill_valid#q ==:. 0) [
              g_for (Array.length tag_way_in) (fun i ->
                g_when tag_save_lru#q.[i:i] [
                  tag_way_in.(i) $== insert ~t:way.(i).Way.tag_way_out ~f:gnd tagmem_way_valid; 
                ]);
              tag_we $==. 1;
            ];

            (* After refill update the tag memory entry of the
              filled way with the LRU history, the tag and set
              valid to 1. *)
            g_when (refill_done) [
              g_for (Array.length tag_way_in) (fun i ->
                g_proc [
                  tag_way_in.(i) $== tag_way_save.(i)#q;
                  g_when tag_save_lru#q.[i:i] [
                    tag_way_in.(i) $== vdd @: tag_wtag;
                  ];
                ]);
              tag_lru_in $== next_lru_history;

              tag_we $==. 1;
            ];
          ];
        ];

        Invalidate, [
          (* Lazy invalidation, invalidate everything that matches tag address *)
          tag_lru_in $==. 0;
          g_for (Array.length tag_way_in) (fun i -> tag_way_in.(i) $==. 0);

          tag_we $==. 1;
        ];

      ];

    ] in

    (************************************************)

    let module Simple_tag = Ram.Simple_dp(struct
      let addr_width = M.o.dcache_set_width
      let data_width = tagmem_width
    end)(M) in
    let tag_din = tag_lru_in#q @: way_vector (fun x -> x.Way.tag_din) in
    let tag_ram = Simple_tag.(ram_inst ~enable_bypass:false 
      I.{
        clk = i.clk;
        raddr = tag_rindex;
        re = vdd;
        waddr = tag_windex;
        we = tag_we#q;
        din = tag_din;
      })
    in
    let () = tag_dout <== tag_ram.Simple_tag.O.dout in

    let module Simple_way = Ram.Simple_dp(struct
      let addr_width = way_width - 2
      let data_width = M.o.operand_width
    end)(M) in
    let way_data_ram = Array.mapi (fun idx x ->
      Simple_way.(ram_inst ~enable_bypass:false
        I.{
          clk = i.clk;
          raddr = x.Way.raddr.[way_width-3:0];
          re = vdd;
          waddr = x.Way.waddr.[way_width-3:0];
          we = way_we#q.[idx:idx];
          din = x.Way.din;
        })) way 
    in

    let () = 
      for i=0 to M.o.icache_ways-1 do
        way_dout.(i) <== way_data_ram.(i).Simple_way.O.dout
      done
    in

    let module Cache_lru = Cache_lru.Make(struct let numways = 2 end)(M) in
    let cache_lru = 
      if M.o.icache_ways >= 2 then
        Cache_lru.cache_lru_inst
          Cache_lru.I.{
            current = current_lru_history;
            access = access#q;
          }
      else
        Cache_lru.O.(map (fun (_,b) -> zero b) t) (* XXX not sure *)
    in
    let () = lru <== cache_lru.Cache_lru.O.lru_pre in
    let () = next_lru_history <== cache_lru.Cache_lru.O.update in

    (************************************************)

    (* Allowing (out of the cache line being refilled) accesses during refill
      exposes a bug somewhere, causing the Linux kernel to end up with a
      bus error UNHANDLED EXCEPTION.
      Until that is sorted out, disable it. *)
    let cpu_ack = (read (*|: refill & i.ic_access*)) &: hit |: refill_hit &: i.ic_access in
    let refill_req = read &: i.cpu_req &: (~: hit) |: refill in
    let spr_bus_ack = spr_bus_ack_o#q in
    let spr_bus_dat_o = zero M.o.operand_width in

    O.{
      refill;
      refill_req;
      refill_done;
      invalidate = invalidate_o;
      cpu_ack;
      cpu_dat;
      spr_bus_dat_o;
      spr_bus_ack;
    }

  module Inst = M.Inst(I)(O)
  let icache_inst = Inst.inst "icache" icache

end

