(******************************************************************************
 This Source Code Form is subject to the terms of the
 Open Hardware Description License, v. 1.0. If a copy
 of the OHDL was not distributed with this file, You
 can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

 Description: Data cache implementation

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

  module I = interface
    clk[1]
    rst[1]
    dc_enable[1]
    dc_access[1]
    cpu_dat_i[M.o.operand_width]
    cpu_adr[M.o.operand_width]
    cpu_adr_match[M.o.operand_width]
    cpu_req[1]
    cpu_we[1]
    cpu_bsel[4]
    refill_allowed[1]
    wradr[M.o.operand_width]
    wrdat[M.o.operand_width]
    we[1]
    snoop_adr[32]
    snoop_valid[1]
    spr_bus_addr[16]
    spr_bus_we[1]
    spr_bus_stb[1]
    spr_bus_dat_i[M.o.operand_width]
  end

  module O = interface 
    refill[1]
    refill_req[1]
    refill_done[1]
    cpu_err[1]
    cpu_ack[1]
    cpu_dat_o[M.o.operand_width]
    snoop_hit[1]
    spr_bus_dat_o[M.o.operand_width]
    spr_bus_ack[1]
    redundant[1]
  end

  type sm = Idle | Read | Write | Refill | Invalidate
    deriving(Enum, Bounded)

  module Snoop = interface
    way_out
    check_way_tag
    check_way_match
    check_way_valid
    way_hit
  end

  module Way = interface
    raddr waddr din
    check_tag check_match check_valid
    hit
    tag_din tag_way_out
    (snoop : Snoop)
  end

  let dcache i = 
    let open I in
    let module R = Utils.Regs(struct let clk = i.clk let rst = i.rst end) in

    (* Address space in bytes for a way *)
    let way_width = M.o.dcache_block_width + M.o.dcache_set_width in
    (*
    * Tag memory layout
    *            +---------------------------------------------------------+
    * (index) -> | LRU | wayN valid | wayN tag |...| way0 valid | way0 tag |
    *            +---------------------------------------------------------+
    *)

    (* The tag is the part left of the index *)
    let tag_width = M.o.dcache_limit_width - way_width in
    (* The tag memory contains entries with OPTION_DCACHE_WAYS parts of
      each TAGMEM_WAY_WIDTH. Each of those is tag and a valid flag. *)
    let tagmem_way_width = tag_width + 1 in
    let tagmem_way_valid = tagmem_way_width - 1 in
    (* Additionally, the tag memory entry contains an LRU value. The
      width of this is 0 for OPTION_DCACHE_LIMIT_WIDTH==1 *)
    let tag_lru_width = M.o.dcache_ways * (M.o.dcache_ways-1) / 2 in
    (* We have signals for the LRU which are not used for one way
      caches. To avoid signal width [-1:0] this generates [0:0]
      vectors for them, which are removed automatically then. *)
    let tag_lru_width_bits = if M.o.dcache_ways >= 2 then tag_lru_width else 1 in
    (* Compute the total sum of the entry elements *)
    let tagmem_width = tagmem_way_width * M.o.dcache_ways + tag_lru_width in
    (* For convenience we define the position of the LRU in the tag
      memory entries *)
    let tag_lru_msb = tagmem_width - 1 in
    let tag_lru_lsb = tag_lru_msb - tag_lru_width + 1 in

  (*
    Printf.printf "
    Options:
      OPTION_OPERAND_WIDTH = %i
      OPTION_DCACHE_BLOCK_WIDTH = %i
      OPTION_DCACHE_SET_WIDTH = %i
      OPTION_DCACHE_WAYS = %i
      OPTION_DCACHE_LIMIT_WIDTH = %i
      OPTION_DCACHE_SNOOP = %b

    Derived params:
      way_width = %i
      tag_width = %i
      tagmem_way_width = %i
      tagmem_way_valid = %i
      tag_lru_width = %i
      tag_lru_width_bits = %i
      tagmem_width = %i
      tag_lru_msb = %i
      tag_lru_lsb = %i

  %!"
      M.o.M.o.operand_width 
      M.o.dcache_block_width 
      M.o.dcache_set_width 
      M.o.dcache_ways 
      M.o.dcache_limit_width 
      M.o.dcache_snoop 
      way_width
      tag_width 
      tagmem_way_width 
      tagmem_way_valid 
      tag_lru_width 
      tag_lru_width_bits 
      tagmem_width 
      tag_lru_msb 
      tag_lru_lsb;
  *)

    (************************************************)

    let snoop_dout = wire tagmem_width in
    let tag_dout = wire tagmem_width in
    let way_dout = Array.init M.o.dcache_ways (fun _ -> wire M.o.operand_width) in

    let refill_hit = wire 1 in

    let snoop_tag = R.g_reg ~e:vdd tag_width in
    let snoop_check = R.g_reg ~e:vdd 1 in
    let write_pending = R.g_reg ~e:vdd 1 in
    let tag_save_lru = R.g_reg ~e:vdd M.o.dcache_ways in
    let tag_way_save = Array.init M.o.dcache_ways (fun _ -> R.g_reg ~e:vdd tagmem_way_width) in
    let refill_valid = R.g_reg ~e:vdd (1 lsl (M.o.dcache_block_width-2)) in
    let refill_valid_r = R.g_reg ~e:vdd (1 lsl (M.o.dcache_block_width-2)) in
    let invalidate_adr = R.g_reg ~e:vdd (way_width-M.o.dcache_block_width) in
    let snoop_windex = R.g_reg ~e:vdd M.o.dcache_set_width in

    let tag_way_in = Array.init M.o.dcache_ways (fun _ -> g_wire (zero tagmem_way_width)) in
    let tag_lru_in = g_wire (zero tag_lru_width_bits) in
    let tag_we = g_wire gnd in
    let way_we = g_wire (zero M.o.dcache_ways) in
    let access = g_wire (zero M.o.dcache_ways) in
    let way_wr_dat = g_wire (zero 32) in
    let invalidate_ack = g_wire gnd in
    let tag_windex = g_wire (zero M.o.dcache_set_width) in

    let lru = wire M.o.dcache_ways in
    let next_lru_history = wire tag_lru_width_bits in

    let state_is, sm, next = R.statemachine ~e:vdd 
      (Enum_sm.enum_from_to Bounded_sm.min_bound Bounded_sm.max_bound)
    in

    let refill = state_is Refill in
    let read = state_is Read in
    let write = state_is Write in

    (************************************************)

    let snoop_index = i.snoop_adr.[way_width-1:M.o.dcache_block_width] in

    let current_lru_history, tag_lru_out = 
      if M.o.dcache_ways >= 2 then
        tag_dout.[tag_lru_msb:tag_lru_lsb],
        tag_dout.[tag_lru_msb:tag_lru_lsb] 
      else
        gnd, gnd
    in

    let tag_rindex = i.cpu_adr.[way_width-1:M.o.dcache_block_width] in
    let tag_tag = i.cpu_adr_match.[M.o.dcache_limit_width-1:way_width] in
    let tag_wtag = i.wradr.[M.o.dcache_limit_width-1:way_width] in

    let way = Array.init M.o.dcache_ways (fun idx ->
      let tag_way_out = tag_dout.[(idx+1)*tagmem_way_width-1:idx*tagmem_way_width] in
      let check_tag = tag_way_out.[tag_width-1:0] in
      let check_match = check_tag ==: tag_tag in
      let check_valid = tag_way_out.[tagmem_way_valid:tagmem_way_valid] in
      let hit = check_valid &: check_match in
      Way.{
        raddr = i.cpu_adr.[way_width-1:2];
        waddr = mux2 write i.cpu_adr_match.[way_width-1:2] i.wradr.[way_width-1:2];
        din = way_wr_dat#q;
        check_tag;
        check_match;
        check_valid;
        hit;
        tag_din = tag_way_in.(idx)#q;
        tag_way_out;
        snoop = 
          if M.o.dcache_snoop then
            let way_out = snoop_dout.[((idx+1)*tagmem_way_width)-1:idx*tagmem_way_width] in
            let check_way_tag = way_out.[tagmem_way_width-1:0] in
            let check_way_match = check_way_tag ==: snoop_tag#q in
            let check_way_valid = way_out.[tagmem_way_valid:tagmem_way_valid] in
            let way_hit = check_way_valid &: check_way_match in
            Snoop.{
              way_out;
              check_way_tag;
              check_way_match;
              check_way_valid;
              way_hit;
            }
          else
            (* Snoop.(map (fun (_,b) -> zero b) t) *) (* XXX FIXME *)
            Snoop.{
              way_out = zero tagmem_way_width;
              check_way_tag = zero tag_width;
              check_way_match = gnd;
              check_way_valid = gnd;
              way_hit = gnd;
            }
      })
    in

    let way_reduce f x = Array.map x way |> Array.to_list |> reduce f in
    let way_vector x = Array.map x way |> Array.to_list |> List.rev |> concat in
    let hit = way_reduce (|:) (fun x -> x.Way.hit) in
    let snoop_hit = 
      if M.o.dcache_snoop then 
        (way_reduce (|:) (fun x -> x.Way.snoop.Snoop.way_hit)) &: snoop_check#q
      else 
        gnd
    in

    let cpu_dat_o = 
      let c = Array.mapi (fun i x -> Way.(x.hit |: (refill_hit &: tag_save_lru#q.[i:i]),
                                          way_dout.(i))) way in
      Array.fold_left (fun x (c,d) -> mux2 c d x) (zero M.o.operand_width) c
    in

    let next_refill_adr = 
      if M.o.dcache_block_width = 5 then  
        (i.wradr.[31:5] @: i.wradr.[4:0] +: consti 5 4) 
      else
        (i.wradr.[31:4] @: i.wradr.[3:0] +: consti 4 4)
    in

    let index a d = mux a (List.rev @@ bits d) in
    let refill_done = index next_refill_adr.[M.o.dcache_block_width-1:2] refill_valid#q in
    let () = refill_hit <== 
      ((index i.cpu_adr_match.[M.o.dcache_block_width-1:2] refill_valid_r#q) &:
      (i.cpu_adr_match.[M.o.dcache_limit_width-1:M.o.dcache_block_width] ==:
        i.wradr.[M.o.dcache_limit_width-1:M.o.dcache_block_width]) &:
      refill &: (~: (write_pending#q)))
    in

    (* SPR bus interface *)

    (* The SPR interface is used to invalidate the cache blocks. When
      an invalidation is started, the respective entry in the tag
      memory is cleared. When another transfer is in progress, the
      handling is delayed until it is possible to serve it.
      
      The invalidation is acknowledged to the SPR bus, but the cycle
      is terminated by the core. We therefore need to hold the
      invalidate acknowledgement. Meanwhile we continuously write the
      tag memory which is no problem. *)

    (* An invalidate request is either a block flush or a block invalidate *)
    let invalidate = i.spr_bus_stb &: i.spr_bus_we &:
      ((i.spr_bus_addr ==: M.Spr.Dc.(const dcbfr)) |: 
       (i.spr_bus_addr ==: M.Spr.Dc.(const dcbir))) in

    (* Acknowledge to the SPR bus. *)
    let spr_bus_ack = invalidate_ack#q in

    let g_for len g = 
      g_proc @@ Array.to_list @@ Array.init len (fun i -> g i)
    in

    (*
    * Cache FSM
    * Starts in IDLE.
    * State changes between READ and WRITE happens cpu_we_i is asserted or not.
    * cpu_we_i is in sync with cpu_adr_i, so that means that it's the
    * *upcoming* write that it is indicating. It only toggles for one cycle,
    * so if we are busy doing something else when this signal comes
    * (i.e. refilling) we assert the write_pending signal.
    * cpu_req_i is in sync with cpu_adr_match_i, so it can be used to
    * determined if a cache hit should cause a refill or if a write should
    * really be executed.
    *)
    
    let () = compile [

      g_if (i.cpu_we) [
        write_pending $==. 1;
      ] @@ g_elif (~: (i.cpu_req)) [
        write_pending $==. 0;
      ] [];
      
      refill_valid_r $== refill_valid#q;

      g_if (i.snoop_valid) [
        (* If there is a snoop event, we need to store this
          information. This happens independent of whether we
          have a snoop tag memory or not.  *)
        snoop_check $==. 1;
        snoop_windex $== snoop_index;
        snoop_tag $== i.snoop_adr.[M.o.dcache_limit_width-1:way_width];
      ] [
        snoop_check $==. 0;
      ];

      sm [
        Idle, [
          g_if (invalidate) [
            (* If there is an invalidation request
              Store address in invalidate_adr that is muxed to the tag memory write address *)
            invalidate_adr $== i.spr_bus_dat_i.[way_width-1:M.o.dcache_block_width];

            (* Change to invalidate state that actually accesses the tag memory *)
            next Invalidate;
          ] @@ g_elif (i.cpu_we |: write_pending#q) [
            next Write;
          ] @@ g_elif (i.cpu_req) [
            next Read;
          ] [];
        ];

        Read, [
          g_if (i.dc_access |: i.cpu_we &: i.dc_enable) [
            g_if ((~: hit) &: i.cpu_req &: (~: (write_pending#q)) &: i.refill_allowed) [
              refill_valid $==. 0;
              refill_valid_r $==. 0;

              (* Store the LRU information for correct replacement
                on refill. Always one when only one way. *)
              tag_save_lru $== (if M.o.dcache_ways=1 then vdd else lru);

              g_for (Array.length way) (fun i ->
                tag_way_save.(i) $== way.(i).Way.tag_way_out);

              next Refill;
            ] @@ g_elif (i.cpu_we |: write_pending#q) [
              next Write;
            ] @@ g_elif (invalidate) [
              next Idle;
            ] [];
          ] @@ g_elif ((~: (i.dc_enable)) |: invalidate) [
            next Idle;
          ] [];
        ];

        Refill, [
          g_when (i.we) [
            (*refill_valid[wradr_i[OPTION_DCACHE_BLOCK_WIDTH-1:2]] = 1; *)
            refill_valid $== (refill_valid#q |: 
                            (binary_to_onehot i.wradr.[M.o.dcache_block_width-1:2]));
            g_when (refill_done) [
              next Idle;
            ];
          ];
          (* Abort refill on snoop-hit
            TODO: only abort on snoop-hits to refill address *)
          g_when (snoop_hit) [
            refill_valid $==. 0;
            refill_valid_r $==. 0;
            next Idle;
          ]
        ]; 

        Write, [
          g_when (((~: (i.dc_access)) |: (~: (i.cpu_req)) |: (~: (i.cpu_we))) &: (~: snoop_hit)) [
            write_pending $==. 0;
            next Read;
          ]
        ];

        Invalidate, [
          g_if (invalidate) [
            (* Store address in invalidate_adr that is muxed to the tag memory write address *)
            invalidate_adr $== i.spr_bus_dat_i.[way_width-1:M.o.dcache_block_width];
            next Invalidate;
          ] [
            next Idle;
          ]
        ];
      ];

    ] in

    (*
      This is the combinational part of the state machine that
      interfaces the tag and way memories.
    *)
    let () = compile [
      (* Default is to keep data, don't write and don't access *)
      tag_lru_in $== tag_lru_out;
      g_for (Array.length way) (fun i -> tag_way_in.(i) $== way.(i).Way.tag_way_out);

      tag_we $==. 0;
      way_we $==. 0;

      access $==. 0;

      way_wr_dat $== i.wrdat;

      (* The default is (of course) not to acknowledge the invalidate *)
      invalidate_ack $==. 0;

      g_if (snoop_hit) [
        (* This is the write access *)
        tag_we $==. 1;
        tag_windex $== snoop_windex#q;
        g_for (Array.length way) (fun i ->
          g_if way.(i).Way.snoop.Snoop.way_hit [
            tag_way_in.(i) $==. 0;
          ] [
            tag_way_in.(i) $== way.(i).Way.snoop.Snoop.way_out;
          ]);
      ] [
        (* 
          The tag mem is written during reads and writes to write
          the lru info and  during refill and invalidate.
        *)
        tag_windex $== 
          mux2 (read |: write) i.cpu_adr_match.[way_width-1:M.o.dcache_block_width] @@
          mux2 (state_is Invalidate) invalidate_adr#q @@
          i.wradr.[way_width-1:M.o.dcache_block_width];

        sm [
          Idle, [
            (* 
            When idle we can always acknowledge the invalidate as it
            has the highest priority in handling. When something is
            changed on the state machine handling above this needs
            to be changed.
            *)
            invalidate_ack $==. 1;
          ];

          Read, [
            g_when (hit) [
              (*
                We got a hit. The LRU module gets the access
                information. Dep]ing on this we update the LRU
                history in the tag.
              *)
              access $== way_vector (fun x -> x.Way.hit);

                (* This is the updated LRU history after hit *)
              tag_lru_in $== next_lru_history;

              tag_we $==. 1;
            ];
          ];

          Write, [
            way_wr_dat $== i.cpu_dat_i;
            g_when (hit &: i.cpu_req) [
              (* Mux cache output with write data *)
              way_wr_dat $==
                mux2 i.cpu_bsel.[3:3] i.cpu_dat_i.[31:24] cpu_dat_o.[31:24] @:
                mux2 i.cpu_bsel.[2:2] i.cpu_dat_i.[23:16] cpu_dat_o.[23:16] @:
                mux2 i.cpu_bsel.[1:1] i.cpu_dat_i.[15:8 ] cpu_dat_o.[15:8 ] @:
                mux2 i.cpu_bsel.[0:0] i.cpu_dat_i.[ 7:0 ] cpu_dat_o.[ 7:0 ];

              way_we $== way_vector (fun x -> x.Way.hit);

              tag_lru_in $== next_lru_history;

              tag_we $==. 1;
            ];
          ];

          Refill, [
            g_when (i.we) [
              (*
              Write the data to the way that is replaced (which is
              the LRU)
              *)
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

              (*
              After refill update the tag memory entry of the
              filled way with the LRU history, the tag and set
              valid to 1.
              *)
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
            ]
          ];

          Invalidate, [
            invalidate_ack $==. 1;

            (* Lazy invalidation, invalidate everything that matches tag address *)
            tag_lru_in $==. 0;
            g_for (Array.length tag_way_in) (fun i -> tag_way_in.(i) $==. 0);

            tag_we $==. 1;
          ];
        ]
      ]
    ] in

    (************************************************)

    (* submodules *)
    let module Simple_tag = Ram.Simple_dp(struct
      let addr_width = M.o.dcache_set_width
      let data_width = tagmem_width
    end)(M) in
    let tag_din = tag_lru_in#q @: way_vector (fun x -> x.Way.tag_din) in
    let tag_i raddr = 
      Simple_tag.I.{
        clk = i.clk;
        raddr;
        re = vdd;
        waddr = tag_windex#q;
        we = tag_we#q;
        din = tag_din;
      }
    in
    let tag_ram = Simple_tag.ram_inst ~enable_bypass:M.o.dcache_snoop (tag_i tag_rindex) in
    let snoop_tag_ram = Simple_tag.ram_inst ~enable_bypass:true (tag_i snoop_index) in
    let () = tag_dout <== tag_ram.Simple_tag.O.dout in
    let () = snoop_dout <== snoop_tag_ram.Simple_tag.O.dout in

    let module Cache_lru = Cache_lru.Make(struct let numways = 2 end)(M) in
    let cache_lru = 
      if M.o.dcache_ways >= 2 then
        Cache_lru.(cache_lru_inst
          I.{
            current = current_lru_history;
            access = access#q;
          })
      else
        Cache_lru.O.(map (fun (_,b) -> zero b) t) (* XXX not sure *)
    in
    let () = lru <== cache_lru.Cache_lru.O.lru_pre in
    let () = next_lru_history <== cache_lru.Cache_lru.O.update in

    let module Simple_way = Ram.Simple_dp(struct
      let addr_width = way_width - 2
      let data_width = M.o.operand_width
    end)(M) in
    let way_data_ram = Array.mapi (fun idx x ->
      Simple_way.(ram_inst ~enable_bypass:true
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
      for i=0 to M.o.dcache_ways-1 do
        way_dout.(i) <== way_data_ram.(i).Simple_way.O.dout
      done
    in

    (************************************************)

    let snoop_hit = if M.o.dcache_snoop then snoop_hit else gnd in
    let cpu_ack = ((read |: refill) &: hit &: (~: (write_pending#q)) |: refill_hit) &: 
      i.cpu_req &: (~: snoop_hit) in
    let refill_req = read &: i.cpu_req &: (~: hit) &: 
      (~: (write_pending#q)) &: i.refill_allowed |: refill in

    let cpu_err = gnd in
    let spr_bus_dat_o = zero M.o.operand_width in

    O.{
      refill;
      refill_req;
      refill_done;
      cpu_err;
      cpu_ack;
      cpu_dat_o;
      snoop_hit;
      spr_bus_dat_o;
      spr_bus_ack;
      redundant = List.fold_left (&:) gnd @@ List.map lsb @@ I.to_list i;
    }

  module Inst = M.Inst(I)(O)
  let dcache_inst = Inst.inst "dcache" dcache

end

