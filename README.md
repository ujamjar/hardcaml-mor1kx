# OpenRISC 1200 CPU

A port of the _mor1kx_ OpenRISC 1200 implementation to HardCaml.

*STATUS* most is code ported, but totally untested.  The plan is 
to generate submodules individually, then insert them into the mor1kx 
rtl and check all outputs against the verilog model.  Fix and repeat
until everything works.

# License

The original code is covered by the _Open-Hardware-Description-License_.
This port is also covered by that license.

# Modules

```
  mor1kx-defines.v                    -> Defines
  mor1kx_utils.vh                     -> Utils (etc)
  mor1kx-sprs.v                       -> Spr
  mor1kx_branch_prediction.v          -> Branch_prediction
  mor1kx_bus_if_avalon.v              -> Avalon
  mor1kx_bus_if_wb32.v                -> Wishbone
  mor1kx_cache_lru.v                  -> Cache_lru
  mor1kx_cfgrs.v                      -> Cfgrs
  mor1kx_cpu_cappuccino.v
  mor1kx_cpu_espresso.v
  mor1kx_cpu_prontoespresso.v
  mor1kx_cpu.v
  mor1kx_ctrl_cappuccino.v
  mor1kx_ctrl_espresso.v              -> Ctrl.Espresso, Debugunit.Espresso
  mor1kx_ctrl_prontoespresso.v
  mor1kx_dcache.v                     -> Dcache
  mor1kx_decode_execute_cappuccino.v  -> Decode_execute_cappuccino
  mor1kx_decode.v                     -> Decode
  mor1kx_dmmu.v                       -> Dmmu
  mor1kx_execute_alu.v                -> Execute_alu
  mor1kx_execute_ctrl_cappuccino.v    -> Execute_ctrl_cappuccino
  mor1kx_fetch_cappuccino.v           -> Fetch.Cappuccino
  mor1kx_fetch_espresso.v             -> Fetch.Espresso
  mor1kx_fetch_prontoespresso.v       -> Fetch.Pronto_espresso
  mor1kx_fetch_tcm_prontoespresso.v   -> Fetch.Tcm_prontoespress
  mor1kx_icache.v                     -> Icache
  mor1kx_immu.v                       -> Immu
  mor1kx_lsu_cappuccino.v             -> Lsu.Cappuccino
  mor1kx_lsu_espresso.v               -> Lsu.Espresso
  mor1kx_pic.v                        -> Pic
  mor1kx_rf_cappuccino.v              -> rf.Cappuccino
  mor1kx_rf_espresso.v                -> Rf.Espresso
  mor1kx_simple_dpram_sclk.v          -> Ram
  mor1kx_store_buffer.v               -> Store_buffer
  mor1kx_ticktimer.v                  -> Ticktimer
  mor1kx_true_dpram_sclk.v            -> Ram
  mor1kx.v
  mor1kx_wb_mux_cappuccino.v          -> Wb_mux.Cappuccino
  mor1kx_wb_mux_espresso.v            -> Wb_mux.Espresso
```

# Porting notes

## Hierarchy

The code can be generated as a single, flat module, or with hierachy using
mutiple modules.

## Operator precedence

The precedence rules for Verilog and OCaml differ.  Mainly this relates to `&` and `|`.
In Verilog `&` has higher precendence than `|` while in OCaml `&:` and `|:` have the same 
precedence.  Bracketing is not used very much in the original code.

To work round this there's a camlp4 syntax extension which provides proper precendence 
rules for the bitwise operators; `pa_vprec.cmo`.

At the moment this needs to be built and installed first before building the core.

```
$ make ext install_ext
```

## Code porting Issues

* True dual port RAM cannot be described in HardCaml.  There's a working hack, but we
  really need vendor RAM implementations to do this properly.

* There are some very minor differences related to reset behaviour that will need to be
  checked.

* (<==\) operator isnt going to work as hoped.  Used in serial multiplier.  Needs rewrite.
