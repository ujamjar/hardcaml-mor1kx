type cpu = 
  | Cappuccino
  | Espresso
  | Pronto_espresso

type trigger =
  | Edge
  | Level
  | Latched_level

type shifter = 
  | Shifter_barrel
  | Shifter_serial

type multiplier = 
  | Multiplier_none
  | Multiplier_threestage
  | Multiplier_pipelined
  | Multiplier_simulation

type divider = 
  | Divider_none
  | Divider_serial
  | Divider_simulation

type options = 
  {
    operand_width            : int;
    cpu                      : cpu;
    dcache_block_width       : int;
    dcache_set_width         : int;
    dcache_ways              : int;
    dcache_limit_width       : int;
    dcache_snoop             : bool;
    dmmu_set_width           : int;
    dmmu_ways                : int;
    icache_block_width       : int;
    icache_set_width         : int;
    icache_ways              : int;
    icache_limit_width       : int;
    immu_set_width           : int;
    immu_ways                : int;
    pic_trigger              : trigger;
    pic_nmi_width            : int;
    rf_num_shadow_gpr        : int;
    rf_addr_width            : int;
    rf_words                 : int;
    reset_pc                 : HardCaml.Signal.Comb.t;
    tcm_fetcher              : bool;
    shifter                  : shifter;
    store_buffer_depth_width : int;
  }

type features = 
  { (* note; not properly checked every option is really a bool *)
    datacache          : bool;
    dmmu               : bool;
    dmmu_hw_tlb_reload : bool;
    instructioncache   : bool;
    immu               : bool;
    immu_hw_tlb_reload : bool;
    timer              : bool;
    debugunit          : bool;
    perfcounters       : bool;
    mac                : bool;
    syscall            : bool;
    trap               : bool;
    range              : bool;
    pic                : bool;
    dsx                : bool;
    overflow           : bool;
    carry_flag         : bool;
    fastcontexts       : bool;
    multiplier         : multiplier;
    divider            : divider;
    addc               : bool;
    sra                : bool;
    ror                : bool;
    ext                : bool;
    cmov               : bool;
    ffl1               : bool;
    msync              : bool;
    psync              : bool;
    csync              : bool;
    atomic             : bool;
    cust1              : bool;
    cust2              : bool;
    cust3              : bool;
    cust4              : bool;
    cust5              : bool;
    cust6              : bool;
    cust7              : bool;
    cust8              : bool;
    store_buffer       : bool;
    multicore          : bool;
    traceport_exec     : bool;
    pmu                : bool;
    delayslot          : bool;
    evbar              : bool;
    aecsr              : bool;
  }

val default_options : options
val default_features : features

