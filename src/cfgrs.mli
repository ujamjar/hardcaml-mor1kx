module O : interface
    vr vr2 upr cpucfgr dmmucfgr immucfgr
    dccfgr iccfgr dcfgr pccfgr fpcsr avr
end

val cfgrs : Option.options -> Option.features -> HardCaml.Signal.Comb.t O.t


