open HardCaml.Signal.Comb
open Defines
open Option
open Utils

module I = interface
  clk[1]
  rst[1]
  irq[32]
  spr_access[1]
  spr_we[1]
  spr_addr[16]
  spr_dat_i[32]
end

module O = interface
  spr_picmr[32]
  spr_picsr[32]
  spr_bus_ack[1]
  spr_dat_o[32]
end

let irqmap f irq_unmasked spr_dat_i = 
  let a = Array.init 32 (fun i -> f (bit irq_unmasked i) (bit spr_dat_i i)) in
  concat @@ Array.to_list a

let edge irq_unmasked spr_picsr_access i = 
  let open I in
  let open HardCaml.Signal.Guarded in
  let module R = Regs(struct let clk = i.clk let rst = i.rst end) in
  let irqline irq_unmasked spr_dat_i = 
    let picsr = R.g_reg ~e:vdd 1 in
    let () = compile [
      g_if (i.spr_we &: spr_picsr_access) [
        picsr $== mux2 spr_dat_i gnd picsr#q;
      ] @@ g_elif ((~: (picsr#q)) &: irq_unmasked) [
        picsr $==. 1;
      ] []
    ] in
    picsr#q
  in
  irqmap irqline irq_unmasked i.spr_dat_i

let level irq_unmasked = irq_unmasked

let latched_level irq_unmasked spr_picsr_access i = 
  let open I in
  let open HardCaml.Signal.Guarded in
  let module R = Regs(struct let clk = i.clk let rst = i.rst end) in
  let irqline irq_unmasked spr_dat_i = 
    let picsr = R.g_reg ~e:vdd 1 in
    let () = compile [
      g_if (i.spr_we &: spr_picsr_access) [
        picsr $== (irq_unmasked |: spr_dat_i);
      ] @@ g_elif ((~: (picsr#q)) &: irq_unmasked) [
        picsr $== (picsr#q |: irq_unmasked);
      ] []
    ] in
    picsr#q
  in
  irqmap irqline irq_unmasked i.spr_dat_i

let pic o i = 
  let open I in
  let open HardCaml.Signal.Guarded in
  let module R = Regs(struct let clk = i.clk let rst = i.rst end) in

  let spr_picmr_access = i.spr_access &: Spr.(offset i.spr_addr ==: offset picmr_addr) in
  let spr_picsr_access = i.spr_access &: Spr.(offset i.spr_addr ==: offset picsr_addr) in
  
  let spr_picmr =
    let nmi_mask x = 
      if o.pic_nmi_width = 0 then i.spr_dat_i
      else select x 31 o.pic_nmi_width @: ones o.pic_nmi_width
    in
    R.reg 
      ~cv:(nmi_mask (zero 32))
      ~rv:(nmi_mask (zero 32))
      ~e:(i.spr_we ^: spr_picmr_access)
      (nmi_mask i.spr_dat_i)
  in

  let irq_unmasked = spr_picmr &: i.irq in

  let spr_picsr = 
    match o.pic_trigger with
    | Edge -> edge irq_unmasked spr_picsr_access i  
    | Level -> level irq_unmasked 
    | Latched_level -> latched_level irq_unmasked spr_picsr_access i
  in

  let spr_bus_ack = i.spr_access in
  let spr_dat_o = 
    mux2 (i.spr_access &: spr_picsr_access) spr_picsr @@
    mux2 (i.spr_access &: spr_picmr_access) spr_picmr @@
    zero 32
  in

  O.({
    spr_picmr;
    spr_picsr;
    spr_bus_ack;
    spr_dat_o;
  })
