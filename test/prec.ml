(* check precedence *)

open HardCaml.Bits.Comb.IntbitsList

(* without pa_vprec we evaluate: ((vdd |: gnd) &: gnd) => gnd
 * and with pa_vprec we get:     (vdd |: (gnd &: gnd)) => vdd *)

let x = vdd |: gnd &: gnd
