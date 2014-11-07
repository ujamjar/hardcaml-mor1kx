(* check the new statemachine implementation *)

open HardCaml.Signal.Types
open HardCaml.Signal.Comb
open HardCaml.Signal.Seq
open HardCaml.Signal.Guarded

type t = A | B | C | D
  deriving(Enum, Bounded)

module I = interface
  clk[1]
  clr[1]
  start[1]
end

module O = interface
  a[1] b[1] c[1] d[1]
end

let test_sm i =
  let open I in
  let state_is, statemachine, next = Utils.statemachine
    ~encoding:`onehot
    { r_sync with 
      reg_clock=i.clk;
      reg_clear=i.clr } vdd
    [ A; B; C; D ]
  in
  let () = compile [
    statemachine [
      A, [
        g_when i.start [
          next B;
        ];
      ];
      B, [
        next C;
      ];
      C, [
        next D;
      ];
      D, [
        next A;
      ];
    ]
  ] in
  let a = state_is A in
  let b = state_is B in
  let c = state_is C in
  let d = state_is D in
  O.({ a; b; c; d })

module B = HardCaml.Bits.Comb.IntbitsList
module Wave = HardCaml.Vcd_ext.Make(B)
module G = HardCaml.Interface.Gen(B)(I)(O)

let circ, sim, i, o = G.make "statemachine" test_sm
let sim = Wave.gtkwave ~args:"-S test/gwShowall.tcl" sim
let () = HardCaml.Rtl.Verilog.write print_string circ
let test =
  let open I in
  let module S = HardCaml.Cyclesim.Api in
  let open B in
  S.reset sim;
  i.clr := vdd;
  S.cycle sim;
  i.clr := gnd;
  i.start := vdd;
  S.cycle sim;
  i.start := gnd;
  S.cycle sim;
  S.cycle sim;
  S.cycle sim;
  S.cycle sim;
  S.cycle sim;

  ignore @@ input_line stdin


