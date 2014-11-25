(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Description: utilities

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

open HardCaml.Signal.Comb

module Regs(S : sig
  val clk : HardCaml.Signal.Comb.t
  val rst : HardCaml.Signal.Comb.t
end) = HardCaml.Signal.Make_seq(struct
  open HardCaml.Signal.Types
  open HardCaml.Signal.Seq
  let reg_spec = { r_sync with reg_clock = S.clk; reg_clear = S.rst }
  let ram_spec = { r_none with reg_clock = S.clk }
end)

(***********************************************************************)

let ($==\) (x, l) y = 
  HardCaml.Signal.Guarded.( x $== insert x#q y l )

(***********************************************************************)

module type Module_cfg = sig
  val o : Option.options
  val f : Option.features
  module Bits : HardCaml.Comb.S
  module Inst(I : HardCaml.Interface.S)(O : HardCaml.Interface.S) : sig
    val inst : string -> (Bits.t I.t -> Bits.t O.t) -> Bits.t I.t -> Bits.t O.t
  end
  module Spr : module type of Spr.Make(Bits)
end

module type Module_cfg_signal = Module_cfg with type Bits.t = HardCaml.Signal.Comb.t

module type Inst_db = sig 
  val db : HardCaml.Circuit.Hierarchy.database 
end

module Inst_flat(I : HardCaml.Interface.S)(O : HardCaml.Interface.S) = struct
  let inst _ f i = f i
end

module Inst_hier(Db : Inst_db)(I : HardCaml.Interface.S)(O : HardCaml.Interface.S) = struct
  module Inst = HardCaml.Interface.Hier(I)(O)
  let inst name f i = 
    let _ = I.(map2 (fun (n,b) x -> 
      if width x <> b then failwith (name ^ ": input " ^ n ^ " expected " ^ 
        string_of_int b ^ " bits, but got " ^ string_of_int (width x) ^ " bits")) t i);
    in
    let o = Inst.make Db.db name f i in
    let _ = O.(map2 (fun (n,b) x -> 
      if width x <> b then failwith (name ^ ": output " ^ n ^ " expected " ^ 
        string_of_int b ^ " bits, but got " ^ string_of_int (width x) ^ " bits")) t o);
    in
    o

end

module Module_hier(Db : Inst_db) = struct
  let o = Option.default_options
  let f = Option.default_features
  module Bits = HardCaml.Signal.Comb
  module Inst = Inst_hier(Db)
  module Spr = Spr.Make(Bits)
end

module Module_flat = struct
  let o = Option.default_options
  let f = Option.default_features
  module Bits = HardCaml.Signal.Comb
  module Inst = Inst_flat
  module Spr = Spr.Make(Bits)
end

module Module_comb = struct
  let o = Option.default_options
  let f = Option.default_features
  module Bits = HardCaml.Bits.Comb.IntbitsList
  module Inst = Inst_flat
  module Spr = Spr.Make(Bits)
end

