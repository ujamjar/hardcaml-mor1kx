(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

module Multiram : sig
  open HardCaml.Signal.Types
  open HardCaml.Signal.Comb

  type 'a write = 
      {
          we : 'a;
          wd : 'a;
          wa : 'a;
      }
  type 'a read = 
      {
          re : 'a;
          ra : 'a;
      }

  type ram = size:int -> we:t -> wa:t -> d:t -> re:t -> ra:t -> t

  val ram : ?priority_write:bool -> ram:ram -> size:int -> spec:register -> 
      wr:t write array -> rd:t read array -> t array

end

module Gray(B : HardCaml.Comb.S) : sig

    val to_list : int -> B.t list

    val binary_to_gray : B.t -> B.t

    val gray_to_binary : B.t -> B.t

    val test : int -> bool * bool

end

val statemachine : ?encoding:[ `binary | `onehot | `gray ] ->
  HardCaml.Signal.Types.register ->
  HardCaml.Signal.Comb.t ->
  'a list ->
  ('a -> HardCaml.Signal.Comb.t) *                                            (* is_state *)
  ('a HardCaml.Signal.Guarded.cases -> HardCaml.Signal.Guarded.statement) *   (* switch *)
  ('a -> HardCaml.Signal.Guarded.statement)                                   (* next *)

module type Spec = sig
    val reg_spec : HardCaml.Signal.Types.register
    val ram_spec : HardCaml.Signal.Types.register
end

module type Seq = sig

    open HardCaml.Signal.Types

    val reg : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        e:signal -> signal -> signal

    val reg_fb : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        e:signal -> w:int -> (signal -> signal) -> signal

    val pipeline : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        n:int -> e:signal -> signal -> signal

    open HardCaml.Signal.Guarded

    val g_reg : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        e:signal -> int -> variable

    val g_pipeline : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        n:int -> e:signal -> int -> variable

    val statemachine : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        e:signal -> 'a list -> 
        (('a -> signal) * ('a cases -> statement) * ('a -> statement))

    val memory : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        int -> we:signal -> wa:signal -> d:signal -> ra:signal -> signal

    val ram_wbr : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        int -> we:signal -> wa:signal -> d:signal -> re:signal -> ra:signal -> signal

    val ram_rbw : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        int -> we:signal -> wa:signal -> d:signal -> re:signal -> ra:signal -> signal

    val multi_ram_wbr : ?priority_write:bool -> 
      rd:HardCaml.Signal.Comb.t Multiram.read array ->
      wr:HardCaml.Signal.Comb.t Multiram.write array ->
      int -> HardCaml.Signal.Comb.t array

    val multi_ram_rbw : ?priority_write:bool -> 
      rd:HardCaml.Signal.Comb.t Multiram.read array ->
      wr:HardCaml.Signal.Comb.t Multiram.write array ->
      int -> HardCaml.Signal.Comb.t array

end

module Make_seq(S : Spec) : Seq

module Regs(S : sig 
  val clk : HardCaml.Signal.Comb.t
  val rst : HardCaml.Signal.Comb.t
end) : Seq

module Logic(B : HardCaml.Comb.S) : sig
  val drop_bottom : B.t -> int -> B.t
  val drop_top : B.t -> int -> B.t
  val sel_bottom : B.t -> int -> B.t
  val sel_top : B.t -> int -> B.t
  val insert : t:B.t -> f:B.t -> int -> B.t
  val sel : B.t -> (int * int) -> B.t
  val cases : B.t -> B.t -> (int * B.t) list -> B.t
  val pmux : (B.t * B.t) list -> B.t -> B.t
  val to_array : B.t -> B.t array
  val of_array : B.t array -> B.t
  val (||:) : B.t -> B.t -> B.t
  val (&&:) : B.t -> B.t -> B.t
end


val g_elif : HardCaml.Signal.Comb.t ->
  HardCaml.Signal.Guarded.statement list ->
  HardCaml.Signal.Guarded.statement list ->
  HardCaml.Signal.Guarded.statement list

val ($==\) :
  HardCaml.Signal.Guarded.variable * int -> 
  HardCaml.Signal.Types.signal -> HardCaml.Signal.Guarded.statement

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

module Module_hier(Db : Inst_db) : Module_cfg_signal
module Module_flat : Module_cfg with type Bits.t = HardCaml.Signal.Comb.t
module Module_comb : Module_cfg with type Bits.t = HardCaml.Bits.Comb.IntbitsList.t

