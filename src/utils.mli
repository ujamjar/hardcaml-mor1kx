(* ****************************************************************************
  This Source Code Form is subject to the terms of the
  Open Hardware Description License, v. 1.0. If a copy
  of the OHDL was not distributed with this file, You
  can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

  Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

***************************************************************************** *)

type spr_part = 
  | VI of (int * int) * int
  | BI of int * int
  | BB of int * bool

val mk_spr : string -> spr_part list -> HardCaml.Signal.Comb.t

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

module type S = sig

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

module Make_seq(S : Spec) : S

module Regs(S : sig 
  val clk : HardCaml.Signal.Comb.t
  val rst : HardCaml.Signal.Comb.t
end) : S

val drop_bottom : HardCaml.Signal.Comb.t -> int -> HardCaml.Signal.Comb.t
val drop_top : HardCaml.Signal.Comb.t -> int -> HardCaml.Signal.Comb.t
val sel_bottom : HardCaml.Signal.Comb.t -> int -> HardCaml.Signal.Comb.t
val sel_top : HardCaml.Signal.Comb.t -> int -> HardCaml.Signal.Comb.t
val insert : t:HardCaml.Signal.Comb.t -> f:HardCaml.Signal.Comb.t -> int -> HardCaml.Signal.Comb.t
val sel : HardCaml.Signal.Comb.t -> (int * int) -> HardCaml.Signal.Comb.t

val cases : HardCaml.Signal.Comb.t -> HardCaml.Signal.Comb.t ->
  (int * HardCaml.Signal.Comb.t) list -> HardCaml.Signal.Comb.t

val g_elif : HardCaml.Signal.Comb.t ->
  HardCaml.Signal.Guarded.statement list ->
  HardCaml.Signal.Guarded.statement list ->
  HardCaml.Signal.Guarded.statement list

val ($==\) :
  HardCaml.Signal.Guarded.variable * int -> 
  HardCaml.Signal.Types.signal -> HardCaml.Signal.Guarded.statement

