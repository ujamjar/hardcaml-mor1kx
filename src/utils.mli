
type t = 
  | VI of (int * int) * int
  | BI of int * int
  | BB of int * bool

val mk_spr : string -> t list -> HardCaml.Signal.Comb.t

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
        (variable * ('a cases -> statement) * ('a -> statement))

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
val insert : HardCaml.Signal.Comb.t -> HardCaml.Signal.Comb.t -> int -> HardCaml.Signal.Comb.t
val sel : HardCaml.Signal.Comb.t -> (int * int) -> HardCaml.Signal.Comb.t

val cases : HardCaml.Signal.Comb.t -> HardCaml.Signal.Comb.t ->
  (int * HardCaml.Signal.Comb.t) list -> HardCaml.Signal.Comb.t

