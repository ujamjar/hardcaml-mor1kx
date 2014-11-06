open HardCaml.Signal.Comb

type spr_part = 
  | VI of (int * int) * int
  | BI of int * int
  | BB of int * bool

let mk_spr n l = 
  let low = function
    | VI((_,l),_) -> l
    | BI(l,_) 
    | BB(l,_) -> l
  in
  let l = List.sort (fun a b -> compare (low a) (low b)) l in
  let (@:) a b = 
    if a = HardCaml.Signal.Types.Signal_empty then b
    else if b = HardCaml.Signal.Types.Signal_empty then a
    else a @: b
  in
  let check t l p = if l <> p then 
    raise (Failure (Printf.sprintf "name=%s[%s] l=%i pos=%i\n" n t l p)) in
  let rec f pos = function
    | [] -> empty
    | VI((h,l),v)::t ->  
      let w = h-l+1 in 
      let _ = check "VI" l pos in
      (consti w v) @: f (pos+w) t
    | BI(l,v)::t ->
      let _ = check "BI" l pos in
      (consti 1 v) @: f (pos+1) t
    | BB(l,v)::t ->
      let _ = check "BB" l pos in
      (if v then vdd else gnd) @: f (pos+1) t
  in
  uresize (f 0 l) 32

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

module Make_seq(S : Spec) = struct

    open HardCaml

    let make_spec 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge spec =
        let sel a b = 
            match a with
            | None -> b
            | Some(x) -> x
        in
        Signal.Types.({
            reg_clock       = sel clk  spec.reg_clock;
            reg_clock_level = sel clkl spec.reg_clock_level;
            reg_reset       = sel r    spec.reg_reset;
            reg_reset_level = sel rl   spec.reg_reset_level;
            reg_reset_value = sel rv   spec.reg_reset_value;
            reg_clear       = sel c    spec.reg_clear;
            reg_clear_level = sel cl   spec.reg_clear_level;
            reg_clear_value = sel cv   spec.reg_clear_value;
            reg_enable      = sel ge   spec.reg_enable;
        })

    let reg 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge ~e d =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.reg_spec in
        Signal.Seq.reg spec e d

    let reg_fb 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge ~e ~w f =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.reg_spec in
        Signal.Seq.reg_fb spec e w f

    let pipeline 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge ~n ~e d =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.reg_spec in
        Signal.Seq.pipeline n spec e d

    let g_reg
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge ~e w =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.reg_spec in
        Signal.Guarded.g_reg spec e w

    let g_pipeline 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge ~n ~e w =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.reg_spec in
        Signal.Guarded.g_pipeline n spec e w
 
    let statemachine
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge ~e states =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.reg_spec in
        Signal.Guarded.statemachine spec e states
 
    let memory
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge  
        size ~we ~wa ~d ~ra =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.ram_spec in
        Signal.Seq.memory ~size ~spec ~we ~w:wa ~d ~r:ra

    let ram_wbr 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge  
        size ~we ~wa ~d ~re ~ra =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.ram_spec in
        Signal.Seq.ram_wbr ~size ~spec ~we ~wa ~d ~re ~ra

    let ram_rbw 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge  
        size ~we ~wa ~d ~re ~ra =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.ram_spec in
        Signal.Seq.ram_rbw ~size ~spec ~we ~wa ~d ~re ~ra

end

module Regs(S : sig
  val clk : HardCaml.Signal.Comb.t
  val rst : HardCaml.Signal.Comb.t
end) = Make_seq(struct
  open HardCaml.Signal.Types
  open HardCaml.Signal.Seq
  let reg_spec = { r_sync with reg_clock = S.clk; reg_clear = S.rst }
  let ram_spec = { r_none with reg_clock = S.clk }
end)

let drop_bottom x n = select x (width x - 1) n
let drop_top x n = select x (width x - 1 - n) 0
let sel_bottom x n = select x (n-1) 0
let sel_top x n = select x (width x - 1) (width x - n)
let insert ~t ~f n = 
  let wt, wf = width t, width f in
  if n < 0 then failwith "insert <0"
  else if wt < (wf + n) then failwith "insert overflow"
  else if wt = wf && n = 0 then f
  else if n=0 then select t (wt - 1) wf @: f
  else if wt = (wf + n) then f @: select t (wt - wf - 1) 0
  else select t (wt - 1) (wf + n) @: f @: select t (n-1) 0

let sel x (h,l) = select x h l

let cases sel default l = 
  let max = 1 + List.fold_left (fun acc (i,_) -> max i acc) 0 l in
  let a = Array.make max default in
  let () = List.iter (fun (i,x) -> a.(i) <- x) l in
  if 1 lsl (width sel) = max then
    mux sel (Array.to_list a)
  else
    mux sel (Array.to_list a @ [default])

(* XXX move to hardcaml *)
let g_elif c t f = HardCaml.Signal.Guarded.([ g_if c t f ])

let ($==\) (x, l) y = 
  HardCaml.Signal.Guarded.( x $== insert x#q y l )


