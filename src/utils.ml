open HardCaml.Signal.Comb

(***********************************************************************)

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

(***********************************************************************)

module Multiram = struct
  (* XXX can't remember if I tested this properly... *)

  let rec bin2oh s = 
    let (&::) a b = repeat a (width b) &: b in
    if width s = 1 then s @: ~: s 
    else 
        ((((msb s)) &:: bin2oh (lsbs s)) @: 
      ((~: (msb s)) &:: bin2oh (lsbs s)))


  let rec oh2bin s = 
    let pairs s = 
      let s = if width s mod 2 = 0 then s else gnd @: s in
      let b = List.rev (bits s) in
      HardCaml.Utils.zip (HardCaml.Utils.leven b) (HardCaml.Utils.lodd b)
    in
    let enc2_1 (a, b) = (b, a |: b) in
    if width s = 1 then gnd
    else if width s = 2 then bit s 1
    else
      let s, p = HardCaml.Utils.unzip (List.map enc2_1 (pairs s)) in
      oh2bin (concat (List.rev p)) @: reduce (|:) s

  let rec oh2bin_p s = 
    let w = width s in
    let l = HardCaml.Utils.nbits (w-1) in
    let rec f b i = 
      match b with
      | [] -> empty (* shouldnt get here *)
      | h::[] -> consti l i
      | h::t -> mux2 h (consti l i) (f t (i+1))
    in
    f (List.rev (bits s)) 0

  (* lvt multiport ram *)

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

  let ram_1wr ~ram ~size ~wr ~rd = 
    (* 1 write, n read ports *)
    Array.map 
      (fun rd ->
        ram ~size 
          ~we:wr.we ~wa:wr.wa ~d:wr.wd 
          ~re:rd.re ~ra:rd.ra) rd

  let lvt ~priority_write ~size ~spec ~wr ~rd = 
    let n_wr, n_rd = Array.length wr, Array.length rd in
    let bin2oh we s = Array.map ((&:) we) (Array.of_list (List.rev (bits (bin2oh s)))) in
    let we1h = Array.map (fun wr -> bin2oh wr.we wr.wa) wr in
    let regs = Array.init size (fun i -> 
      let wes = Array.init n_wr (fun j -> we1h.(j).(i)) in
      let we = reduce (|:) (Array.to_list wes) in
      let oh2bin = if priority_write then oh2bin_p else oh2bin in
      let d = oh2bin (concat (List.rev (Array.to_list wes))) in
      HardCaml.Signal.Seq.reg spec we d)
    in
    let regs = Array.to_list regs in
    Array.map (fun rd -> mux rd.ra regs) rd

  let ram ?(priority_write=false) ~ram ~size ~spec ~wr ~rd = 
    let n_wr, n_rd = Array.length wr, Array.length rd in
    let banks = Array.map (fun wr -> ram_1wr ~ram ~size ~wr ~rd) wr in
    let lvt = lvt ~priority_write ~size ~spec ~wr ~rd in
    let lvt = Array.init n_rd (fun i -> HardCaml.Signal.Seq.reg spec rd.(i).re lvt.(i)) in

    Array.init n_rd (fun i -> mux lvt.(i) 
      (Array.to_list (Array.init n_wr (fun j -> banks.(j).(i))))) 

end

(***********************************************************************)

module Gray(B : HardCaml.Comb.S) = struct

  open B

  let rec to_list bits = 
    if bits=1 then [ gnd; vdd ]
    else
      let c = to_list (bits-1) in
      (List.map ((@:) gnd) c) @ (List.map ((@:) vdd) (List.rev c))

  let binary_to_gray b = b ^: (srl b 1)
  
  let gray_to_binary b = 
    let ue x = uresize x (width b) in
    let rec f b mask = 
      let b = b ^: (ue mask) in
      if width mask = 1 then b
      else f b (msbs mask)
    in
    f b (msbs b)

  let test n = 
    let a = to_list n in
    let b = Array.(to_list (init (1 lsl n) 
      (fun i -> binary_to_gray (consti n i))))
    in
    let c = List.map (fun x -> B.to_int (gray_to_binary x)) b in
    a=b, c=HardCaml.Utils.range (1 lsl n)

end


(***********************************************************************)

(* new statemchine implementation *)
let statemachine_binary rspec enable states = 
  let open HardCaml.Signal.Guarded in
  (* assign a value to each state *)
  let nstates = List.length states in
  let ls = HardCaml.Utils.clog2 nstates in
  let states = HardCaml.Utils.mapi (fun i s -> s, consti ls i) states in
  (* state variable *)
  let state_var = g_reg rspec enable ls in 
  (* update state *)
  let state_val s = 
    try List.assoc s states 
    with _ -> 
      (* report that we couldn't find the state.  We cant show which
        * one, as we don't know it's type (even if it will generally be 
        * a string *)
      failwith "couldn't find state"
  in
  let next_state s = state_var $== (state_val s) in
  let state_var = state_var#q (*-- "state_binary"*) in
  let switch cases = 
    g_switch (state_var) 
      (List.map (fun (s, c) -> state_val s, c) cases)
  in
  (fun s -> state_val s ==: state_var),
  switch, next_state

let statemachine_onehot rspec enable states = 
  let open HardCaml.Signal.Guarded in
  let nstates = List.length states in
  let onehot i = 
    let module B = HardCaml.Bits.Comb.IntbitsList in
    let ls = HardCaml.Utils.clog2 nstates in
    constibl B.((binary_to_onehot (consti ls i)).[nstates-1:0])
  in
  let states = HardCaml.Utils.mapi 
    (fun i s -> s, (i, onehot i)) states 
  in
  let state_var = 
    g_reg 
      HardCaml.Signal.Types.({ rspec with (* must be reset to get into state 0 *)
        reg_clear_value = one nstates;
        reg_reset_value = one nstates; })
    enable nstates in 
  (* update state *)
  let state_val s = 
    try List.assoc s states 
    with _ -> 
      (* report that we couldn't find the state.  We cant show which
        * one, as we don't know it's type (even if it will generally be 
        * a string *)
      failwith "couldn't find state"
  in
  let next_state s = state_var $== snd (state_val s) in
  let state_var = state_var#q (*-- "state_onehot"*) in
  let switch cases = 
    g_proc
      (List.map (fun (s, c) ->
        let i, _ = state_val s in
        g_when (bit state_var i) c) cases)
  in
  (fun s -> bit state_var (fst (state_val s))),
  switch, next_state

let statemachine_gray rspec enable states = 
  let open HardCaml.Signal.Guarded in
  (* assign a value to each state *)
  let nstates = List.length states in
  let ls = HardCaml.Utils.clog2 nstates in
  let gray i = 
    let module B = HardCaml.Bits.Comb.IntbitsList in
    let module G = Gray(B) in
    constibl (G.binary_to_gray (B.consti ls i))
  in
  let states = HardCaml.Utils.mapi (fun i s -> s, gray i) states in
  (* state variable *)
  let state_var = g_reg rspec enable ls in 
  (* update state *)
  let state_val s = 
    try List.assoc s states 
    with _ -> 
      (* report that we couldn't find the state.  We cant show which
        * one, as we don't know it's type (even if it will generally be 
        * a string *)
      failwith "couldn't find state"
  in
  let next_state s = state_var $== (state_val s) in
  let state_var = state_var#q (*-- "state_gray"*) in
  let switch cases = 
    g_switch state_var 
      (List.map (fun (s, c) -> state_val s, c) cases)
  in
  (fun s -> state_val s ==: state_var),
  switch, next_state

let statemachine ?(encoding=`binary) = 
  match encoding with
  | `binary -> statemachine_binary
  | `onehot -> statemachine_onehot
  | `gray -> statemachine_gray

(***********************************************************************)

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
        statemachine spec e states
 
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

    let multi_ram_wbr ?priority_write ~rd ~wr size =
        Multiram.ram ?priority_write ~ram:(Signal.Seq.ram_wbr ~spec:S.ram_spec)
          ~size ~spec:S.reg_spec ~wr ~rd

    let multi_ram_rbw ?priority_write ~rd ~wr size =
        Multiram.ram ?priority_write ~ram:(Signal.Seq.ram_rbw ~spec:S.ram_spec)
          ~size ~spec:S.reg_spec ~wr ~rd

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

(***********************************************************************)

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

let g_elif c t f = HardCaml.Signal.Guarded.([ g_if c t f ])

let ($==\) (x, l) y = 
  HardCaml.Signal.Guarded.( x $== insert x#q y l )


