(*
  let rifel l = R.reg_fb ~e:vdd ~w:(width @@ snd @@ List.hd l) (fun d ->
        (List.fold_left (fun f (c,d) -> (fun s -> f (mux2 c d s)))
          (fun s -> s) l) d)
      in
*)

(*  various cases related to

  if (c0) q <= d0;
  else if (c1) q <= d1;
  ...
  else (cn) q<= dn

*)

open S

let pmux list last = 
  (List.fold_left (fun f (c, d) -> (fun s -> f (mux2 c d s))) (fun s -> s) list) last

let c0,d0 = gnd,consti 2 0
let c1,d1 = vdd,consti 2 1
let def = consti 2 2
let l = [ c0,d0; c1,d1 ]

(* if c0 then d1
 * else if c1 then d2
 * else def *)
let a = pmux l def
(* if c0 then d1
 * else if c1 then d2
 * else din *)
let b = R.reg_fb ~e:vdd ~w:2 (pmux l)
(* if c0 then d1
 * else if c1 then d2
 * else def *)
let c = R.reg ~e:vdd (pmux l def)

(* if c0 then din
 * else if c1 then d2
 * else din *)
let d = R.reg_fb ~e:vdd (fun d ->
  pmux [
    c0, d;
    c1, d1
  ] d)

