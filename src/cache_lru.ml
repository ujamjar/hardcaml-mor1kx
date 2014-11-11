(******************************************************************************
 This Source Code Form is subject to the terms of the
 Open Hardware Description License, v. 1.0. If a copy
 of the OHDL was not distributed with this file, You
 can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

 Description: Data cache LRU implementation

 Copyright (C) 2012 Stefan Wallentowitz <stefan.wallentowitz@tum.de>
 Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

 ******************************************************************************)

module type S = sig 
  val numways : int
end

module Make(C : S)(M : Utils.Module_cfg) = struct
  
  open M.Bits

  let vwidth = C.numways * (C.numways-1) / 2

  module I = interface
    current[vwidth]
    access[C.numways]
  end

  module O = interface
    update[vwidth]
    lru_pre[C.numways]
    lru_post[C.numways]
  end

  let idx i j = 
    let rec f n off =
      if n=i then off+j-i-1
      else f (n+1) (off+C.numways-n-1)
    in
    f 0 0

  let upper_diag = 
    let rec g i j = 
      if i = C.numways then []
      else if j = C.numways then g (i+1) (i+2)
      else (i,j) :: g i (j+1)
    in
    g 0 1

  let expand current = 
    Array.init C.numways (fun i ->
      Array.init C.numways (fun j ->
        if i = j then vdd
        else if j > i then bit current (idx i j)
        else ~: (bit current (idx j i))))

  let compress expand = 
    concat @@ List.rev @@ List.map (fun (i,j) -> expand.(i).(j)) upper_diag

  let mask expand access = 
    let access = Array.init (width access) (fun i -> access.[i:i]) in
    Array.mapi 
      (fun i a -> 
        let c = access.(i) in
        Array.mapi 
          (fun j x -> 
            let d = access.(j) in
            if i = j then x
            else mux2 c gnd (mux2 d vdd x)) a) expand

  let lru expand = 
    concat @@ List.rev @@ Array.to_list @@
      Array.map (fun a -> reduce (&:) (Array.to_list a)) expand

  let cache_lru i = 
    let open I in

    let expand = expand i.current in
    let lru_pre = lru expand in
    let expand = mask expand i.access in
    let update = compress expand in
    let lru_post = lru expand in

    O.{
      update;
      lru_pre;
      lru_post;
    }

  module Inst = M.Inst(I)(O)
  let cache_lru_inst = Inst.inst "cache_lru" cache_lru 

end

