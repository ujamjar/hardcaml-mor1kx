(******************************************************************************
 This Source Code Form is subject to the terms of the
 Open Hardware Description License, v. 1.0. If a copy
 of the OHDL was not distributed with this file, You
 can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

 Copyright (C) 2012 Stefan Wallentowitz <stefan.wallentowitz@tum.de>
 Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

 ******************************************************************************)

module type S = sig 
  val numways : int
end

module Make(S : S)(M : Utils.Module_cfg) : sig

  module I : interface
    current
    access
  end

  module O : interface
    update
    lru_pre
    lru_post
  end

  val cache_lru : M.Bits.t I.t -> M.Bits.t O.t
  val cache_lru_inst : M.Bits.t I.t -> M.Bits.t O.t

end

