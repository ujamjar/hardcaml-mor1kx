(******************************************************************************
 This Source Code Form is subject to the terms of the
 Open Hardware Description License, v. 1.0. If a copy
 of the OHDL was not distributed with this file, You
 can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

 Copyright (C) 2012 Stefan Wallentowitz <stefan.wallentowitz@tum.de>
 Copyright (C) 2014 Andy Ray <andy.ray@ujamjar.com>

 ******************************************************************************)

module I : interface
  current
  access
end

module O : interface
  update
  lru_pre
  lru_post
end

val cache_lru : HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t

