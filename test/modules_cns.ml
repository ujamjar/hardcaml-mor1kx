open HardCaml

module type Module = sig
  module I : Interface.S
  module O : Interface.S
  type t
  type a
  val name : string
  val f : a -> t I.t -> t O.t
end

module type Module_signal = Module with type t = Signal.Comb.t

module type Submodule = functor (M:Module) -> Module

module Submodule_flat(M : Module) = struct
  type a = M.a
  type t = M.t
  module I = M.I
  module O = M.O
  let name = M.name
  let f arg i = M.f arg i
end

let db = Circuit.Hierarchy.empty()

module Submodule_hier(M : (Module with type t = Signal.Comb.t)) = (struct
  type a = M.a
  type t = M.t
  module I = M.I
  module O = M.O
  let name = M.name
  module Inst = Interface.Hier(I)(O)
  let f arg i = 
    Inst.make db name (M.f arg) i
end : Module_signal)

module M1(B : Comb.S)(Submodule : Submodule) = struct
  type a = unit
  type t = B.t
  module I = interface a[1] b[1] end
  module O = interface c[1] end
  let name = "M1"
  let f () i = O.{ c = I.(B.(|:) i.a i.b) }
end

module M2(B : Comb.S)(Submodule : Submodule) = struct
  type a = int
  type t = B.t
  module I = interface a[1] b[1] end
  module O = interface c[1] end
  let name = "M1"
  let f _ i = 
    let module M1 = Submodule(M1(B)(Submodule)) in
    O.{ c = I.(B.(|:) i.a i.b) }

end


