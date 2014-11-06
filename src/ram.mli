module type S = sig
  val addr_width : int
  val data_width : int
end

module Simple_dp(S : S) : sig

  module I : interface
    clk
    raddr re
    waddr we
    din
  end

  module O : interface
    dout
  end

end

module True_dp(S : S) : sig
  
  module I : interface
    clk
    addr_a we_a din_a
    addr_b we_b din_b
  end

  module O : interface
    dout_a dout_b
  end

end


