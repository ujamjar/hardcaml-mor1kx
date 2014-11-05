(******************************************************************************
 This Source Code Form is subject to the terms of the
 Open Hardware Description License, v. 1.0. If a copy
 of the OHDL was not distributed with this file, You
 can obtain one at http://juliusbaxter.net/ohdl/ohdl.txt

 Description: Branch prediction module
 Generates a predicted flag output and compares that to the real flag
 when it comes back in the following pipeline stage.
 Signals are deliberately not named after the pipeline stage they belong to,
 in order to keep this module generic.

 Copyright (C) 2013 Stefan Kristiansson <stefan.kristiansson@saunalahti.fi>

 ******************************************************************************)

open HardCaml.Signal.Comb

module I = interface
    (* Signals belonging to the stage where the branch is predicted. *)
    op_bf[1] op_bnf[1] immjbr_upper[10]
    (* Signals belonging to the stage where the branch is resolved. *)
    prev_op_brcond[1] prev_predicted_flag[1] flag[1]
end

module O = interface
    predicted_flag[1]
    (* Branch misprediction indicator *)
    branch_mispredict[1]
end

let branch_prediction i = 
  let open I in
  let immjbr_upper = bit i.immjbr_upper 9 in

  (* Compare the real flag with the previously predicted flag and signal a
     misprediction in case of a mismatch. *)
  let branch_mispredict = i.prev_op_brcond &: (i.flag <>: i.prev_predicted_flag) in
   (* Static branch prediction - backward branches are predicted as taken,
      forward branches as not taken. *)
  let predicted_flag = (i.op_bf &: immjbr_upper) |: (i.op_bnf &: ~: immjbr_upper) in

  O.({ predicted_flag; branch_mispredict; })


