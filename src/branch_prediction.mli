module I : interface
    (* Signals belonging to the stage where the branch is predicted. *)
    op_bf op_bnf immjbr_upper
    (* Signals belonging to the stage where the branch is resolved. *)
    prev_op_brcond prev_predicted_flag flag
end

module O : interface
    predicted_flag
    (* Branch misprediction indicator *)
    branch_mispredict
end

val branch_prediction : HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t

