open Camlp4.PreCast
open Syntax

(* This simple syntax extension makes the standard bit operators have
 * the same precedence as verilog.  Useful for porting code that doesnt
 * do much bracketing!
 *
 * We also add (||:) and (&&:) operators.  
 *
 * From lowest to highest we have:
 * (||:) -> (&&:) -> (|:) -> (^:) -> (&:)
 *)

let () = 
  EXTEND Gram
    GLOBAL: expr;

    expr: LEVEL "||" 
    [ [ e0 = SELF; "||:"; e1 = SELF -> <:expr<$e0$ ||: $e1$>> ] ];
    expr: LEVEL "&&" 
    [ [ e0 = SELF; "&&:"; e1 = SELF -> <:expr<$e0$ &&: $e1$>> ] 
    | "BITOR"  [ e0 = SELF; "|:"; e1 = SELF -> <:expr<$e0$ |: $e1$>> ]
    | "BITXOR" [ e0 = SELF; "^:"; e1 = SELF -> <:expr<$e0$ ^: $e1$>> ] 
    | "BITAND" [ e0 = SELF; "&:"; e1 = SELF -> <:expr<$e0$ &: $e1$>> ]
    ];

END

