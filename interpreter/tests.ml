open Types

(* You can test expressions of type resultS or resultC and how they are evaluated *)
(* These will only work once you have compiled types.ml *)

(* This is the one kind of test you can write. *)
let t0a = evaluate (NumC 2.3) = Num 2.3
let t1a = evaluate (BoolC false) = Bool false
let t1b = evaluate (BoolC true) = Bool true

(* You can also use interp directly to specify a custom environment. *)
let t0b = let env1 = bind "x" (Num 3.1) empty
          in interp env1 (NumC 2.3) = Num 2.3
let t1c = let env2 = bind "c" (Bool false) empty
		  in interp env2 (BoolC false) = Bool false

(* You can also test desugar to make sure it behaves properly. *)
let t0c = desugar (NumS 2.3) = NumC 2.3
let t1d = desugar (BoolS false) = BoolC false

(* Or you can combine with evaluate to get to the final value. *)
let t0d = evaluate (desugar (NumS 2.3)) = Num 2.3
let t1e = evaluate (desugar (BoolS false)) = Bool false

let t2a =  evaluate (IfC (BoolC true, NumC 2.3, NumC 4.3)) = Num 2.3 
let t2b = evaluate (IfC (BoolC false, NumC 2.3, NumC 4.3)) = Num 4.3
let t2c = desugar (NotS (BoolS false)) = BoolC true
let t2l = desugar (NotS (BoolS true)) = BoolC false

let t2d = desugar (AndS (BoolS true, BoolS true)) = BoolC true
let t2e = desugar (AndS (BoolS false, BoolS true)) = BoolC false
let t2f = desugar (AndS (BoolS true, BoolS false)) = BoolC false
let t2g = desugar (AndS (BoolS false, BoolS false)) = BoolC false

let t2h = desugar (OrS (BoolS true, BoolS false)) = BoolC true
let t2i = desugar (OrS (BoolS false, BoolS true)) = BoolC true
let t2j = desugar (OrS (BoolS false, BoolS false)) = BoolC false
let t2k = desugar (OrS (BoolS true, BoolS true)) = BoolC true
