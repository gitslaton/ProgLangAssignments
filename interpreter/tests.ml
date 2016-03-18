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
(* normal evaluation and desugaring *)
let t2a =  evaluate (IfC (BoolC true, NumC 2.3, NumC 4.3)) = Num 2.3 

(* test and or not C and S *)
let t5a = evaluate (desugar (AndS (BoolS true, BoolS true))) = Bool true
let t5b = evaluate (desugar (AndS (BoolS false, BoolS true))) = Bool false
let t5c = evaluate (desugar (AndS (BoolS false, BoolS false))) = Bool false

let t5d = evaluate (desugar (OrS (BoolS true, BoolS false))) = Bool true
let t5e = evaluate (desugar (OrS (BoolS false, BoolS false))) = Bool false
let t5f = evaluate (desugar (OrS (BoolS true, BoolS true))) = Bool true

let t5g = evaluate (desugar (NotS (BoolS true))) = Bool false
let t5h = evaluate (desugar (NotS (BoolS false))) = Bool true

let t5i = evaluate (desugar (NotS ((AndS (BoolS true, BoolS false))))) = Bool true

(*test arith C and S*)
let t3a = evaluate (desugar (ArithS ("+", NumS 1.0, NumS 2.0))) = Num 3.0
let t3b = evaluate (ArithC ("-", NumC 10.0, NumC 3.0)) = Num 7.0
let t3c = evaluate (ArithC ("*", NumC 3.0, NumC 4.0)) = Num 12.0
let t3d = evaluate (ArithC ("/", NumC 7.0, NumC 2.0)) = Num 3.5

let t3g = desugar (ArithS ("+", NumS 1.0, NumS 2.0)) = ArithC ("+", NumC 1.0, NumC 2.0)
let t3h = evaluate (desugar (ArithS ("+", (ArithS ("*", NumS 3.0, NumS 2.0)), (ArithS ("/", NumS 6.0, NumS 2.0))))) = Num 9.0

(*test comparison operator and Eq C and S *)
let t4a = evaluate (desugar (EqS (BoolS true, BoolS true))) = Bool true
let t4b = evaluate (desugar (EqS (BoolS false, BoolS true))) = Bool false
let t4c = evaluate (EqC (BoolC true, NumC 1.0)) = Bool false
let t4d = evaluate (desugar (EqS (NumS 13.0, NumS 0.0))) = Bool false
let t4e = evaluate (EqC (NumC 1.0, NumC 1.0)) = Bool true


(* exception testing *)
let t3e = try (ignore (evaluate (ArithC ("/", NumC 1.0, NumC 0.0))); false) with
	      | Interp "cannot divide by zero" -> true
	      | _ -> false
let t3f = try (ignore (evaluate (ArithC ("_", NumC 1.0, NumC 1.0))); false) with
		  | Interp "not an allowed symbol" -> true
		  | _ -> false 	      



(* nested expression testing *)




