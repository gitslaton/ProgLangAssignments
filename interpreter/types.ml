exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
type exprS = NumS of float | ArithS of string * exprS * exprS |
	           CompS of string * exprS * exprS |
             BoolS of bool | IfS of exprS * exprS * exprS | 
             OrS of exprS * exprS | AndS of exprS * exprS | NotS of exprS |
             EqS of exprS * exprS | NeqS of exprS * exprS |
             TupS of exprS * exprS
(* You will need to add more cases here. *)
type exprC = NumC of float | ArithC of string * exprC * exprC |
	           CompC of string * exprC * exprC | 
             BoolC of bool | IfC of exprC * exprC * exprC |
             EqC of exprC * exprC |
             TupC of exprC * exprC


(* You will need to add more cases here. *)
type value = Num of float | Bool of bool

type 'a env = (string * 'a) list
let empty = []

(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = 
  match env with
  | []          -> None
  | (s,v) :: tl -> if s = str then Some v else lookup str tl
  
(* val bind :  string -> 'a -> 'a env -> 'a env *)
let bind str v env = (str, v) :: env


(*
   HELPER METHODS
   You may be asked to add methods here. You may also choose to add your own
   helper methods here.
*)
let arithEval s e1 e2 = 
  match (e1, e2) with
  | (Num x, Num y) -> Num (match s with
                           | "+" -> x +. y 
                           | "-" -> x -. y
                           | "*" -> x *. y
                           | "/" -> if y = 0.0 then raise (Interp "cannot divide by zero") else x /. y
                           | _   -> raise (Interp "not an allowed symbol"))
  | _ -> raise (Interp "not a num") 

let compEval s e1 e2 = 
   match (e1, e2) with
   | (Num x, Num y) -> Bool (match s with
   		             | ">"  -> x > y
   			     | ">=" -> x >= y
   			     | "<"  -> x < y
   			     | "<=" -> x <= y
   			     | _ -> raise (Interp "not an allowed symbol"))
   | _ -> raise (Interp "one or both expressions are not nums")

let eqEval e1 e2 = 
	match (e1, e2) with
	| (Num x, Num y)   -> Bool (x = y)
	| (Bool a, Bool b) -> Bool (a = b)
	| _                -> Bool false
(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = 
  match exprS with
  | NumS i                    -> NumC i
  | BoolS b 	              -> BoolC b
  | IfS (ifS, thenS, elseS)   -> IfC (desugar ifS, desugar thenS, desugar elseS)
  | NotS (bS)                 -> desugar (IfS (bS, BoolS false, BoolS true))
  | OrS (ifS, ifS')           -> desugar (IfS (ifS, BoolS true, (IfS (ifS', BoolS true, BoolS false))))
  | AndS (ifS, ifS')          -> desugar (IfS (ifS, (IfS (ifS', BoolS true, BoolS false)), BoolS false))
  | ArithS (op, eS1, eS2)     -> (let x = desugar eS1 in let y = desugar eS2 in
                                  match (desugar eS1, desugar eS2) with
                                  | (NumC _, _)
                                  | (_, NumC _)
                                  | (ArithC _, _)
                                  | (_, ArithC _) -> ArithC (op, x, y)
                                  | _             -> raise (Desugar "not valid arithmatic expressions"))
  | CompS (op, eS1, eS2)       -> CompC (op, desugar eS1, desugar eS2)
  | EqS (eS1, eS2)	       -> EqC (desugar eS1, desugar eS2)
  | NeqS (eS1, eS2)	       -> desugar (NotS (EqS (eS1, eS2)))


(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r =
  match r with
  | NumC i                  -> Num i
  | BoolC b 	            -> Bool b
  | IfC (ifC, thenC, elseC) -> let e = interp env ifC in
                               (match e with
                                | Bool b -> if b then interp env thenC else interp env elseC
                                | _  -> raise (Interp "not a bool"))
  | ArithC (op, ec1, ec2)   -> let x = interp env ec1 in let y = interp env ec2 in arithEval op x y 
  | CompC (s, ec1, ec2)     -> let x = interp env ec1 in let y = interp env ec2 in compEval s x y
  | EqC (ec1, ec2)          -> let a = interp env ec1 in let b = interp env ec2 in eqEval a b 

(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []


(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = 
  match r with
  | Num i           -> string_of_float i
  | Bool b          -> string_of_bool b
