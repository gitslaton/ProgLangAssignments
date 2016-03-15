exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
type exprS = NumS of float |
             BoolS of bool | IfS of exprS * exprS * exprS | OrS of exprS * exprS | AndS of exprS * exprS | NotS of exprS

(* You will need to add more cases here. *)
type exprC = NumC of float | 
             BoolC of bool | IfC of exprC * exprC * exprC | ArithC of string * exprC * exprC


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
(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = 
  match exprS with
  | NumS i                  -> NumC i
  | BoolS b 	              -> BoolC b
  | IfS (ifS, thenS, elseS) -> (match desugar ifS with
                               | BoolC b -> if b then desugar thenS else desugar elseS
                               | _ -> raise (Interp "not a bool!"))
  | NotS (bS)              -> desugar (IfS (bS, BoolS false, BoolS true))
  | OrS (ifS, ifS')          -> (match (desugar ifS, desugar ifS') with
                                | (BoolC b, BoolC b') -> if b 
                                                         then BoolC true 
                                                         else if b'
                                                              then BoolC true
                                                              else BoolC false
                                | _ -> raise (Interp "One or both expressions are not bools"))
  | AndS (ifS, ifS')          -> (match (desugar ifS, desugar ifS') with
                                  | (BoolC b, BoolC b') -> if b 
                                                           then if b'
                                                                then BoolC true
                                                                else BoolC false 
                                                           else BoolC false
                                  | _ -> raise (Interp "One or both expressions are not bools"))

let arithEval s ec1 ec2 = 
  match (ec1, ec2) with
  | (Num x, Num y) -> (match s with
                       | "+" -> x + y 
                       | "-" -> x .- y
                       | "*" -> x .* y
                       | "/" -> if y .= 0.0 then raise (Interp "cannot divide by zero") else x ./ y
                       | _ -> raise (Interp "not an allowed symbol!"))
  | _ -> raise (Interp "not a num!") 

(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r =
  match r with
  | NumC i        -> Num i
  | BoolC b 	  -> Bool b
  | IfC (ifC, thenC, elseC) -> (match interp env ifC with
                                | Bool b -> if b then interp env thenC else interp env elseC
                                | _      -> raise (Interp "Not a bool"))
  | ArithC (op, ec1, ec2) -> let x = interp env ec1 in let y = inter env ec2 in
                             arithEval (op, x, y)




(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []


(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = 
  match r with
  | Num i           -> string_of_float i
  | Bool b          -> string_of_bool b