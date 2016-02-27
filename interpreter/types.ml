exception Desugar_failed

type resultS = NumS of float
type resultC = NumC of float
type 'a env = (string * 'a) list
type value = Num of float

(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = match env with
  | []          -> None
  | (s,v) :: tl -> if s = str then Some v else lookup str tl
(* val bind :  string -> 'a -> 'a env -> 'a env *)
let bind str v env = (str, v) :: env


(* INTERPRETER *)

(* desugar : resultS -> resultC *)
let rec desugar expr = match expr with
  | NumS i        -> NumC i

(* interp : Value env -> resultC -> value *)
let rec interp env r = match r with
  | NumC i        -> Num i

(* evaluate : resultC -> val *)
let evaluate exprC = exprC |> interp []



let rec valToString r = match r with
  | Num i           -> string_of_float i
