exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

type exprS = NumS of float | ArithS of string * exprS * exprS |
			    CompS of string * exprS * exprS |
	   		 BoolS of bool| IfS of exprS * exprS * exprS |
	   		 OrS of exprS * exprS | AndS of exprS * exprS | NotS of exprS |
	   		 EqS of exprS * exprS | NeqS of exprS * exprS

type exprC = NumC of float | ArithC of string * exprC * exprC |
			    CompC of string * exprC * exprC |
			    BoolC of bool | IfC of exprC * exprC * exprC |
			    EqC of exprC * exprC

type value = Num of float | Bool of bool

(* Environment lookup *)
type 'a env
val empty : 'a env
val lookup : string -> 'a env -> 'a option
val bind :  string -> 'a -> 'a env -> 'a env

(* Interpreter steps *)
val desugar : exprS -> exprC
val interp : value env -> exprC -> value
val evaluate : exprC -> value

(* result post-processing *)
val valToString : value -> string
