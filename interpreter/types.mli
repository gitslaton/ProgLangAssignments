exception Desugar_failed

type resultS = NumS of float
type resultC = NumC of float
type value = Num of float

(* Environment lookup *)
type 'a env
val lookup : string -> 'a env -> 'a option
val bind :  string -> 'a -> 'a env -> 'a env

(* Interpreter steps *)
val desugar : resultS -> resultC
val interp : value env -> resultC -> value
val evaluate : resultC -> value

(* result post-processing *)
val valToString : value -> string
