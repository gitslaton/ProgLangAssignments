(* CALCULATIONS *)
let t8a = has_vars (Add (Var, Int 2))
let t8b = not (has_vars (Add (Int 1, Int 2)))

let t9a = count_vars (Add (Var, Int 2)) = 1
let t9b = count_vars (Add (Int 1, Int 2)) = 0

let t10a = calc_eval (Add (Var, Int 2), 3) = 5

let t11a = func_of_calc (Add (Var, Int 2)) 3 = 5

let t12a = subst (Add (Var, Int 1), Mul (Var, Var)) =
                Mul (Add (Var, Int 1), Add (Var, Int 1))

let t13a = power 3 = Mul (Mul (Var, Var), Var)

let t14a = term (2, 1) = Mul(Int 2, Var)

let t15a = poly [(2, 1); (1, 4)] = Add (term (2, 1), term (1, 4))

let t16a = simplify (Add (Int 0, Var)) = Var
let t16b = simplify (Add (Int 3, Int 4)) = Int 7
let t16c = calc_eval (simplify (poly [(2, 1); (1, 0)]), 3) = 7
