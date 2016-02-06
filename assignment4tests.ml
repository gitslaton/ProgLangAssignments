(* THUNKS *)
(* This complicated test ensures you don't call the thunk too soon *)
let t1a = let f = fun () -> raise (Failure "")
          in try (try (thunk f) with Failure "" -> (fun () -> false)) ()
             with Failure "" -> true
                | _ -> false

let t1b = (thunk (fun () -> 5)) () = 5

let t2a = (thunk_of_value 4) () = 4

let t3a = try (try (thunk_of_eval ((fun x -> raise (Failure "")), 4))
               with Failure "" -> (fun () -> false)) ()
          with Failure "" -> true
             | _ -> false
let t3b = thunk_of_eval ((fun x -> x + 1), 5) () = 6

let t4a = try_thunk (fun () -> raise (Failure "hi")) = None

let t5a = let f = fun () -> raise (Failure "")
          in try (try (thunk_of_pair (f, f)) with Failure "" -> (fun () -> (1, 1))) () =
                  (0, 0)
             with Failure "" -> true
                | _ -> false
let t5b = thunk_of_pair ((fun () -> 4), (fun () -> 5)) () = (4, 5)

let t6a = let f = fun () -> raise (Failure "")
          in try (try thunk_map (f, f)
                  with Failure "" -> (fun () -> false)) ()
             with Failure "" -> true
                | _ -> false
let t6b = thunk_map ((fun () -> 4), (fun x -> 2 * x)) () = 8

let t7a = let f = fun () -> raise (Failure "")
          in try (try thunk_of_list [f; f]
                  with Failure "" -> (fun () -> [])) () = []
             with Failure "" -> true
                | _ -> false
let t7b = let f = fun () -> 5
          in thunk_of_list [f; f] () = [5; 5]

let t8a = insert (empty, "foo", 3) = [("foo", 3)]

let t9a = has ([("foo", 2)], "foo") = true

let t10a = lookup ([("bar", 3); ("foo", 2)], "bar") = 3
let t10b = try (lookup ([("bar", 3); ("foo", 2)], "baz"); false)
           with Not_found -> true
(* In the following test the search should fail because your code
   should stop looking after baz, since "baz" > "bar".
   This is of course not a "proper" table, but it is a good test that
   your code behaves properly. *)
let t10c = try (lookup ([("baz", 3); ("bar", 2)], "bar"); false)
           with Not_found -> true

let t11a = lookup_opt ([("bar", 3); ("foo", 2)], "bar") = Some 3
(* Again the search should be stopping after "foo" *)
let t11b = lookup_opt ([("foo", 2); ("bar", 3)], "bar") = None

let t12a = delete ([("bar", 3); ("baz", 1); ("foo", 2)], "baz") = [("baz", 1); ("foo", 2)]

let t13a = keys [("bar", 3); ("foo", 2)] = ["bar"; "foo"]

let t14a = is_proper [("bar", 3); ("foo", 2)] = true
