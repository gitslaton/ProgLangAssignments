(* Programming Languages, Assignment 4 *)
(*
   You should write your functions in this file.
   You should NOT specify the types of your functions. Let the system determine
   them for you. This will result in types that the system may report to you
   different than the instructions, because of the type alias. You should make
   sure that the resulting types are equivalent to the requested ones.
   Write your code right below the corresponding comment describing the
   function you are asked to write.
*)

(* ----------------------------------------
                   THUNKS
   ---------------------------------------- *)

(*
   A "thunk" is an unevaluated expression. We can represent those as functions
   `unit -> 'a`. Such a function takes no input, but the body does not get
   evaluated until the function is called. For example we could define a thunk
   as the function `let th = fun () -> 3 + 4`. The expression `3 + 4` will only
   be evaluated if and when we call the thunk, like so: `th ()`.

   In the first part of this assignment, you will build functions that work with
   thunks. It is important that you make sure that expressions are not evaluated
   until they have to.

   Start by looking at the type definition of a thunk.
*)
type 'a thunk = unit -> 'a

(*let th = fun () -> 3 + 4
let curry f = fun x y -> f (x, y)*)
(*
   Write a function `thunk` that takes as input a function of type `unit -> 'a`
   and returns the `'a thunk` from it. This is an incredibly simple function.
   It should have type: (unit -> 'a) -> 'a thunk
*)

let thunk f = f

(*
   Write a function `thunk_of_value` that takes as input a value of type `'a` and
   returns the thunk that if evaluated would produce that value. Again an incredibly
   simple function.
   Should have type: 'a -> 'a thunk
*)

let thunk_of_value x = fun () -> x

(*
   Write a function `thunk_of_eval` that takes as input a pair of a function `'a -> 'b`
   and a value `'a` and returns the `'b thunk` that if called would apply the function
   to the value. This is also a very simple function. Make sure that the function
   is not applied until the thunk is evaluated.
   It should have type: ('a -> 'b) * 'a -> 'a thunk
*)

let thunk_of_eval (f, x) = fun () -> f x



(*
   Write a function `try_thunk` that takes as input a `'a thunk` and returns a value of
   type `'a option` as follows: If the thunk evaluates to a value `v`, it returns `Some v`.
   If the thunk raises any kind of exception, it returns `None`. You will need to use
   a `try-with` construct, search online for examples. Note that the part that comes
   after the "with" is a pattern.
   It should have type: 'a thunk -> 'a option
*)

let try_thunk f = 
   try Some (f ()) with
   | _ -> None

(*
   Write a function `thunk_of_pair` that takes as input a pair of thunks, and returns
   the thunk that if evaluated would produce the pair of values that those two thunks
   would give. Make sure that the two provided thunks are not evaluated until the
   returned thunk is called.
   It should have type: 'a thunk * 'b thunk -> ('a * 'b) thunk
*)

let thunk_of_pair (f, f') = fun () -> (f (), f' ())


(*
   Write a function `thunk_map` that takes as input a pair of a `'a thunk` and a
   function `'a -> 'b`, and returns a `'b thunk` that if called would return the
   result of evaluating the function with input the value produced by evaluating
   the first thunk. Make sure that the provided thunk is not evaluated until
   the returned thunk is called.
   It should have type: 'a thunk * ('a -> 'b) -> 'b thunk
*)

let thunk_map (f, k) = fun () -> let f_eval = f () in k f_eval 

(*
   Write a function `thunk_of_list` that takes as input a list of `'a thunk`s and
   returns a `'a list thunk` that when evaluated would in turn evaluate all the
   thunks in the list in their list order and return the resulting `'a list`.
   Make sure that no thunks on the list are evaluated until the returned thunk is
   called.
   It should have type: 'a thunk list -> 'a list thunk
*)

let rec thunk_of_list f_lst = 
   fun () -> match f_lst with
             | [] -> []
             | hd::tl -> hd ()::thunk_of_list tl ()



(* ----------------------------------------
               LOOKUP TABLES
   ---------------------------------------- *)
(*
   In this section we will create the basis for a simple lookup table system.
   A lookup table stores values of some type 'a indexed by "keys". The important
   operation for keys is that they should be comparable. We will use plain strings
   for that purpose. So the table holds pairs (key, value). We can store such a
   pair, or we can ask for the value stored for a particular key. Then the system
   is supposed to look through the various pairs stored in the table in search of
   a pair where the key matches the looked-for key. It then would return the
   corresponding value. If the key cannot be found, it would raise an exception.
   You will implement this basic idea here.

   We will represent a lookup table, or simply table, via a list of key-value
   pairs. You will need to maintain the invariant that the keys are all in
   increasing order. So when in search for a key you can always stop (and should
   stop) when the key you are searching for is larger than the next key in the
   list.

   You should never have two pairs with the same key in your list.

   It will be important when creating examples to keep in mind how strings compare.
   It is what is known as lexicographic ordering: Their first letters are compared,
   and if one is "earlier" then that string is smaller; if they are equal then the
   second letters are compared, and so on. We end up exhausting one of the strings
   in the process, then that one is smaller. For example:
   "foe" < "foo" < "fool" < "for"
   This is the default string behavior in OCAML, you do not need to do anything
   special for it.

   These lookup tables are often called symbol tables, and we will call the string
   keys symbols via a type alias.
*)

type symbol = string
type 'a table = (symbol * 'a) list

let empty : 'a table = []   (* A more intuitive notation for the empty list/table *)

(*
   Write a function `insert` that takes as input a triple of a symbol table, a symbol
   and a value, and it will return an updated table where we have inserted the pair
   of the symbol and the value in the table. If the table already has a pair with the
   same symbol, you must replace that pair. As a trivial example of inserting in the
   empty table we defined above:
   insert (empty, "foo", 3) = [("foo", 3)]
   It should have type: 'a table * symbol * 'a -> 'a table
*)

let rec insert (sym_tbl, in_sym, in_v) = 
   match sym_tbl with
   | [] -> [(in_sym, in_v)]
   | (s, v)::[] -> if in_sym < s 
                   then (in_sym, in_v)::(s, v)::[]
                   else if in_sym = s 
                        then (in_sym, in_v)::[]
                        else (s, v)::(in_sym, in_v)::[]
   | (s, v)::(s_ahead, v_ahead)::tl -> let tl' = (s_ahead, v_ahead)::tl in
                                       if in_sym < s
                                       then (in_sym, in_v)::(s, v)::insert(tl', s_ahead, v_ahead)
                                       else if in_sym = s
                                            then (in_sym, in_v)::insert(tl', s_ahead, v_ahead)
                                            else (s, v)::insert(tl', in_sym, in_v) 

(*
   Write a function `has` that takes as input a pair of a symbol table and a symbol
   and returns a boolean of whether the symbol table contains a pair with key that
   symbol.
   It should not look any further in the list than is necessary, i.e. once the stored
   keys are bigger than the searched-for key there is no need to continue the search.
   It should have type: 'a table * symbol -> bool
*)

let rec has (sym_tbl, s) =
   match sym_tbl with
   | [] -> false
   | (s', v)::[] -> s = s'
   | (s', v)::tl -> not (s' > s) && (s = s' || has (tl, s))
                        
(*
   Write a function `lookup` that takes as input a pair of a symbol table and a
   symbol. If it finds in the table a pair with that symbol as a key then it
   returns the corresponding value. If the key does not appear in the table, then
   it should raise the exception `Not_found`.
   It should not use `has`.
   It should not look any further in the list than is necessary.
   It should have type: 'a table * symbol -> 'a
*)

let rec lookup (sym_tbl, s) = 
   match sym_tbl with
   | [] -> raise (Not_found)
   | (s', v)::tl -> if s' = s
                   then v
                   else if s' > s
                        then raise (Not_found) 
                        else lookup (tl, s)

(*
   Write a function `lookup_opt` that takes as input a pair of a symbol table and a
   symbol. If it finds in the table a pair with that symbol as a key then it
   returns `Some v` where `v` is the corresponding value. If the key does not appear
   in the table, then it should return `None`.
   It should not use `has` or `lookup`.
   It should not look any further in the list than is necessary.
   It should have type: 'a table * symbol -> 'a option
*)

let rec lookup_opt (sym_tbl, s) = 
   match sym_tbl with
   | [] -> None
   | (s', v)::tl -> if s' = s
                    then Some v
                    else if s' > s
                         then None
                         else lookup_opt(tl, s)

(*
   Write a function `delete` that takes as input a pair of a symbol table and a
   symbol. It returns the symbol table that results from the removal of the pair
   with key that symbol, if there was any such key.
   It should not use `has` or any of the other functions.
   It should have type: 'a table * symbol -> 'a table
*)

let rec delete (sym_tbl, s) = 
   match sym_tbl with
   | [] -> []
   | (s', v)::tl -> if s' = s
                    then tl
                    else if s' > s
                         then sym_tbl
                         else (s', v)::(delete (tl, s))


(*
   Write a function `keys` that takes as input a symbol table and returns a list
   of the keys in the table.
   It should have type: 'a table -> symbol list
*)

let rec keys sym_tbl = 
   match sym_tbl with
   | [] -> []
   | (s, v)::tl -> s::keys tl

(*
   Write a function `is_proper` that takes as input a symbol table and returns
   a boolean indicating if the table is "proper", namely if the invariant is
   maintained that they keys appear in strictly increasing order.
   It should have type: 'a table -> bool
*)
let rec is_proper sym_tbl =
   match sym_tbl with
   | [] -> true
   | hd::[] -> true
   | (s, v)::(s', v')::tl -> s < s' && is_proper tl
