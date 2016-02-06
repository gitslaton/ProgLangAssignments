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

(*
   Write a function `thunk` that takes as input a function of type `unit -> 'a`
   and returns the `'a thunk` from it. This is an incredibly simple function.
   It should have type: (unit -> 'a) -> 'a thunk
*)



(*
   Write a function `thunk_of_value` that takes as input a value of type `'a` and
   returns the thunk that if evaluated would produce that value. Again an incredibly
   simple function.
   Should have type: 'a -> 'a thunk
*)



(*
   Write a function `thunk_of_eval` that takes as input a pair of a function `'a -> 'b`
   and a value `'a` and returns the `'b thunk` that if called would apply the function
   to the value. This is also a very simple function. Make sure that the function
   is not applied until the thunk is evaluated.
   It should have type: ('a -> 'b) * 'a -> 'a thunk
*)




(*
   Write a function `try_thunk` that takes as input a `'a thunk` and returns a value of
   type `'a option` as follows: If the thunk evaluates to a value `v`, it returns `Some v`.
   If the thunk raises any kind of exception, it returns `None`. You will need to use
   a `try-with` construct, search online for examples. Note that the part that comes
   after the "with" is a pattern.
   It should have type: 'a thunk -> 'a option
*)



(*
   Write a function `thunk_of_pair` that takes as input a pair of thunks, and returns
   the thunk that if evaluated would produce the pair of values that those two thunks
   would give. Make sure that the two provided thunks are not evaluated until the
   returned thunk is called.
   It should have type: 'a thunk * 'b thunk -> ('a * 'b) thunk
*)



(*
   Write a function `thunk_map` that takes as input a pair of a `'a thunk` and a
   function `'a -> 'b`, and returns a `'b thunk` that if called would return the
   result of evaluating the function with input the value produced by evaluating
   the first thunk. Make sure that the provided thunk is not evaluated until
   the returned thunk is called.
   It should have type: 'a thunk * ('a -> 'b) -> 'b thunk
*)



(*
   Write a function `thunk_of_list` that takes as input a list of `'a thunk`s and
   returns a `'a list thunk` that when evaluated would in turn evaluate all the
   thunks in the list in their list order and return the resulting `'a list`.
   Make sure that no thunks on the list are evaluated until the returned thunk is
   called.
   It should have type: 'a thunk list -> 'a list thunk
*)




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



(*
   Write a function `has` that takes as input a pair of a symbol table and a symbol
   and returns a boolean of whether the symbol table contains a pair with key that
   symbol.
   It should not look any further in the list than is necessary, i.e. once the stored
   keys are bigger than the searched-for key there is no need to continue the search.
   It should have type: 'a table * symbol -> bool
*)



(*
   Write a function `lookup` that takes as input a pair of a symbol table and a
   symbol. If it finds in the table a pair with that symbol as a key then it
   returns the corresponding value. If the key does not appear in the table, then
   it should raise the exception `Not_found`.
   It should not use `has`.
   It should not look any further in the list than is necessary.
   It should have type: 'a table * symbol -> 'a
*)



(*
   Write a function `lookup_opt` that takes as input a pair of a symbol table and a
   symbol. If it finds in the table a pair with that symbol as a key then it
   returns `Some v` where `v` is the corresponding value. If the key does not appear
   in the table, then it should return `None`.
   It should not use `has` or `lookup`.
   It should not look any further in the list than is necessary.
   It should have type: 'a table * symbol -> 'a option
*)



(*
   Write a function `delete` that takes as input a pair of a symbol table and a
   symbol. It returns the symbol table that results from the removal of the pair
   with key that symbol, if there was any such key.
   It should not use `has` or any of the other functions.
   It should have type: 'a table * symbol -> 'a table
*)



(*
   Write a function `keys` that takes as input a symbol table and returns a list
   of the keys in the table.
   It should have type: 'a table -> symbol list
*)



(*
   Write a function `is_proper` that takes as input a symbol table and returns
   a boolean indicating if the table is "proper", namely if the invariant is
   maintained that they keys appear in strictly increasing order.
   It should have type: 'a table -> bool
*)

