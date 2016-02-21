(* Programming Languages, Assignment 6 *)
(*
   You should write your functions in this file.
   You should NOT specify the types of your functions. Let the system determine
   them for you.

   Write your code right below the corresponding comment describing the
   function you are asked to write.
*)


(* ---------------------------------
              STREAMS
   --------------------------------- *)
(*
   In this assignment we will learn about streams, though they will be a
   bit different from the streams you may have seen before. A "stream" is meant
   to be a structure that prints out one new value at a time. In order to obtain
   this value we "call" the stream, and what we get back is the value along with
   the new stream to call in order to get the next value. Streams keep producing
   values forever. A simple stream is one that returns the same value all the
   time. Another is a stream that produces the natural numbers one after the
   other, like 1, 2, 3, ...

   In this assignment you will implement a number of methods for streams. We start
   with the definition of a stream, which will take a moment to digest. It is a
   self-referential definition with a single type variant. There is no "terminating"
   point like with lists. For some technical reasons however we do need to define
   this type variant with only one alternative, you cannot have a self-referential
   type otherwise, we cannot for instance say `type t = unit -> int * t` even though
   we would like to. But this is how you should think of streams: They are "thunks",
   i.e. functions with no arguments, that when called produce a value and a new stream.
*)
type 'a stream = St of (unit -> 'a * 'a stream)

(* This function is provided as a small sample of a way to work with streams, as well
   as a minimal testing tool.
   It returns the first entry in the stream. The stream `st'` can be used to
   work with the rest of the original stream's values.
*)
let take1 (St th) =      (* Pattern match on the stream variant. *)
   let (v, st') = th () in v   (* Call the corresponding thunk to get value and the remaining stream *)

(*
   TIPS:
   - As in the example of `take1` above, you will often find it beneficial to use the
   pattern `(St th)` in place of `st` in your function arguments. This will not always
   be the case, but it often will. Think of whether you need to call on the thunk that
   is in the stream or just pass it on as a whole to a helper function.
   - Most of your returns that need to produce a stream will look like:
   `St (fun () -> ....)` with possibly a lot of work happening in those dots.
   - When trying to write a function that turns one type of stream into another, you
   may find it convenient to write a helper function that takes as input the pair
   (v, st) of next input value and next input stream and returns the pair (v', st')
   of next output value and next output stream. You can then often use that function
   as suitable in the dots on the previous tip.
   - Some of these are not easy. Do not be discouraged if you don't see how to do it
   right away.
*)
(* IMPORTANT NOTE:
   In all stream problems you need to make sure that stream values are not evaluated
   too soon, before they are needed. For instance in the map function that takes a
   'a stream and produces a 'b stream you need to make sure that the first element of
   the 'a stream is not calculated until the first element of the 'b stream has to be
   calculated, and so on. There is a test provided for the map case to ensure that does
   not happen in that case.

   As another example, if the function in `from_f` simply raises an exception when
   called, then creating the stream should NOT raise an exception, but trying to get
   the first value of the stream via say `take1` should.
*)

(* Stream generators. These functions create streams.
   Before you implement these functions, you may want to implement the function `take`
   provided later. It can be helpful in testing your solutions.
*)
(*
   Write a function `const` that takes as input a value of some type `'a` and returns
   a stream of type `'a stream` that keeps producing that value over and over.
   It should have type `'a -> 'a stream`.
*)


(*
   Write a function `alt` that takes as input two values of some type `'a` and returns
   a stream of type `'a stream` that keeps alternating between those two values.
   It should have type `'a -> 'a -> 'a stream`.
*)



(*
   Write a function `seq` that takes as input a start integer `a` and a step `step` and
   returns a stream that produces the numbers a, a + step, a + 2*step, ..., i.e. moving
   up by step each time.
   It should have type `int -> int -> int stream`
*)


(*
   Write a function `from_f` that takes as input a function `int -> 'a` and returns
   an `'a stream` that produces in turn the values f 1, f 2, f 3 and so on.
   It should have type `(int -> 'a) -> 'a stream`.
*)

(*
   Write a function `from_list` that takes as input an `'a list` and returns a stream
   that produces the elements in the list one at a time, then starts all over. For
   instance `from_list [1;2;3]` would produce 1,2,3,1,2,3,1,2,3,...
   If `from_list` is given an empty list, then your resulting stream may run forever
   in search of the (nonexistent) next value, and that is OK.
   It should have type `'a list -> 'a stream`.
*)


(* Stream users. These functions take as input a stream, and either produce some value
   or a new stream.
*)
(*
   Write a function `take` that takes as input a number `n` and a stream `st` and
   returns a list of the first n elements of the stream (and the empty list if n<=0).
   It should have type `int -> 'a stream -> 'a list`.
*)


(*
   Write a function `drop` that takes as input a number `n` and a stream `st` and
   returns a new stream where the first n elements of the original stream are dropped.
   So for instance when n<=0 the original stream would be returned.
   It should have type `int -> 'a stream -> 'a stream`.
*)


(*
   Write a function `prepend` that takes as input a `'a list` and a `'a stream` and
   returns an `'a stream` that will first go through the list and then continue with
   the provided stream.
   It should have type: `'a list -> 'a stream -> 'a stream`.
*)


(*
   Write a function `map` that takes as input a function `'a -> 'b` and a `'a stream`,
   and produces a `'b stream` whose values are the results of applying the function
   to the corresponding values of the 'a stream. For example if st is the stream
   1, 2, 3, ... and the function is the squaring function, the resulting stream would
   be 1, 4, 9, ...
   It should have type `('a -> 'b) -> 'a stream -> 'b stream`.
*)


(*
   Write a function `pair_up` that takes as input a `'a stream` and returns a
   `('a * 'a) stream` whose values are consecutive pairs of values from the original
   stream. For example if the original stream is 1, 2, 3, 4, ..., then the new stream
   would have values (1, 2), (3, 4), (5, 6), ...
   It should have type `'a stream -> ('a * 'a) stream`.
*)


(*
   Write a function `zip2` that takes as input a `'a stream` and a `'b stream` and
   returns a `('a * 'b) stream` by pairing together the corresponding values.
   It should have type `'a stream -> 'b stream -> ('a * 'b) stream`.
*)


(*
   Write a function `accum` that takes as input a function `'b -> 'a -> 'b`, an initial
   value of type `'b` and an `'a` stream, and produces a `'b stream` that at each step
   accumulates (folds) the values up to that point. For instance if the initial
   steam is 1, 2, 3, 4, 5, ... and the function is addition with an initial value of 5,
   then the resulting stream would be 5, 6, 8, 11, 15, 20, ...
   It should have type `('b -> 'a -> 'b) -> 'b -> 'a stream -> 'b stream`.
*)


(*
   Write a function `filter` that takes as input a predicate function `'a -> bool` and
   a `'a stream`, and returns a `'a stream` of those values that are true. For instance
   if the predicate is testing whether a number is odd, and the initial stream is 1, 2,
   3, ... then the resulting stream would be 1, 3, 5, ...
   It is entirely possible that such a stream might run forever in search of the next
   value, if for example the predicate returns always false.
   It should have type `('a -> bool) -> 'a stream -> 'a stream`.
*)


(*
   Write a function `collect` that takes as input an integer `n > 0` and a `'a stream`
   and returns a `'a list stream` where each value is formed out of taking the next `n`
   values from the original stream. For example if the original stream is 1, 2, 3, ...
   then `collect 3 st` is the stream [1;2;3], [4;5;6], [7;8;9], ...
   It should have type `int -> 'a stream -> 'a list stream`.
*)


(*
   Write a function `flatten` that takes as input a `'a list stream` and "flattens" it
   out, resulting in the stream that contains all the elements in the order they appear.
   For instance if the initial stream is [1;3], [], [4;5], then the new stream would
   start with 1,3,4,5. Note that empty lists should be skipped. It is possible that a
   stream like that might run forever in search of the "next value", if for example all
   the lists are empty.
   It should have type: `'a list stream -> 'a stream`,
*)


(*
   Write a function `list_combos` that takes as input a `'a stream` st1 and a `'b stream`,
   st2 and produces a `('a * 'b) list stream` st as follows: The n-th value of the result
   stream st will consist a list of all pairs `(x, y)` where `x` is the i-th element of the
   st1 and `y` is the j-th element of st2, and so that i + j = n + 1. So if st1 consists of
   the elements a1,a2,a3,... and st2 consists of b1,b2,b3,... then the result stream will
   have as its first element the 1-element list [(a1, b1)], as its second element the two-
   element list [(a2, b1); (a1, b2)], as its third element the three-element list
   [(a3, b1); (a2, b2); (a1, b3)] and so on. You will probably find it convenient to use a
   function that builds the "next state of the stream" by taking as inputs the lists
   [a_n; a_n-1; ...; a2; a1] and [b_n; b_n-1; ...; b2; b1] of the "elements seen so far"
   and also the current states of the provided streams (which when called would give the
   next entries a_n+1 and b_n+1). It then expands these lists by providing the next entries,
   uses List.rev and List.combine to create the list to be returned, and creates the appropriate
   call to itself to use as the next stream.
   This one is a bit harder. Make sure to check:
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
   for the documentation on the methods List.rev and List.combine.
   Reference solution is 7 lines.
   It should have type: 'a stream -> 'b stream -> ('a * 'b) list stream
*)



(*
   Write a function `list_combos_flat` that takes the same inputs as `list_combos` but
   instead returns the individual pairs as results, rather than lists. It should be simply a
   combination of `list_combos` together with the earlier method `flatten`. A very short
   solution.
   It should have type: 'a stream -> 'b stream -> ('a * 'b) stream
*)


