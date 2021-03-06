(* These tests assume that you have implemented the function `take` as suggested *)
let t1a = take 4 (const 3) = [3; 3; 3; 3]
let t1b = take (-1) (const 0) = []
let t1c = take 0 (const (-1)) = []

let t2a = take 5 (alt 3 4) = [3; 4; 3; 4; 3]
let t2b = take 2 (alt 0 1) = [0; 1]
let t2c = take 1 (alt 2 2) = [2]
let t2d = take 3 (alt 5 5) = [5; 5; 5]

let t3a = take 3 (seq 2 6) = [2; 8; 14]
let t3b = take 2 (seq 1 1) = [1;2]
let t3c = take 4 (seq 0 0) = [0; 0; 0; 0]

let t4a = take 5 (from_f (fun x -> x * x)) = [1; 4; 9; 16; 25]
(* The next test ensures that the function is not called until the corresponding
   value is actually needed. *)
let t4b = try (ignore (from_f (fun _ -> raise (Failure ""))); true) with
          | _ -> false
let t4c = take 1 (from_f (fun x -> x)) = [1]
let t4d = take 3 (from_f (fun x -> x + 1)) = [2; 3; 4]

let t5a = take 5 (from_list [3; 5; 6]) = [3; 5; 6; 3; 5]
let t5b = take 10 (from_list [1;2;3]) = [1;2;3;1;2;3;1;2;3;1]
let t5c = take 4 (from_list [0]) = [0;0;0;0]

let t6a = take 3 (drop 3 (seq 2 6)) = [20; 26; 32]
let t6b = take 2 (drop 2 (from_f (fun x -> x))) = [3;4]

let t7a = take 6 (prepend [1; 2] (const 3)) = [1; 2; 3; 3; 3; 3]

let t8a = take 6 (map (fun x -> x * x) (seq 1 1)) = [1; 4; 9; 16; 25; 36]
(* The next test ensures that the function is not called until the corresponding
   value is actually needed. *)
let t8b = try (ignore (map (fun _ -> raise (Failure "")) (seq 1 1)); true) with
          | _ -> false

let t9a = take 3 (pair_up (seq 1 1)) = [(1, 2); (3, 4); (5, 6)]
let t9b = take 2 (pair_up (const 3)) = [(3, 3); (3, 3)]
let t9c = take 1 (pair_up (prepend [0;0] (const 1))) = [(0, 0)]

let t10a = take 3 (zip2 (seq 1 2) (seq 2 3)) = [(1, 2); (3, 5); (5, 8)]

let t11a = take 4 (accum (+) 0 (seq 1 1)) = [0; 1; 3; 6]

let t12a = take 4 (filter (fun x -> x mod 2 = 0) (seq 1 1)) = [2; 4; 6; 8]

let t13a = take 3 (collect 3 (seq 1 1)) = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]]

let t14a = take 5 (flatten (collect 3 (seq 1 1))) = [1; 2; 3; 4; 5]

let t15a = take 4 (list_combos (seq 1 1) (seq 1 1)) =
                  [[(1, 1)]; [(2, 1); (1, 2)]; [(3, 1); (2, 2); (1, 3)];
                   [(4, 1); (3, 2); (2, 3); (1, 4)]]

let t16a = take 10 (list_combos_flat (seq 1 1) (seq 1 1)) =
                  [(1, 1); (2, 1); (1, 2); (3, 1); (2, 2); (1, 3);
                   (4, 1); (3, 2); (2, 3); (1, 4)]
(*
   The following test is an integration test putting together some of the things
   you've build so far. It builds a stream that produces (albeit not terribly
   efficiently) all pythagorean triples:
   https://en.wikipedia.org/wiki/Pythagorean_triple
   It does the following:
   - Starts with the sequence of natural numbers 1, 2, 3, 4, using `seq`
   - Forms all possible pairs of pairs ((a, b), c) from that sequence using
   list_combos_flat
   - Filters that list using the condition a^2 + b^2 = c^2 and the `filter` function

   Then we read out the first 5 answers.
*)
let t17a = let nats = seq 1 1 in
           let pairs = list_combos_flat (list_combos_flat nats nats) nats in
           let triples = filter (fun ((a, b), c) -> a * a + b * b = c * c) pairs
           in take 8 triples = [((4, 3), 5); ((3, 4), 5); ((8, 6), 10); ((6, 8), 10);
                                ((12, 5), 13); ((5, 12), 13); ((12, 9), 15); ((9, 12), 15)]

(*
   This integration tests shows that whenever you add all consecutive integers starting
   from 1 then the result is all the perfect squares:
*)
let t17b = let odds = seq 1 2 in
           let sums = accum (+) 0 odds in
           let squares = map (fun x -> x * x) (seq 0 1)
           in take 8 (zip2 sums squares) =
                  [(0, 0); (1, 1); (4, 4); (9, 9);
                   (16, 16); (25, 25); (36, 36); (49, 49)]
