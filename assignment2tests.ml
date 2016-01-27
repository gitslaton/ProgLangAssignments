let t1a = getnth (3, ["hi"; "there"; "you"]) = "you"
let t1b = try (getnth (3, ["hi"; "there"]); false)  with
            | Failure "getnth" -> true
            | _ -> false
let t1c = try (getnth (0, []); false)  with
            | Failure "getnth" -> true
            | _ -> false
let t1d = getnth (1, ["yo"]) = "yo"
let t1e = getnth (1, ["this"; "one"]) = "this"
let t1f = getnth (2, ["hi"; "there"; "you"]) = "there"


let t2a = lookup ("you", []) = None
let t2b = lookup ("you", [("him", 2); ("you", 3)]) = Some 3
let t2c = lookup ("", []) = None
let t2d = lookup ("k", [("k", -1)]) = Some (-1)
let t2e = lookup ("k", [("notk", 3)]) = None
  
let t3a = inPairs [1; 2; 3; 4; 5] = [(1, 2); (3, 4)]
let t3b = inPairs [] = []
let t3c = inPairs [1] = []
let t3d = inPairs [3;2] = [(3,2)]
let t3e = inPairs [1;2;3] = [(1,2)]


let t4a = flatten [[1; 2; 3]; []; [4; 5]; [6]] = [1; 2; 3; 4; 5; 6]
let t4b = flatten [[]] = []
let t4c = flatten [[1]] = [1] 
let t4d = flatten [[]; [1]] = [1]
let t4e = flatten [[0]; []] = [0]

let t5a = remove (3, [3; 4; 3; 1]) = [4; 1]
let t5b = remove (0, []) = []
let t5c = remove (1, [1]) = []
let t5d = remove (2, [2; 1]) = [1]
let t5e = remove (0, [1; 1; 1]) = [1; 1; 1]


let t6a = removeDups [4; 1; 2; 1; 4; 5; 20] = [4; 1; 2; 5; 20]
let t6b = removeDups [1] = [1]
let t6c = removeDups [] = []
let t6d = removeDups [1; 0] = [1; 0]
let t6e = removeDups [2; 2; 2; 2; 2] = [2]
let t6f = removeDups [3; 5; 3] = [3; 5]
let t6g = removeDups [4; 0; 0] = [4; 0]


let t7a = collateSome [Some 1; None; Some 2; Some 1; None; Some 3] = [1; 2; 1; 3]
let t7b = collateSome [Some 1] = [1]
let t7c = collateSome [None] = []
let t7d = collateSome [None; Some 0] = [0]
let t7e = collateSome [Some 3; None] = [3]
let t7f = collateSome [Some 5; Some 5] = [5; 5]

let t8a = unzip2 [(1, 2); (3, 4); (5, 6)] = ([1; 3; 5], [2; 4; 6])
let t8b = unzip2 [(1, 2)] = ([1], [2])
let t8d = unzip2 [(5, 7); (1, 2)] = ([5; 1], [7; 2])
let t8c = unzip2 [] = ([], [])

let t9a = makeChange (20, [8; 3; 2]) = Some [8; 8; 2; 2]
let t9b = makeChange (20, [8; 3]) = Some [8; 3; 3; 3; 3]
let t9c = makeChange (20, [13; 11]) = None
