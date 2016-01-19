(* Add your own tests. Make sure to pay attention to edge cases. *)
let t1a = fixLastTwo (5, 1, 2) = (5, 1, 2)
let t1b = fixLastTwo (5, 2, 1) = (5, 1, 2)
let t1c = fixLastTwo (0, 0, 0) = (0, 0, 0)
let t1d = fixLastTwo (-4, -3, -2) = (-4, -3, -2)
let t1e = fixLastTwo (9, 1, -2) = (9, -2, 1) 


let t2a = order (2, 5, 3) = (2, 3, 5)
let t2b = order (5, 3, 2) = (2, 3, 5)
let t2c = order (0, 0, 0) = (0, 0 ,0)
let t2d = order (-3, -5, -2) = (-5, -3, -2)


let t3a = distance (6, 3) = 3
let t3b = distance (0, 0) = 0
let t3c = distance (-10, 10) = 20
let t3d = distance (-5, -2) = 3
let t3e = distance (20, 34) = 14


let t4a = greeting (23, "Pete") = "Greetings Pete, you are 23 years old!"
let t4b = greeting (-1, "1231") = "Greetings 1231, you are -1 years old!"
let t4c = greeting (0, "") = "Greetings , you are 0 years old!"
let t4d = greeting (090, "") = "Greetings , you are 90 years old!"

let t5a = greeting2 (0, "Jackson") = "Greetings Jackson, you are not born yet!"
let t5b = greeting2 (-3424, "") = "Greetings , you are not born yet!"
let t5c = greeting2 (-1, "Thrall") = "Greetings Thrall, you are not born yet!"

let t5d = greeting2 (1, "Tauren") = "Greetings Tauren, you are a youngster!"
let t5e = greeting2 (20, "Goblin") = "Greetings Goblin, you are a youngster!"
let t5f = greeting2 (10, "Mr. T") = "Greetings Mr. T, you are a youngster!"

let t5g = greeting2 (21, "Sylvanas") = "Greetings Sylvanas, you are young at heart!"
let t5g = greeting2 (10000, "Illidan") = "Greetings Illidan, you are young at heart!"


let t6a = tooShort (4, "tree") = false
let t6b = tooShort (0, "") = false
let t6c = tooShort (-1, "m") = false
let t6d = tooShort (1, "") = true
let t6e = tooShort (9, "hello") = true


let t7a = totalLength ("you", "me") = 5
let t7b = totalLength ("", "") = 0
let t7c = totalLength ("a", "b") = 2
let t7d = totalLength ("", "b") = 1


let t8a = orderedByLength ("long", "one", "at") = false
let t8b = orderedByLength ("", "", "") = true
let t8c = orderedByLength ("one", "two", "thre") = true
let t8d = orderedByLength ("is", "this", "fine?") = true
let t8e = orderedByLength ("med", "long", "no") = false
let t8f = orderedByLength ("longer", "short", "longest") = false


let t9a = prodInRange (3, 5) = true
let t9b = prodInRange (0, 0) = false
let t9c = prodInRange (5, 2) = false
let t9d = prodInRange (20, 1) = false
let t9e = prodInRange (3, 7) = false
let t9f = prodInRange (4, 4) = true
