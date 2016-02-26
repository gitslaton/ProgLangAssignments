let t1a = range1 5 = [1;2;3;4;5]
let t1b = range1 0 = []
let t1c = range1 1 = [1]

let t2a = tabulate (fun x -> x + 1) 2 = [2; 3]

let test_pc = [
[H;D;D];
[D;H;D];
[D;D;H]]

let test_pc2 = [
[D;D;H];
[D;D;H];
[D;H;D]]

let not_val_test = [
[H;D;D;D];
[D;H;D];
[D;D;H]
]


let t3a = valid_pic test_pc
let t3b = not (valid_pic not_val_test)

let t4a = string_of_pxl D = "."
let t4b = string_of_pxl H = "#"

let t5a = string_of_row [H;D;D] = "#..\n"
let t5b = string_of_row [H;H;H] = "###\n"
let t5c = string_of_row [D;D;D] = "...\n"

let t6a = string_of_pic test_pc = "#..\n.#.\n..#\n"
let t6b = string_of_pic not_val_test = "#...\n.#.\n..#\n"

let t7a = flip_vertical test_pc = [[D;D;H];[D;H;D];[H;D;D]]

let t8a = flip_horizontal test_pc2 = [[H;D;D];[H;D;D];[D;H;D]] 

let t9a = flip_both test_pc2 = 
  [[D;H;D];
   [H;D;D];
   [H;D;D]] 

let t10a = mirror_vertical test_pc2 = test_pc2 @ (flip_vertical test_pc2)
let t10b = mirror_horizontal test_pc2 = [
[D;D;H;H;D;D];
[D;D;H;H;D;D];
[D;H;D;D;H;D]]

let t10c = mirror_both test_pc = [
[H;D;D;D;D;H];
[D;H;D;D;H;D];
[D;D;H;H;D;D];
[D;D;H;H;D;D];
[D;H;D;D;H;D];
[H;D;D;D;D;H]]


let f i j = if i = j then H else D
let t11a = pixelate f 3 3 = test_pc

let t12a = stack_vertical test_pc test_pc2 = test_pc @ test_pc2
let t12b = (try (stack_vertical not_val_test test_pc) with
            IncompatibleDims -> []) = [] (* this won't let me return a bool for some reason so I'm testing against an empty list*)

let t13a = stack_horizontal test_pc test_pc2 = [
[H;D;D;D;D;H];
[D;H;D;D;D;H];
[D;D;H;D;H;D]]

let not_val_test2 = [
[H;D;D;D];
[D;H;D];
[D;D;H];
[H;H;H]]

let t13b = (try stack_horizontal test_pc not_val_test2 with
            IncompatibleDims -> []) = []

let t14a = invert test_pc = [
[D;H;H];
[H;D;H];
[H;H;D]]