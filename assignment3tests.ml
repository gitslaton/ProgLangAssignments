let t1a = result (Rock, Paper) = SndWin
let t1b = result (Scissors, Paper) = FstWin
let t1c = result (Rock, Rock) = Tie
let t1d = result (Paper, Paper) = Tie
let t1e = result (Scissors, Scissors) = Tie
let t1f = result (Paper, Rock) = FstWin
let t1g = result (Paper, Scissors) = SndWin
let t1h = result (Scissors, Rock) = SndWin
let t1i = result (Rock, Scissors) = FstWin

let t2a = is_tie (Rock, Paper) = false
let t2b = is_tie (Rock, Rock) = true
let t2c = is_tie (Paper, Scissors) = false 

let t3a = game_from_plays ([Rock; Paper; Rock], [Scissors; Rock; Rock]) =
                          [(Rock, Scissors); (Paper, Rock); (Rock, Rock)]
let t3b = game_from_plays ([Rock], [Rock]) = [(Rock, Rock)]
let t3c = game_from_plays ([], []) = []
let t3d = game_from_plays ([], [Scissors]) = []
let t3e = game_from_plays ([Paper; Scissors], []) = []
let t3f = game_from_plays ([Paper; Scissors], [Scissors; Rock; Scissors]) = 
                          [(Paper, Scissors); (Scissors, Rock)]

let t4a = valid_game [(Rock, Scissors)] = true
let t4b = valid_game [(Paper, Paper)] = false
let t4d = valid_game [(Rock, Paper); (Scissors, Scissors)] = false
let t4c = valid_game [(Scissors, Paper); (Paper, Rock)] = false
let t4e = valid_game [] = false 


let t5a = play_game [(Rock, Rock); (Scissors, Rock)] = SndWin
let t5b = play_game [(Paper, Paper)] = Tie
let t5c = play_game [(Scissors, Scissors); (Rock, Rock)] = Tie
let t5d = play_game [(Paper, Rock)] = FstWin
let t5g = play_game [(Paper, Scissors)] = SndWin
let t5e = play_game [] = Tie
let t5f = play_game [(Scissors, Scissors); (Rock, Rock); (Rock, Scissors)] = FstWin

let t6a = to_f (F 2.3) = 2.3
let t6b = to_f (F (-1.3)) = (-1.3)
let t6c = to_f (C 0.0) = 32.0
let t6d = to_f (C (-1.3)) = 29.66

let t7a = temp_compare (F 2.3, F 4.5) = -1
let t7b = temp_compare (C 0.0, F 32.0) = 0
let t7c = temp_compare (C 32.0, C 32.0) = 0
let t7d = temp_compare (F 0.0, F 0.0) = 0
let t7e = temp_compare (F 0.0, C (-20.0)) = 1
let t7f = temp_compare (C 20.0, F 19.0) = 1
let t7g = temp_compare (F 0.0, C 0.0) = -1


let t8a = string_of_temp (C 2.3) = "2.3C"
let t8b = string_of_temp (C 0.0) = "0.0C"
let t8c = string_of_temp (C (-1.03)) = "-1.03C"
let t8d = string_of_temp (F 32.0) = "32.0F"
let t8f = string_of_temp (F (-6.078)) = "-6.078F"

let t9a = max_temp [F 2.1; C 2.1] = C 2.1

let t10a = max_temp2 [F 2.1; C 2.1] = C 2.1
