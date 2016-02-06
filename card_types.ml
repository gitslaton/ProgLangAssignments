type suit = Clubs | Spades | Hearts | Diamonds;;
type value = Jack | Queen | King | Ace | IntN of int;;
type card = suit * value;;
type hand = card list;;

let is_valid (crd : card) = 
  match crd with
  | (_, v) -> match v with
              | IntN i -> i >= 2 || i <= 10
              | x -> true;;

let value (crd: card) = 
  match crd with
  | (_, v) -> match v with
              | IntN i -> i
              | Ace -> 11
              | King -> 10
              | Queen -> 10
              | Jack -> 10;;

let rec hand_value (hnd : hand) = 
  match hnd with
  | [] -> 0
  | hd::tl -> value(hd) + hand_value(tl);;


  let card_equality (c1, c2 : card * card) =
  

  let valid_hand (hnd : hand) =
    let rec d_test (n, val_hand, hand_lst) = 
      if n > 5
      then false
      else match hand_lst with
           | hd::tl