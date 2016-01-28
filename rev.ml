let rec rev lst =
  let rec acc_lst lst1 lst2 = 
  match lst1 with
  | [] -> []
  | hd::tl -> 