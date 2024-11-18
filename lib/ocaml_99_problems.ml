let rec last (list : string list) =
  match list with
  | [] -> None
  | [ res ] -> Some res
  | _ :: tail -> last tail
;;

let rec last_two (list : string list) =
  match list with
  | [] | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: tail -> last_two tail
;;

let rec nth_element_of_list (list : string list) (index : int) =
  match list with
  | [] -> None
  | head :: tail -> if index > 0 then nth_element_of_list tail (index - 1) else Some head
;;
