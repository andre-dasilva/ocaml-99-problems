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

let rec length_of_a_list (list : string list) =
  match list with
  | [] -> 0
  | _ :: tail -> length_of_a_list tail + 1
;;

let rec reverse_a_list (list : string list) =
  match list with
  | [] -> []
  | head :: tail -> reverse_a_list tail @ [ head ]
;;

let is_palindrome (list : string list) = reverse_a_list list = list

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten (list : 'a node list) : 'a list =
  let flatten_node node =
    match node with
    | One single -> [ single ]
    | Many multiple -> flatten multiple
  in
  match list with
  | [] -> []
  | head :: tail -> flatten_node head @ flatten tail
;;

let compress (list : string list) = list
