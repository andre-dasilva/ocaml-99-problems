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

let compress (list : string list) =
  let rec gather state l =
    match l with
    | head :: tail ->
      (match tail with
       | next :: _ -> if head = next then gather state tail else gather (head :: state) tail
       | _ -> head :: state)
    | _ -> state
  in
  let result = gather [] list in
  List.rev result
;;

let print_list list =
  List.iter print_endline list;
  print_endline ""
;;

let pack (list : string list) =
  let rec gather state inner l =
    match l with
    | head :: tail ->
      (match tail with
       | next :: _ ->
         if head = next
         then gather state (head :: inner) tail
         else gather ((head :: inner) :: state) [] tail
       | _ -> (head :: inner) :: state)
    | _ -> state
  in
  let result = gather [] [] list in
  List.rev result
;;

let run_length_encoding (list : string list) =
  let rec gather state counter current_letter l =
    match l with
    | head :: tail ->
      if head = current_letter
      then gather state (counter + 1) head tail
      else gather ((counter, current_letter) :: state) 1 head tail
    | _ -> (counter, current_letter) :: state
  in
  match list with
  | [] -> []
  | head :: tail -> List.rev (gather [] 1 head tail)
;;

type 'a rle =
  | Single of 'a
  | Multiple of int * 'a

let modified_run_length_encoding (list : string list) : string rle list =
  let rec gather state counter current_letter l =
    let item = if counter == 1 then Single current_letter else Multiple (counter, current_letter) in
    match l with
    | head :: tail ->
      if head = current_letter
      then gather state (counter + 1) head tail
      else gather (item :: state) 1 head tail
    | _ -> item :: state
  in
  match list with
  | [] -> []
  | head :: tail -> List.rev (gather [] 1 head tail)
;;

let rec decode_run_length_encoded_list (list : string rle list) : string list =
  let rec produce_char_list state counter letter =
    if counter = 0 then state else produce_char_list (letter :: state) (counter - 1) letter
  in
  let rle_to_char_list rle : string list =
    match rle with
    | Single a -> [ a ]
    | Multiple (n, a) -> produce_char_list [] n a
  in
  match list with
  | head :: tail -> rle_to_char_list head @ decode_run_length_encoded_list tail
  | _ -> []
;;

let rec duplicates (list : string list) =
  match list with
  | [] -> []
  | head :: tail -> head :: head :: duplicates tail
;;
