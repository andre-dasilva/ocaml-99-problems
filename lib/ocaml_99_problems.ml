let rec last (list: string list) =
  match list with
  | [] -> None
  | [res] -> Some(res)
  | _ :: tail -> last tail
