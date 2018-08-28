let rec remove_none : 'a option list -> 'a list = function
  | Some x :: rest -> x :: remove_none rest
  | None :: rest -> remove_none rest
  | [] -> []
