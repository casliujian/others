let apply f op =
  match op with
  | None -> None
  | Some a -> Some (f a) 