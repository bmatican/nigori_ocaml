type allowed = [
  `get
  | `get_indices
  | `get_revisions
  | `put
  | `delete
  | `update
  | `authenticate
  | `register
  | `unregister
]

let from = function
  | "/get" -> Some `get
  | "/get-indices" -> Some `get_indices
  | "/get-revisions" -> Some `get_revisions
  | "/put" -> Some `put
  | "/delete" -> Some `delete
  | "/update" -> Some `update
  | "/authenticate" -> Some `authenticate
  | "/register" -> Some `register
  | "/unregister" -> Some `unregister
  | _ -> None
