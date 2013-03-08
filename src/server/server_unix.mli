module type S = sig
  val main : unit -> unit
end

module Make (DB: Database.DB) : S
