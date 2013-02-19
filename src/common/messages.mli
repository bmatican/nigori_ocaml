open Primitives
open Messages_t

val request_get : string
val request_get_indices : string
val request_get_revisions : string
val request_put : string
val request_delete : string
val request_update : string
val request_authenticate : string
val request_register : string
val request_unregister : string

val make_auth_request: DSA.key * DSA.key -> string -> authenticate_request
val to_sign_auth_request: authenticate_request -> string
val encode_auth_request: authenticate_request -> authenticate_request
val decode_auth_request: authenticate_request -> authenticate_request


val make_revision_value: string -> string -> revision_value
val encode_revision_value: revision_value -> revision_value
val decode_revision_value: revision_value -> revision_value


val make_get_request: DSA.key * DSA.key -> string -> string -> ?revision:string option -> unit -> get_request
val to_sign_get_request: get_request -> string
val encode_get_request: get_request -> get_request
val decode_get_request: get_request -> get_request


val make_get_response: revision_value list -> ?key:string option -> unit -> get_response
val encode_get_response: get_response -> get_response
val decode_get_response: get_response -> get_response


val make_get_indices_request: DSA.key * DSA.key -> string -> get_indices_request
val to_sign_get_indices_request: get_indices_request -> string
val encode_get_indices_request: get_indices_request -> get_indices_request
val decode_get_indices_request: get_indices_request -> get_indices_request


val make_get_indices_response: string list -> get_indices_response
val encode_get_indices_response: get_indices_response -> get_indices_response
val decode_get_indices_response: get_indices_response -> get_indices_response


val make_get_revisions_request: DSA.key * DSA.key -> string -> string -> get_revisions_request
val to_sign_get_revisions_request: get_revisions_request -> string
val encode_get_revisions_request: get_revisions_request -> get_revisions_request
val decode_get_revisions_request: get_revisions_request -> get_revisions_request


val make_get_revisions_response: string list -> ?key:string option -> unit -> get_revisions_response
val encode_get_revisions_response: get_revisions_response -> get_revisions_response
val decode_get_revisions_response: get_revisions_response -> get_revisions_response


val make_put_request: DSA.key * DSA.key -> string -> string -> string -> string -> put_request
val to_sign_put_request: put_request -> string
val encode_put_request: put_request -> put_request
val decode_put_request: put_request -> put_request


val make_delete_request: DSA.key * DSA.key -> string -> string -> ?revision:string option -> unit -> delete_request
val to_sign_delete_request: delete_request -> string
val encode_delete_request: delete_request -> delete_request
val decode_delete_request: delete_request -> delete_request


val make_register_request: DSA.key -> ?token:string -> unit -> register_request
val encode_register_request: register_request -> register_request
val decode_register_request: register_request -> register_request


val make_unregister_request: DSA.key * DSA.key -> string -> unregister_request
val encode_unregister_request: unregister_request -> unregister_request
val decode_unregister_request: unregister_request -> unregister_request
