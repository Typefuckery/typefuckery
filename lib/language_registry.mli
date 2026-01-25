type serializer = (module To_string.TEXT_SERIALIZER)

type error =
  [ `Language_already_registered of string | `Language_not_found of string ]

type t

val empty : t
val register_language : t -> name:string -> serializer -> (t, error) result
val find_language : t -> string -> (serializer, error) result
val list_languages : t -> string list

module Global : sig
  val set : t -> unit
  val clear : unit -> unit
  val register_language : name:string -> serializer -> (unit, error) result
  val find_language : string -> (serializer, error) result
  val list_languages : unit -> string list
end

val init : unit -> unit
