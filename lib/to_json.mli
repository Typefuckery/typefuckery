type json =
  | Null
  | Bool of bool
  | Int of int
  | String of string
  | Array of json list
  | Object of (string * json) list

module type S = sig
  val json_to_string : json -> string
  val card_to_json : 'div Cards.core_card -> json
  val personnel_to_json : 'div Cards.core_personnel -> json
  val procedure_to_json : 'div Cards.core_procedure -> json
  val event_to_json : 'div Cards.core_event -> json
  val entity_to_json : 'div Cards.core_entity -> json
  val any_core_card_to_json : Cards.any_core_card -> json
end

module Make (_ : To_string.TEXT_SERIALIZER) : S
module Core : S
include module type of Core
