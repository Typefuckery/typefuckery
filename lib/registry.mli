type set_id = string
type set_name = string
type card_type = Personnel | Procedure | Event | Entity

type registered_card = {
  card_id : Core.Card_id.t;
  name : string;
  card_type : card_type;
  set_id : set_id;
  set_name : set_name;
  rendered_text : string;
  card_data : Cards.core_card option;
}

type set_metadata = { id : set_id; name : set_name }
type set = { metadata : set_metadata; cards : registered_card list }

type error =
  [ `Set_id_already_registered of set_id
  | `Set_name_already_registered of set_name * set_id
  | `Duplicate_card_id_within_set of set_id * Core.Card_id.t * int
  | `Card_id_already_registered of Core.Card_id.t * set_id * set_id ]

type t

val empty : t
val card_id : ('div, 'fx, 'trig) Cards.card -> Core.Card_id.t
val card_name : ('div, 'fx, 'trig) Cards.card -> string
val card_type_of : ('div, 'fx, 'trig) Cards.card -> card_type

val register_division :
  t ->
  id:set_id ->
  name:set_name ->
  cards:('div, 'fx, 'trig) Cards.card list ->
  renderer:(('div, 'fx, 'trig) Cards.card -> string) ->
  (t, error) result

val register_core_division :
  t ->
  id:set_id ->
  name:set_name ->
  cards:Cards.core_card list ->
  (t, error) result

val list_sets : t -> set_metadata list
val find_set_by_id : t -> set_id -> set option
val find_set_by_name : t -> set_name -> set option
val list_cards : t -> registered_card list
val find_card : t -> Core.Card_id.t -> registered_card option

val render_card :
  registered_card -> (module To_string.TEXT_SERIALIZER) -> string option

module Global : sig
  val set : t -> unit
  val clear : unit -> unit

  val register_division :
    id:set_id ->
    name:set_name ->
    cards:('div, 'fx, 'trig) Cards.card list ->
    renderer:(('div, 'fx, 'trig) Cards.card -> string) ->
    (unit, error) result

  val register_core_division :
    id:set_id ->
    name:set_name ->
    cards:Cards.core_card list ->
    (unit, error) result

  val list_sets : unit -> set_metadata list
  val find_set_by_id : set_id -> set option
  val find_set_by_name : set_name -> set option
  val list_cards : unit -> registered_card list
  val find_card : Core.Card_id.t -> registered_card option
end
