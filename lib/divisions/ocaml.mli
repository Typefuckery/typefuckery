val id : Registry.set_id
val name : Registry.set_name
val cards : Core.ocaml_div Cards.core_card list
val register : unit -> (unit, Registry.error) result
