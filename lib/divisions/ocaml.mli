val id : Registry.set_id
val name : Registry.set_name
val cards : Cards.core_card list
val register : unit -> (unit, Registry.error) result
