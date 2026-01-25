module Make_core_division (M : sig
  val id : Registry.set_id
  val name : Registry.set_name
  val lore : Lore.t option
  val cards : Cards.core_card list
end) =
struct
  let id : Registry.set_id = M.id
  let name : Registry.set_name = M.name
  let lore : Lore.t option = M.lore
  let cards : Cards.core_card list = M.cards
  let register () = Registry.Global.register_core_division ~id ~name ~cards
end
