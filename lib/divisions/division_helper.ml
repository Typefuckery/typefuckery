module Make_core_division (M : sig
  type div

  val division : div Core.division_tag
  val name : Registry.set_name
  val lore : Lore.t option
  val cards : div Cards.core_card list
end) =
struct
  let id : Registry.set_id = Core.set_id_of_division M.division
  let name : Registry.set_name = M.name
  let lore : Lore.t option = M.lore
  let cards : M.div Cards.core_card list = M.cards

  let register () =
    Registry.Global.register_core_division ~id ~name
      ~cards:(Cards.pack_core_cards cards)
end
