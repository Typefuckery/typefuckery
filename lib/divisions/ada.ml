open Core

include Division_helper.Make_core_division (struct
  let division = Ada_div
  let name = "Ada Division"
  let lore = None

  type div = ada

  let cards : div Cards.core_card list = []
end)
