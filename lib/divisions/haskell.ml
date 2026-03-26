open Core
open Cards
open Card_dsl

let space_leak =
  entity "space_leak" "Space Leak of 2008" haskell ~threat:Euclid
    ~timer:ten
    ~on_end_phase:(discard ~player:each_player one)
    ~on_breach:(discard_hand ~player:each_player ())
    ~contained:Condition.never ()

include Division_helper.Make_core_division (struct
  type div = haskell

  let division = Haskell_div
  let name = "Haskell Division"
  let lore = None
  let cards : div Cards.core_card list = [ Entity space_leak ]
end)
