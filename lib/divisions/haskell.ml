open Cards
open Card_dsl

let space_leak =
  entity "haskell:space_leak" "Space Leak of 2008" Haskell ~threat:Euclid
    ~timer:ten
    ~on_end_phase:(discard ~player:each_player one)
    ~on_breach:(discard_hand ~player:each_player ())
    ~contained:Condition.never ()

include Division_helper.Make_core_division (struct
  let id = "haskell"
  let name = "Haskell Division"
  let lore = None
  let cards = [ Entity space_leak ]
end)
