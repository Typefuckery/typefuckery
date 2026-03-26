open Core
open Card_dsl
open Cards

let scrupulous_shrimp =
  entity "scrupulous_shrimp" "The Scrupulous Shrimp" rust
    ~lore:(Lore.doc_uri "")
    ~flavor_text:
      "Weaponized ethics. The borrow checker becomes a moral imperative, and \
       every allocation a sin."
    ~threat:euclid ~timer:Timer.four
    ~on_end_phase:
      (each_player_chooses
         ~option_a:(discard ~player:each_player one)
         ~option_b:(everyone -@ two))
    ~on_breach:(seq [ everyone -@ five; draw ~player:each_player five ])
    ~contained:Conditions.(personnel_count Gamma 2)
    ()

include Division_helper.Make_core_division (struct
  type div = rust

  let division = Rust_div
  let name = "Rust Division"
  let lore = None
  let cards : div Cards.core_card list = [ Entity scrupulous_shrimp ]
end)
