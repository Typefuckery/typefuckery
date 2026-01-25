open Card_dsl
open Cards

let scrupulous_shrimp =
  entity "rust:scrupulous_shrimp" "The Scrupulous Shrimp" Rust
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
  let id = "rust"
  let name = "Rust Division"
  let lore = None
  let cards = [ Entity scrupulous_shrimp ]
end)
