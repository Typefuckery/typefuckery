open Card_dsl
open Cards
open Engine

let calm_weave_protocol =
  let e = any_entity () in
  procedure "ocaml:calm_weave_protocol" "Calm Weave Protocol" OCaml
    (let_entity e (fun e _ p -> seq [ contain e; p +@ one ]))

let semantic_drift =
  event "ocaml:semantic-drift" "Semantic Drift" OCaml ~lore:(Lore.doc_uri "")
    ~flavor_text:"Language slips; meaning leaks; the system blinks."
    (each_player_personnel () --> any_sector ~filter:other_than_source_sector ())

include Division_helper.Make_core_division (struct
  let id = "ocaml"
  let name = "OCaml Division"
  let lore = None
  let cards = [ Procedure calm_weave_protocol; Event semantic_drift ]
end)
