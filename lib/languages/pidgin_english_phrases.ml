open Phrases_common
include English_phrases

let this = "dis"
let the = "de"
let a = "one"
let each = "evry"
let another = "anodda"
let chosen = "pick"
let personnel = "Person"
let personnel_possessive = "De Person ein"
let procedure = "move"
let event = "Happenin"
let entity = "Ting"
let entities = "Tings"
let sector = "place"
let abyss = "de Abyss"
let in_ = "inna"
let then_ = "den"
let and_ = "an"
let when_ = "wen"
let that = "dat"
let add = { infinitive = "put"; imperative = "put"; third_person = "puts" }

let remove =
  { infinitive = "take"; imperative = "take"; third_person = "takes" }

let move =
  { infinitive = "shift"; imperative = "shift"; third_person = "shifts" }

let discard =
  { infinitive = "toss"; imperative = "toss"; third_person = "tosses" }

let deploy =
  { infinitive = "drop"; imperative = "drop"; third_person = "drops" }

let prevent =
  { infinitive = "block"; imperative = "block"; third_person = "blocks" }

let choose =
  { infinitive = "pick"; imperative = "pick"; third_person = "picks" }

let reset =
  { infinitive = "reset"; imperative = "reset"; third_person = "resets" }

let breach_markers_to_start =
  "go back to how many breach marker e get first time"

let you_may = "you can"
let another_target_player = "anodda target player"
let a_target_player = "one target player"
let each_player = "every player"
let their_hand = "dem"
let no_effect = "nuttin happen"
let if_you_cant_do_nothing = "if you cyaan, nuttin happen"
let when_deployed = "wen dis person get drop"
let when_cc_reduced_to_zero = "wen De Person ein CC drop to 0"
let at_start_of_end_phase = "at de start of de End Phase"
let this_round = "dis round"
let not_from_card_effect = "(not from one card effect)"
let not_from_entity_effect = "(not from one Ting ein effect)"
let chosen_by_active_player = "pick by de active player"
let chosen_by_starting_player = "pick by de starting player"
let chosen_by_controller = "pick by de controller"
let label_resolve_immediately = "Do dis now:"
let none_placeholder = "(nuttin)"
let have = "get"
let an_entity_possessive = "one Ting ein"
let a_card_effect_would_reduce = "one card effect go reduce"
let is_deployed = "get drop"
let is_breached = "get breach"
let is_not_breached = "no get breach"
let cannot_be_contained = "cyaan be contained"
let not_ = "no"
let other_than = "oda dan"
let with_ = "wid"
let effect_resolves = "effect go off"
let one_to_another noun = Printf.sprintf "from one %s to anodda" noun
let one_of_to_the_other noun = Printf.sprintf "from one of %s to de odda" noun
let has_min_cc ~min_cc = Printf.sprintf "got at least %d %s" min_cc cc

let filter_in_sector sec =
  Printf.sprintf "%s %s %s" in_ sector (sector_name sec)

let filter_and left right = Printf.sprintf "(%s AN %s)" left right
let in_same_sector_as_this_personnel = "inna de same place as dis Person"
let other_personnel_description = "anodda"
let other_personnel_in_sector = "Person inna your place"
let filter_in_play = "inna play"
let controlled_by_active_player = "you de control"
let controlled_by_chooser = "dem control"
let bound_that = Printf.sprintf "%s %s" that personnel

let bound_each_personnel_in_this_sector =
  Printf.sprintf "%s %s %s %s %s %s" each personnel in_ this
    personnel_possessive sector

let that_entity = Printf.sprintf "%s %s" that entity
let that_sector = Printf.sprintf "%s %s" that sector
let chooses_one = "picks one"
let either = "either"

let division_name = function
  | Core.Ada -> "Ada"
  | Core.Haskell -> "Haskell"
  | Core.OCaml -> "OCaml"
  | Core.Rust -> "Rust"
  | Core.Institute -> "de Institute"

let sector_name = function
  | Core.Alpha -> "Alpha"
  | Core.Beta -> "Beta"
  | Core.Lambda -> "Lambda"
  | Core.Gamma -> "Gamma"

let sector_state_name = function
  | Core.Secure -> "Secure"
  | Core.Breached -> "Breached"

let threat_level_name = function
  | Cards.Safe -> "Safe"
  | Cards.Euclid -> "Euclid"
  | Cards.Keter -> "Keter"
  | Cards.Titan -> "Titan"
