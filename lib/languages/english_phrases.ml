open Phrases_common

let this = "this"
let the = "the"
let a = "a"
let all = "all"
let each = "each"
let another = "another"
let chosen = "chosen"
let personnel = "Personnel"
let personnel_possessive = "Personnel's"
let procedure = "Procedure"
let event = "Event"
let entity = "Entity"
let entities = "Entities"
let sector = "sector"
let card = "card"
let cards = "cards"
let player = "player"
let cc = "CC"
let breach_marker = "Breach Marker"
let breach_markers = "Breach Markers"
let abyss = "the Abyss"
let hand = "Hand"
let battlefield = "Battlefield"
let stack = "Stack"
let deck = "Deck"
let from = "from"
let to_ = "to"
let in_ = "in"
let on = "on"
let for_ = "for"
let up_to = "up to"
let then_ = "then"
let and_ = "and"
let or_ = "or"
let if_ = "if"
let when_ = "when"
let at = "at"
let that = "that"
let one_to_another noun = Printf.sprintf "from one %s to another" noun
let one_of_to_the_other noun = Printf.sprintf "from one of %s to the other" noun
let add = { infinitive = "add"; imperative = "add"; third_person = "adds" }

let remove =
  { infinitive = "remove"; imperative = "remove"; third_person = "removes" }

let move = { infinitive = "move"; imperative = "move"; third_person = "moves" }
let draw = { infinitive = "draw"; imperative = "draw"; third_person = "draws" }

let discard =
  { infinitive = "discard"; imperative = "discard"; third_person = "discards" }

let deploy =
  { infinitive = "deploy"; imperative = "deploy"; third_person = "deploys" }

let send = { infinitive = "send"; imperative = "send"; third_person = "sends" }
let flip = { infinitive = "flip"; imperative = "flip"; third_person = "flips" }

let prevent =
  { infinitive = "prevent"; imperative = "prevent"; third_person = "prevents" }

let choose =
  { infinitive = "choose"; imperative = "choose"; third_person = "chooses" }

let spend =
  { infinitive = "spend"; imperative = "spend"; third_person = "spends" }

let reset =
  { infinitive = "reset"; imperative = "reset"; third_person = "resets" }

let contain =
  { infinitive = "contain"; imperative = "contain"; third_person = "contains" }

let you = "you"
let you_may = "you may"
let another_target_player = "another target player"
let a_target_player = "a target player"
let each_player = "each player"
let their_hand = "their"
let no_effect = "no effect"
let if_you_cant_do_nothing = "if you can't, do nothing"
let loss = "loss"
let passive = "passive"
let burnout = "burnout"
let cc_cost_label = "-CC"
let at_end_of_round = "at end of round"
let at_end_of_phase = "at end of phase"
let when_deployed = "when this Personnel is deployed"
let when_cc_reduced_to_zero = "when this Personnel's CC is reduced to 0"
let once_per_round = "once per round"
let before_end_phase = "before End Phase"
let before_end_phase_step_2 = "before End Phase step 2"
let at_start_of_end_phase = "at the start of the End Phase"
let at_end_of_round_timing = "at end of round"
let this_round = "this round"
let future_rounds = "future rounds"
let not_from_spending_cc = "(not from spending CC)"
let not_from_card_effect = "(not from a card effect)"
let not_from_entity_effect = "(not from an Entity's effect)"
let not_from_breach = "(not from Breach)"
let chosen_by_active_player = "chosen by the active player"
let chosen_by_starting_player = "chosen by the starting player"
let chosen_by_controller = "chosen by the controller"
let label_deck = "Division:"
let label_id = "Id:"
let label_starting_cc = "Starting CC:"
let label_abilities = "Abilities:"
let label_effect = "Effect:"
let label_resolve_immediately = "Resolve immediately:"
let label_threat_level = "Threat Level:"
let label_breach_timer = "Breach Timer:"
let label_end_phase_effect = "Effect (End Phase, while uncontained):"
let label_breach_effect = "Effect (On Breach):"
let label_containment = "Containment Requirement:"
let none_placeholder = "(none)"
let bonus = "bonus"
let have = "have"
let total_cc = "total CC"
let is_ = "is"
let this_sector = "this sector"
let an_entity_possessive = "an Entity's"
let a_card_effect_would_reduce = "a card effect would reduce"
let is_deployed = "is deployed"
let is_breached = "is breached"
let is_not_breached = "is not breached"
let always_contained = "always contained"
let cannot_be_contained = "cannot be contained"
let not_ = "not"
let other_than = "other than"
let with_ = "with"
let effect_resolves = "effect resolves"
let breach_markers_to_start = "to its starting Breach Markers"
let or_more n = Printf.sprintf "%d+" n

let division_name = function
  | Core.Ada -> "Ada"
  | Core.Haskell -> "Haskell"
  | Core.OCaml -> "OCaml"
  | Core.Rust -> "Rust"
  | Core.Institute -> "Institute"

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

let capitalize = Phrases_common.capitalize
let has_min_cc ~min_cc = Printf.sprintf "has at least %d %s" min_cc cc

let filter_in_sector sec =
  Printf.sprintf "%s %s %s" in_ sector (sector_name sec)

let filter_and left right = Printf.sprintf "(%s AND %s)" left right
let in_same_sector_as_this_personnel = "in the same sector as this Personnel"
let other_personnel_description = "other"
let other_personnel_in_sector = "Personnel in your sector"
let filter_in_play = "in play"
let filter_uncontained = "uncontained"
let controlled_by_active_player = "you control"
let controlled_by_chooser = "they control"
let bound_that = Printf.sprintf "%s %s" that personnel

let bound_each_personnel_in_this_sector =
  Printf.sprintf "%s %s %s %s %s %s" each personnel in_ this
    personnel_possessive sector

let that_entity = Printf.sprintf "%s %s" that entity
let that_sector = Printf.sprintf "%s %s" that sector
let chooses_one = "chooses one"
let either = "either"
