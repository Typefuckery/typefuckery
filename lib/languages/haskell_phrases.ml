open Phrases_common
include English_phrases

let this = "self"
let the = "the"
let a = "pure"
let all = "traverse_"
let each = "forM_"
let another = "other"
let chosen = "fromJust"
let personnel = "Personnel"
let personnel_possessive = "(^. personnel)"
let procedure = "Procedure"
let event = "Event"
let entity = "Entity"
let entities = "[Entity]"
let sector = "Sector"
let card = "Card"
let cards = "[Card]"
let player = "Player"
let cc = "cc :: Natural"
let breach_marker = "BreachMarker"
let breach_markers = "[BreachMarker]"
let abyss = "abyss"
let hand = "_hand"
let battlefield = "_battlefield"
let stack = "_stack"
let deck = "_deck"
let from = "<$"
let to_ = "$>"
let in_ = "`elem`"
let on = ":"
let for_ = "for"
let up_to = "[1.."
let then_ = ">>="
let and_ = "<&&>"
let or_ = "<||>"
let if_ = "when"
let when_ = "\\case"
let at = "@"
let that = "where"

let add =
  {
    infinitive = "cons";
    imperative = "modify . cons $";
    third_person = "(`cons`)";
  }

let remove =
  {
    infinitive = "delete";
    imperative = "modify . delete $";
    third_person = "(`delete`)";
  }

let move =
  {
    infinitive = "transfer";
    imperative = "_from . at k <<.=";
    third_person = "transfer";
  }

let draw =
  {
    infinitive = "draw";
    imperative = "zoom _deck $ state splitAt";
    third_person = "draw";
  }

let discard =
  {
    infinitive = "discard";
    imperative = "_hand %= filter (/=";
    third_person = "discard";
  }

let deploy =
  {
    infinitive = "deploy";
    imperative = "_battlefield %= (|>";
    third_person = "deploy";
  }

let send =
  {
    infinitive = "sendTo";
    imperative = "_abyss <>= pure $";
    third_person = "(`sendTo` abyss)";
  }

let flip =
  {
    infinitive = "over flipped not";
    imperative = "flipped %= not $";
    third_person = "over flipped not";
  }

let prevent =
  {
    infinitive = "guard . not";
    imperative = "guard . not $";
    third_person = "guard . not";
  }

let choose =
  {
    infinitive = "selectM";
    imperative = "selectM >>=";
    third_person = "selectM";
  }

let spend =
  { infinitive = "spend"; imperative = "_cc -= "; third_person = "spend" }

let reset =
  { infinitive = "reset"; imperative = "reset"; third_person = "resets" }

let breach_markers_to_start = "to_starting_breach_markers"
let you = "activePlayer"
let you_may = "whenM mayActivate $"
let another_target_player = "otherPlayer :: Player"
let a_target_player = "targetPlayer :: Player"
let each_player = "forM_ players $ \\player ->"
let their_hand = "playerHand"
let no_effect = "pure () {- noop -}"
let if_you_cant_do_nothing = "`catchError` const (pure ())"
let loss = "throwError GameLoss"
let passive = "deriving Passive"
let burnout = "self .= mempty"
let cc_cost_label = "-- | Cost in CC"
let at_end_of_round = "onEndOfRound :: GameM ()"
let at_end_of_phase = "onEndOfPhase :: GameM ()"
let when_deployed = "onDeploy :: GameM ()"
let when_cc_reduced_to_zero = "onCCZero :: GameM ()"
let once_per_round = "guard =<< uses _usedThisRound not"
let before_end_phase = "beforeEndPhase :: GameM ()"
let before_end_phase_step_2 = "beforeEndPhaseStep2 :: GameM ()"
let at_start_of_end_phase = "atStartOfEndPhase :: GameM ()"
let at_end_of_round_timing = "atEndOfRound :: GameM ()"
let this_round = "currentRound"
let future_rounds = "forever"
let not_from_spending_cc = "{-# RULE source /= SpendCC #-}"
let not_from_card_effect = "{-# RULE source /= CardEffect #-}"
let not_from_entity_effect = "{-# RULE source /= EntityEffect #-}"
let not_from_breach = "{-# RULE source /= Breach #-}"
let chosen_by_active_player = "& chooser .~ activePlayer"
let chosen_by_starting_player = "& chooser .~ startingPlayer"
let chosen_by_controller = "& chooser .~ controller"
let label_deck = "-- | division ::"
let label_id = "-- | cardId ::"
let label_starting_cc = "-- | startingCC ::"
let label_abilities = "-- | abilities ::"
let label_effect = "-- | effect ::"
let label_resolve_immediately = "-- | {-# IMMEDIATE #-}"
let label_threat_level = "-- | threatLevel ::"
let label_breach_timer = "-- | breachTimer ::"
let label_end_phase_effect = "-- | endPhaseEffect :: (uncontained ==>) =>"
let label_breach_effect = "-- | breachEffect :: (onBreach ==>) =>"
let label_containment = "-- | containment ::"
let none_placeholder = "Nothing"
let bonus = "bonus"
let have = "has"
let an_entity_possessive = "(entity ^.)"
let a_card_effect_would_reduce = "whenM (asks $ is _ReduceEffect)"
let is_deployed = "(^. deployed)"
let is_breached = "(^. breached)"
let is_not_breached = "(^. breached . to not)"
let always_contained = "instance AlwaysContained"
let cannot_be_contained = "instance NeverContained"
let not_ = "not"
let other_than = "/="
let with_ = "&"
let effect_resolves = ">>= resolve"
let one_to_another noun = Printf.sprintf "(%s_a, %s_b) & swapped" noun noun
let one_of_to_the_other noun = Printf.sprintf "%s_a <>= %s_b" noun noun
let or_more n = Printf.sprintf "(>= %d)" n
let has_min_cc ~min_cc = Printf.sprintf "filtered ((>= %d) . view _cc)" min_cc

let filter_in_sector sec =
  Printf.sprintf "filtered ((== %s) . view _sector)" (sector_name sec)

let filter_and left right = Printf.sprintf "%s . %s" left right

let in_same_sector_as_this_personnel =
  "filtered ((== self ^. _sector) . view _sector)"

let other_personnel_description = "filtered (/= self)"

let other_personnel_in_sector =
  "(self ^. _sector . _personnel) ^.. folded . filtered (/= self)"

let filter_in_play = "filtered ((== Play) . view _zone)"
let filter_uncontained = "filtered (not . view _contained)"

let controlled_by_active_player =
  "filtered ((== activePlayer) . view _controller)"

let controlled_by_chooser = "filtered ((== chooser) . view _controller)"
let bound_that = ":: Personnel -> GameM ()"

let bound_each_personnel_in_this_sector =
  "forM_ (self ^. _sector . _personnel) $ \\p ->"

let that_entity = "entity"
let that_sector = "entity ^. _sector"
let chooses_one = "<|>"
let either = "either"

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
