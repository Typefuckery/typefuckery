open Phrases_common
include English_phrases

let this = "&self"
let the = "&mut"
let a = "impl"
let all = ".iter()"
let each = ".for_each(|x|"
let another = "other"
let chosen = "unwrap()"
let personnel = "Personnel"
let personnel_possessive = "Personnel's"
let procedure = "Procedure"
let event = "Event"
let entity = "Entity"
let entities = "Vec<Entity>"
let sector = "Sector"
let card = "Box<dyn Card>"
let cards = "Vec<Box<dyn Card>>"
let player = "Player"
let cc = "cc: u32"
let breach_marker = "BreachMarker"
let breach_markers = "Vec<BreachMarker>"
let abyss = "Abyss::default()"
let hand = "hand"
let battlefield = "battlefield"
let stack = "VecDeque"
let deck = "deck"
let from = ".take()"
let to_ = ".into()"
let in_ = ".contains("
let on = ".push("
let for_ = "for"
let up_to = "..="
let then_ = ".and_then(|_|"
let and_ = "&&"
let or_ = "||"
let if_ = "if let Some(_) ="
let when_ = "match"
let at = "@"
let that = "where"

let add =
  { infinitive = "push"; imperative = ".push()"; third_person = ".push()" }

let remove =
  {
    infinitive = "remove";
    imperative = ".remove()";
    third_person = ".remove()";
  }

let move =
  {
    infinitive = "mem::take";
    imperative = "mem::take(&mut";
    third_person = "mem::take(&mut";
  }

let draw =
  {
    infinitive = "pop";
    imperative = ".pop().unwrap()";
    third_person = ".pop().unwrap()";
  }

let discard =
  { infinitive = "drop"; imperative = "drop("; third_person = "drop(" }

let deploy =
  { infinitive = "spawn"; imperative = ".spawn()"; third_person = ".spawn()" }

let send =
  { infinitive = "send"; imperative = "tx.send("; third_person = "tx.send(" }

let flip =
  {
    infinitive = "toggle";
    imperative = ".toggle()";
    third_person = ".toggle()";
  }

let prevent =
  {
    infinitive = "guard";
    imperative = ".lock().unwrap()";
    third_person = ".lock().unwrap()";
  }

let choose =
  {
    infinitive = "select";
    imperative = ".expect(\"";
    third_person = ".expect(\"";
  }

let spend =
  {
    infinitive = "consume";
    imperative = ".saturating_sub(";
    third_person = ".saturating_sub(";
  }

let reset =
  { infinitive = "reset"; imperative = "reset"; third_person = "resets" }

let breach_markers_to_start = "to_starting_breach_markers"
let you = "self"
let you_may = "if let Ok(_) ="
let another_target_player = "&mut other_player"
let a_target_player = "&mut target_player"
let each_player = "for player in players.iter_mut()"
let their_hand = "player.hand"
let no_effect = "// no-op"
let if_you_cant_do_nothing = ".unwrap_or_default()"
let loss = "Err(Loss)"
let passive = "#[derive(Passive)]"
let burnout = "mem::drop(self)"
let cc_cost_label = "/// Cost in CC"
let at_end_of_round = "impl Drop for Round { fn drop(&mut self)"
let at_end_of_phase = "impl Drop for Phase { fn drop(&mut self)"
let when_deployed = "#[on_spawn]"
let when_cc_reduced_to_zero = "if self.cc == 0"
let once_per_round = "#[once(per = Round)]"
let before_end_phase = "#[before(EndPhase)]"
let before_end_phase_step_2 = "#[before(EndPhase, step = 2)]"
let at_start_of_end_phase = "#[on(EndPhase::Start)]"
let at_end_of_round_timing = "#[on(Round::End)]"
let this_round = "self.round"
let future_rounds = "'static"
let not_from_spending_cc = "// source != Source::SpendCC"
let not_from_card_effect = "// source != Source::CardEffect"
let not_from_entity_effect = "// source != Source::EntityEffect"
let not_from_breach = "// source != Source::Breach"
let chosen_by_active_player = ".choose_by(&active_player)"
let chosen_by_starting_player = ".choose_by(&starting_player)"
let chosen_by_controller = ".choose_by(&self.controller)"
let label_deck = "/// Division:"
let label_id = "/// Id:"
let label_starting_cc = "/// StartingCC:"
let label_abilities = "/// Abilities:"
let label_effect = "/// Effect:"
let label_resolve_immediately = "/// #[immediate]"
let label_threat_level = "/// ThreatLevel:"
let label_breach_timer = "/// BreachTimer:"
let label_end_phase_effect = "/// Effect (EndPhase, while !contained):"
let label_breach_effect = "/// Effect (OnBreach):"
let label_containment = "/// ContainmentRequirement:"
let none_placeholder = "None"
let bonus = "bonus"
let have = ".is_some()"
let an_entity_possessive = "entity.borrow()"
let a_card_effect_would_reduce = "matches!(effect, Effect::Reduce { .. })"
let is_deployed = ".is_deployed()"
let is_breached = ".is_breached()"
let is_not_breached = "!.is_breached()"
let always_contained = "#[always_contained]"
let cannot_be_contained = "#[never_contained]"
let not_ = "!"
let other_than = "!="
let with_ = ".with("
let effect_resolves = ".resolve()"

let one_to_another noun =
  Printf.sprintf "std::mem::swap(&mut %s_a, &mut %s_b)" noun noun

let one_of_to_the_other noun =
  Printf.sprintf "%s_a.extend(%s_b.drain(..))" noun noun

let or_more n = Printf.sprintf "%d.." n
let has_min_cc ~min_cc = Printf.sprintf ".filter(|p| p.cc >= %d)" min_cc

let filter_in_sector sec =
  Printf.sprintf ".filter(|p| p.sector == Sector::%s)" (sector_name sec)

let filter_and left right = Printf.sprintf "%s%s" left right
let in_same_sector_as_this_personnel = ".filter(|p| p.sector == self.sector)"
let other_personnel_description = ".filter(|p| !std::ptr::eq(p, self))"

let other_personnel_in_sector =
  "self.sector.personnel().filter(|p| !std::ptr::eq(p, self))"

let filter_in_play = ".filter(|c| c.zone == Zone::Play)"
let filter_uncontained = ".filter(|e| !e.is_contained())"
let controlled_by_active_player = ".filter(|p| p.controller == active_player)"
let controlled_by_chooser = ".filter(|p| p.controller == chooser)"
let bound_that = Printf.sprintf "%s %s" "impl Fn(&" "Personnel)"

let bound_each_personnel_in_this_sector =
  Printf.sprintf "self.sector.personnel().for_each(|%s|"
    (String.lowercase_ascii personnel)

let that_entity = "entity"
let that_sector = "entity.sector()"
let chooses_one = ".choose_one()"
let either = "Either::"

let division_name = function
  | Core.Ada -> "Division::Ada"
  | Core.Haskell -> "Division::Haskell"
  | Core.OCaml -> "Division::OCaml"
  | Core.Rust -> "Division::Rust"
  | Core.Institute -> "Division::Institute"

let sector_name = function
  | Core.Alpha -> "Alpha"
  | Core.Beta -> "Beta"
  | Core.Lambda -> "Lambda"
  | Core.Gamma -> "Gamma"

let sector_state_name = function
  | Core.Secure -> "SectorState::Secure"
  | Core.Breached -> "SectorState::Breached"

let threat_level_name = function
  | Cards.Safe -> "ThreatLevel::Safe"
  | Cards.Euclid -> "ThreatLevel::Euclid"
  | Cards.Keter -> "ThreatLevel::Keter"
  | Cards.Titan -> "ThreatLevel::Titan"
