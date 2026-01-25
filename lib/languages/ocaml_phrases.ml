open Phrases_common
include English_phrases

let this = "self"
let the = "~"
let a = "Some"
let all = "List.iter"
let each = "List.iter"
let another = "other"
let chosen = "Option.get"
let personnel = "personnel"
let personnel_possessive = "personnel."
let procedure = "procedure"
let event = "event"
let entity = "entity"
let entities = "entity list"
let sector = "sector"
let card = "card"
let cards = "card list"
let player = "player"
let cc = "cc : int"
let breach_marker = "breach_marker"
let breach_markers = "breach_marker list"
let abyss = "Zones.abyss"
let hand = "Zones.hand"
let battlefield = "Zones.battlefield"
let stack = "Zones.stack"
let deck = "Zones.deck"
let from = "|>"
let to_ = "~to_:"
let in_ = "|> List.mem"
let on = "::"
let for_ = "for"
let up_to = "List.init"
let then_ = ";"
let and_ = "&&"
let or_ = "||"
let if_ = "if"
let when_ = "function"
let at = "~at:"
let that = "|> fun"
let add = { infinitive = "cons"; imperative = "::"; third_person = "cons" }

let remove =
  {
    infinitive = "List.filter ((<>)";
    imperative = "|> List.filter ((<>)";
    third_person = "List.filter ((<>)";
  }

let move =
  {
    infinitive = "transfer";
    imperative = "transfer ~from:src ~to_:dst";
    third_person = "transfer";
  }

let draw =
  {
    infinitive = "draw";
    imperative = "Queue.pop deck |> Option.some";
    third_person = "draw";
  }

let discard =
  {
    infinitive = "discard";
    imperative = "hand := List.filter ((<>)";
    third_person = "discard";
  }

let deploy =
  {
    infinitive = "deploy";
    imperative = "battlefield := p ::";
    third_person = "deploy";
  }

let send =
  {
    infinitive = "send_to_abyss";
    imperative = "Zones.abyss := card ::";
    third_person = "send_to_abyss";
  }

let flip =
  {
    infinitive = "flip";
    imperative = "card.flipped <- not card.flipped";
    third_person = "flip";
  }

let prevent =
  {
    infinitive = "Option.none";
    imperative = "None (* prevented *)";
    third_person = "Option.none";
  }

let choose =
  {
    infinitive = "select";
    imperative = "select ~from:targets |> Option.get";
    third_person = "select";
  }

let spend =
  {
    infinitive = "spend";
    imperative = "self.cc <- self.cc -";
    third_person = "spend";
  }

let reset =
  { infinitive = "reset"; imperative = "reset"; third_person = "reset" }

let breach_markers_to_start = "to_starting_breach_markers"
let you = "active_player"
let you_may = "Option.iter (fun () ->"
let another_target_player = "~other_player:player"
let a_target_player = "~target:player"
let each_player = "List.iter ~f:(fun player ->"
let their_hand = "player.hand"
let no_effect = "()"
let if_you_cant_do_nothing = "|> Option.value ~default:()"
let loss = "raise Game_loss"
let passive = "[@@passive]"
let burnout = "[@@burnout]"
let cc_cost_label = "(** Cost in CC *)"
let at_end_of_round = "let%trigger end_of_round () ="
let at_end_of_phase = "let%trigger end_of_phase () ="
let when_deployed = "let%trigger on_deploy () ="
let when_cc_reduced_to_zero = "let%trigger on_cc_zero () ="
let once_per_round = "[@@once_per Round]"
let before_end_phase = "let%trigger before_end_phase () ="
let before_end_phase_step_2 = "let%trigger before_end_phase_step_2 () ="
let at_start_of_end_phase = "let%trigger start_of_end_phase () ="
let at_end_of_round_timing = "let%trigger end_of_round () ="
let this_round = "State.current_round"
let future_rounds = "'a. 'a ->"
let not_from_spending_cc = "(* [@source <> Spend_cc] *)"
let not_from_card_effect = "(* [@source <> Card_effect] *)"
let not_from_entity_effect = "(* [@source <> Entity_effect] *)"
let not_from_breach = "(* [@source <> Breach] *)"
let chosen_by_active_player = "~chooser:active_player"
let chosen_by_starting_player = "~chooser:starting_player"
let chosen_by_controller = "~chooser:controller"
let label_deck = "(** division : Division.t *)"
let label_id = "(** id : Card_id.t *)"
let label_starting_cc = "(** starting_cc : int *)"
let label_abilities = "(** abilities : ability list *)"
let label_effect = "(** effect : Effect.t *)"
let label_resolve_immediately = "(** [@@immediate] *)"
let label_threat_level = "(** threat_level : Threat.t *)"
let label_breach_timer = "(** breach_timer : int *)"

let label_end_phase_effect =
  "(** end_phase_effect : Effect.t (* when not contained *) *)"

let label_breach_effect = "(** breach_effect : Effect.t (* on breach *) *)"
let label_containment = "(** containment : Containment.t *)"
let none_placeholder = "None"
let bonus = "~bonus"
let have = "Option.is_some"
let an_entity_possessive = "entity."
let a_card_effect_would_reduce = "match effect with Reduce _ -> true | _ ->"
let is_deployed = ".deployed"
let is_breached = ".breached"
let is_not_breached = "not .breached"
let always_contained = "[@@always_contained]"
let cannot_be_contained = "[@@never_contained]"
let not_ = "not"
let other_than = "<>"
let with_ = "~"
let effect_resolves = "|> Effect.resolve"

let one_to_another noun =
  Printf.sprintf "let %s_a, %s_b = %s_b, %s_a in" noun noun noun noun

let one_of_to_the_other noun =
  Printf.sprintf "%s_a := !%s_a @ !%s_b" noun noun noun

let or_more n = Printf.sprintf "(fun x -> x >= %d)" n

let has_min_cc ~min_cc =
  Printf.sprintf "|> List.filter (fun p -> p.cc >= %d)" min_cc

let filter_in_sector sec =
  Printf.sprintf "|> List.filter (fun p -> p.sector = %s)" (sector_name sec)

let filter_and left right = Printf.sprintf "%s %s" left right

let in_same_sector_as_this_personnel =
  "|> List.filter (fun p -> p.sector = self.sector)"

let other_personnel_description = "|> List.filter ((<>) self)"

let other_personnel_in_sector =
  "self.sector.personnel |> List.filter ((<>) self)"

let filter_in_play = "|> List.filter (fun c -> c.zone = Play)"
let filter_uncontained = "|> List.filter (fun e -> not e.contained)"

let controlled_by_active_player =
  "|> List.filter (fun p -> p.controller = active_player)"

let controlled_by_chooser = "|> List.filter (fun p -> p.controller = chooser)"
let bound_that = ": personnel -> unit"

let bound_each_personnel_in_this_sector =
  "self.sector.personnel |> List.iter (fun p ->"

let that_entity = "entity"
let that_sector = "entity.sector"
let chooses_one = "choose_one"
let either = "Either.fold"

let division_name = function
  | Core.Ada -> "Division.Ada"
  | Core.Haskell -> "Division.Haskell"
  | Core.OCaml -> "Division.OCaml"
  | Core.Rust -> "Division.Rust"
  | Core.Institute -> "Division.Institute"

let sector_name = function
  | Core.Alpha -> "Sector.Alpha"
  | Core.Beta -> "Sector.Beta"
  | Core.Lambda -> "Sector.Lambda"
  | Core.Gamma -> "Sector.Gamma"

let sector_state_name = function
  | Core.Secure -> "Sector_state.Secure"
  | Core.Breached -> "Sector_state.Breached"

let threat_level_name = function
  | Cards.Safe -> "Threat.Safe"
  | Cards.Euclid -> "Threat.Euclid"
  | Cards.Keter -> "Threat.Keter"
  | Cards.Titan -> "Threat.Titan"
