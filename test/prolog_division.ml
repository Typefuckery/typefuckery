module Core = Typefuckery.Core
module Cards = Typefuckery.Cards
module Abilities = Typefuckery.Abilities
module Engine = Typefuckery.Engine
module T = Typefuckery.Targets
module Condition = Typefuckery.Condition
module TS = Typefuckery.To_string
module Int = Typefuckery.Int
open Util

module Prolog_types = struct
  type fx =
    | Unify of { variable : string; value : string }
    | Query of { predicate : string; arity : int }
    | Assert_fact of { fact : string }
    | Retract_fact of { fact : string }
    | Backtrack
    | Cut

  type trig =
    | On_unification_failure
    | On_goal_success of { predicate : string }
    | On_backtrack

  type div = Prolog
end

module type PROLOG_PHRASES = sig
  include TS.PHRASES

  val unify : variable:string -> value:string -> string
  val query : predicate:string -> arity:int -> string
  val assert_fact : fact:string -> string
  val retract_fact : fact:string -> string
  val backtrack : string
  val cut : string
  val on_unification_failure : string
  val on_goal_success : predicate:string -> string
  val on_backtrack : string
  val prolog_division_name : string
end

module Prolog_english_phrases : PROLOG_PHRASES = struct
  include Typefuckery.English_phrases

  let unify ~variable ~value = Printf.sprintf "Unify %s with %s" variable value

  let query ~predicate ~arity =
    Printf.sprintf "Query %s/%d against the knowledge base" predicate arity

  let assert_fact ~fact = Printf.sprintf "Assert fact: %s" fact
  let retract_fact ~fact = Printf.sprintf "Retract fact: %s" fact
  let backtrack = "Backtrack to the last choice point"
  let cut = "Cut: commit to current solution path"
  let on_unification_failure = "When a unification fails"
  let on_goal_success ~predicate = Printf.sprintf "When %s succeeds" predicate
  let on_backtrack = "When execution backtracks"
  let prolog_division_name = "Prolog"
end

module Prolog_speak_phrases : PROLOG_PHRASES = struct
  let this = "self"
  let the = ""
  let a = ""
  let all = "forall"
  let each = "foreach"
  let another = "other"
  let chosen = "selected"
  let personnel = "agent"
  let personnel_possessive = "agent's"
  let procedure = "rule"
  let event = "trigger"
  let entity = "anomaly"
  let entities = "anomalies"
  let sector = "zone"
  let card = "clause"
  let cards = "clauses"
  let player = "user"
  let cc = "energy"
  let breach_marker = "corruption"
  let breach_markers = "corruptions"
  let abyss = "void"
  let hand = "buffer"
  let battlefield = "heap"
  let stack = "call_stack"
  let deck = "knowledge_base"
  let from = "<-"
  let to_ = "->"
  let in_ = "@"
  let on = "@"
  let for_ = ":"
  let up_to = "max"
  let then_ = "then"
  let and_ = ", "
  let or_ = "; "
  let if_ = ":-"
  let when_ = ":-"
  let at = "@"
  let that = "_"
  let one_to_another what = Printf.sprintf "transfer(%s, A, B)" what
  let one_of_to_the_other what = Printf.sprintf "swap(%s)" what

  let add =
    { TS.infinitive = "add"; imperative = "assert"; third_person = "asserts" }

  let remove =
    {
      TS.infinitive = "remove";
      imperative = "retract";
      third_person = "retracts";
    }

  let move =
    {
      TS.infinitive = "move";
      imperative = "relocate";
      third_person = "relocates";
    }

  let draw =
    {
      TS.infinitive = "draw";
      imperative = "consult";
      third_person = "consults";
    }

  let discard =
    {
      TS.infinitive = "discard";
      imperative = "abolish";
      third_person = "abolishes";
    }

  let deploy =
    {
      TS.infinitive = "deploy";
      imperative = "instantiate";
      third_person = "instantiates";
    }

  let send =
    {
      TS.infinitive = "send";
      imperative = "transmit";
      third_person = "transmits";
    }

  let flip =
    { TS.infinitive = "flip"; imperative = "toggle"; third_person = "toggles" }

  let prevent =
    { TS.infinitive = "prevent"; imperative = "guard"; third_person = "guards" }

  let choose =
    {
      TS.infinitive = "choose";
      imperative = "select";
      third_person = "selects";
    }

  let spend =
    {
      TS.infinitive = "spend";
      imperative = "consume";
      third_person = "consumes";
    }

  let reset =
    { TS.infinitive = "reset"; imperative = "reset"; third_person = "resets" }

  let contain =
    {
      TS.infinitive = "contain";
      imperative = "contain";
      third_person = "contains";
    }

  let breach_markers_to_start = "to_starting_breach_markers"
  let you = "?user"
  let you_may = "?user :- optional"
  let another_target_player = "other(?user)"
  let a_target_player = "any(?user)"
  let each_player = "forall(?user)"
  let their_hand = "?user.buffer"
  let no_effect = "true"
  let if_you_cant_do_nothing = "; true"
  let loss = "decrement"
  let passive = "fact"
  let burnout = "on_halt"
  let cc_cost_label = "cost"
  let at_end_of_round = "@ end_round"
  let at_end_of_phase = "@ end_phase"
  let when_deployed = "on_init"
  let when_cc_reduced_to_zero = "energy(self, 0) :-"
  let once_per_round = "once/round"
  let before_end_phase = "before(end_phase)"
  let before_end_phase_step_2 = "before(end_phase_2)"
  let at_start_of_end_phase = "@ start(end_phase)"
  let at_end_of_round_timing = "@ end_round"
  let this_round = "this_round"
  let future_rounds = "future_rounds"
  let not_from_spending_cc = "\\+ spend_source"
  let not_from_card_effect = "\\+ clause_source"
  let not_from_entity_effect = "\\+ anomaly_source"
  let not_from_breach = "\\+ corruption_source"
  let chosen_by_active_player = "select(?active)"
  let chosen_by_starting_player = "select(?first)"
  let chosen_by_controller = "select(?controller)"
  let label_deck = "KB"
  let label_id = "id"
  let label_starting_cc = "init_energy"
  let label_abilities = "rules"
  let label_effect = "body"
  let label_resolve_immediately = "call_immediate"
  let label_threat_level = "threat"
  let label_breach_timer = "timer"
  let label_end_phase_effect = "end_phase_rule"
  let label_breach_effect = "breach_rule"
  let label_containment = "containment_check"
  let none_placeholder = "nil"
  let bonus = "bonus"
  let have = "has"
  let total_cc = "total_energy"
  let is_ = "="
  let this_sector = "this_zone"
  let an_entity_possessive = "anomaly's"
  let a_card_effect_would_reduce = "clause_reduces"
  let is_deployed = "deployed(X)"
  let is_breached = "corrupted(X)"
  let is_not_breached = "\\+ corrupted(X)"
  let always_contained = "contained :- true"
  let cannot_be_contained = "contained :- fail"
  let not_ = "\\+"
  let with_ = ":"
  let effect_resolves = "effect_resolves"
  let or_more n = Printf.sprintf ">= %d" n

  let division_name = function
    | Core.Ada -> "ADA"
    | Core.Haskell -> "HASKELL"
    | Core.OCaml -> "OCAML"
    | Core.Rust -> "RUST"
    | Core.Institute -> "INSTITUTE"

  let sector_name = function
    | Core.Alpha -> "alpha"
    | Core.Beta -> "beta"
    | Core.Gamma -> "gamma"
    | Core.Lambda -> "lambda"

  let sector_state_name = function
    | Core.Secure -> "secure"
    | Core.Breached -> "corrupted"

  let threat_level_name = function
    | Cards.Safe -> "safe"
    | Cards.Euclid -> "euclid"
    | Cards.Keter -> "keter"
    | Cards.Titan -> "titan"

  let capitalize s = s
  let has_min_cc ~min_cc = Printf.sprintf "energy >= %d" min_cc
  let filter_in_sector sec = Printf.sprintf "@ zone(%s)" (sector_name sec)
  let filter_and left right = Printf.sprintf "(%s, %s)" left right
  let in_same_sector_as_this_personnel = "same_zone(self, X)"
  let other_personnel_description = "other"
  let other_personnel_in_sector = "agent @ zone(self)"
  let filter_in_play = "deployed(X)"
  let filter_uncontained = "\\+ contained(X)"
  let controlled_by_active_player = "controls(active_player, X)"
  let controlled_by_chooser = "controls(chooser, X)"
  let other_than = "\\="
  let bound_that = "_ personnel"

  let bound_each_personnel_in_this_sector =
    Printf.sprintf "%s %s %s self's zone" each personnel in_

  let that_entity = Printf.sprintf "%s %s" that entity
  let that_sector = Printf.sprintf "%s zone" that
  let chooses_one = "select_one"
  let either = ";"
  let unify ~variable ~value = Printf.sprintf "%s = %s" variable value
  let query ~predicate ~arity = Printf.sprintf "?- %s/%d" predicate arity
  let assert_fact ~fact = Printf.sprintf "assert(%s)" fact
  let retract_fact ~fact = Printf.sprintf "retract(%s)" fact
  let backtrack = "fail, redo"
  let cut = "!"
  let on_unification_failure = "\\+ unify(_, _) :-"
  let on_goal_success ~predicate = Printf.sprintf "%s :- true," predicate
  let on_backtrack = "redo :-"
  let prolog_division_name = "PROLOG"
end

module Make_prolog_extension (P : PROLOG_PHRASES) :
  Typefuckery.Division_intf.EXTENSION
    with type fx = Prolog_types.fx
     and type trig = Prolog_types.trig
     and type div = Prolog_types.div = struct
  include Prolog_types

  let fx_to_string = function
    | Unify { variable; value } -> P.unify ~variable ~value
    | Query { predicate; arity } -> P.query ~predicate ~arity
    | Assert_fact { fact } -> P.assert_fact ~fact
    | Retract_fact { fact } -> P.retract_fact ~fact
    | Backtrack -> P.backtrack
    | Cut -> P.cut

  let trig_to_string = function
    | On_unification_failure -> P.on_unification_failure
    | On_goal_success { predicate } -> P.on_goal_success ~predicate
    | On_backtrack -> P.on_backtrack

  let div_to_string = function Prolog -> P.prolog_division_name
end

module Prolog_extension = Make_prolog_extension (Prolog_english_phrases)
module E = Engine.Make (Prolog_extension)
module Prolog_renderer = TS.MakeExt (Prolog_extension) (Prolog_english_phrases)
module Prolog_speak_extension = Make_prolog_extension (Prolog_speak_phrases)

module Prolog_speak_renderer =
  TS.MakeExt (Prolog_speak_extension) (Prolog_speak_phrases)

type json = Util.Json.t

open Util.Json

let json_to_string = Util.Json.to_string

module Prolog_json = struct
  let limit_to_json = function
    | None -> Null
    | Some Abilities.Once_per_round -> String "once_per_round"

  let passive_ability_to_json
      (a : Prolog_extension.fx Abilities.passive_ability) : json =
    let effect_text =
      match a.condition with
      | None -> Prolog_renderer.effect_to_string a.card_effect
      | Some cond ->
          Printf.sprintf "If %s: %s"
            (Prolog_renderer.condition_to_string cond)
            (Prolog_renderer.effect_to_string a.card_effect)
    in
    Object
      [
        ("type", String "passive");
        ("limit", Null);
        ("cost_cc", Null);
        ("optional", Null);
        ("trigger_text", Null);
        ("effect_text", String effect_text);
      ]

  let activated_ability_to_json
      (a : Prolog_extension.fx Abilities.activated_ability) : json =
    let cc_cost = Int.to_int a.cc_cost in
    let effect_text =
      match a.condition with
      | None -> Prolog_renderer.effect_to_string a.card_effect
      | Some cond ->
          Printf.sprintf "If %s: %s"
            (Prolog_renderer.condition_to_string cond)
            (Prolog_renderer.effect_to_string a.card_effect)
    in
    Object
      [
        ("type", String "activated");
        ("limit", Null);
        ("cost_cc", Int cc_cost);
        ("optional", Null);
        ("trigger_text", Null);
        ("effect_text", String effect_text);
      ]

  let triggered_ability_to_json
      (a :
        (Prolog_extension.fx, Prolog_extension.trig) Abilities.triggered_ability)
      : json =
    let trigger_text = Prolog_renderer.trigger_to_string a.trigger in
    let effect_text =
      match a.condition with
      | None -> Prolog_renderer.effect_to_string a.card_effect
      | Some cond ->
          Printf.sprintf "if %s: %s"
            (Prolog_renderer.condition_to_string cond)
            (Prolog_renderer.effect_to_string a.card_effect)
    in
    Object
      [
        ("type", String "triggered");
        ("limit", limit_to_json a.limit);
        ("cost_cc", Null);
        ( "optional",
          Bool
            (match a.optionality with
            | Abilities.Optional -> true
            | Abilities.Mandatory -> false) );
        ("trigger_text", String trigger_text);
        ("effect_text", String effect_text);
      ]

  let burnout_ability_to_json
      (a : Prolog_extension.fx Abilities.burnout_ability) : json =
    Object
      [
        ("type", String "burnout");
        ("limit", Null);
        ("cost_cc", Null);
        ("optional", Null);
        ("trigger_text", String "When this Personnel's CC is reduced to 0");
        ("effect_text", String (Prolog_renderer.effect_to_string a.card_effect));
      ]

  let ability_to_json :
      (Prolog_extension.fx, Prolog_extension.trig) Abilities.ability -> json =
    function
    | Abilities.Passive a -> passive_ability_to_json a
    | Abilities.Activated a -> activated_ability_to_json a
    | Abilities.Triggered a -> triggered_ability_to_json a
    | Abilities.Burnout a -> burnout_ability_to_json a

  let personnel_to_json
      (p :
        ( Prolog_extension.div,
          Prolog_extension.fx,
          Prolog_extension.trig )
        Cards.personnel) : json =
    Object
      [
        ("card_type", String "personnel");
        ("id", String (Core.Card_id.to_string p.id));
        ("name", String p.name);
        ( "division",
          String
            (String.lowercase_ascii
               (Prolog_renderer.division_to_string p.division)) );
        ("starting_cc", Int (Int.to_int p.starting_cc));
        ("abilities", Array (List.map ability_to_json p.abilities));
      ]
end

module Prolog_speak_json = struct
  let limit_to_json = function
    | None -> Null
    | Some Abilities.Once_per_round -> String "once/round"

  let passive_ability_to_json
      (a : Prolog_speak_extension.fx Abilities.passive_ability) : json =
    let effect_text =
      match a.condition with
      | None -> Prolog_speak_renderer.effect_to_string a.card_effect
      | Some cond ->
          Printf.sprintf ":- %s: %s"
            (Prolog_speak_renderer.condition_to_string cond)
            (Prolog_speak_renderer.effect_to_string a.card_effect)
    in
    Object
      [
        ("type", String "fact");
        ("limit", Null);
        ("cost_energy", Null);
        ("optional", Null);
        ("trigger_text", Null);
        ("effect_text", String effect_text);
      ]

  let activated_ability_to_json
      (a : Prolog_speak_extension.fx Abilities.activated_ability) : json =
    let energy_cost = Int.to_int a.cc_cost in
    let effect_text =
      match a.condition with
      | None -> Prolog_speak_renderer.effect_to_string a.card_effect
      | Some cond ->
          Printf.sprintf ":- %s: %s"
            (Prolog_speak_renderer.condition_to_string cond)
            (Prolog_speak_renderer.effect_to_string a.card_effect)
    in
    Object
      [
        ("type", String "rule");
        ("limit", Null);
        ("cost_energy", Int energy_cost);
        ("optional", Null);
        ("trigger_text", Null);
        ("effect_text", String effect_text);
      ]

  let triggered_ability_to_json
      (a :
        ( Prolog_speak_extension.fx,
          Prolog_speak_extension.trig )
        Abilities.triggered_ability) : json =
    let trigger_text = Prolog_speak_renderer.trigger_to_string a.trigger in
    let effect_text =
      match a.condition with
      | None -> Prolog_speak_renderer.effect_to_string a.card_effect
      | Some cond ->
          Printf.sprintf ":- %s: %s"
            (Prolog_speak_renderer.condition_to_string cond)
            (Prolog_speak_renderer.effect_to_string a.card_effect)
    in
    Object
      [
        ("type", String "triggered_rule");
        ("limit", limit_to_json a.limit);
        ("cost_energy", Null);
        ( "optional",
          Bool
            (match a.optionality with
            | Abilities.Optional -> true
            | Abilities.Mandatory -> false) );
        ("trigger_text", String trigger_text);
        ("effect_text", String effect_text);
      ]

  let burnout_ability_to_json
      (a : Prolog_speak_extension.fx Abilities.burnout_ability) : json =
    Object
      [
        ("type", String "on_halt");
        ("limit", Null);
        ("cost_energy", Null);
        ("optional", Null);
        ("trigger_text", String "energy(self, 0) :-");
        ( "effect_text",
          String (Prolog_speak_renderer.effect_to_string a.card_effect) );
      ]

  let ability_to_json :
      (Prolog_speak_extension.fx, Prolog_speak_extension.trig) Abilities.ability ->
      json = function
    | Abilities.Passive a -> passive_ability_to_json a
    | Abilities.Activated a -> activated_ability_to_json a
    | Abilities.Triggered a -> triggered_ability_to_json a
    | Abilities.Burnout a -> burnout_ability_to_json a

  let personnel_to_json
      (p :
        ( Prolog_speak_extension.div,
          Prolog_speak_extension.fx,
          Prolog_speak_extension.trig )
        Cards.personnel) : json =
    Object
      [
        ("clause_type", String "agent");
        ("id", String (Core.Card_id.to_string p.id));
        ("name", String p.name);
        ( "kb",
          String
            (String.lowercase_ascii
               (Prolog_speak_renderer.division_to_string p.division)) );
        ("init_energy", Int (Int.to_int p.starting_cc));
        ("rules", Array (List.map ability_to_json p.abilities));
      ]
end

let the_unifier :
    ( Prolog_extension.div,
      Prolog_extension.fx,
      Prolog_extension.trig )
    Cards.personnel =
  {
    id = Core.Card_id.of_string "prolog:the_unifier";
    name = "The Unifier";
    division = Prolog_types.Prolog;
    lore = None;
    flavor_text = None;
    starting_cc = Int.five;
    abilities =
      [
        Abilities.Activated
          {
            id = None;
            cc_cost = Int.two;
            condition = None;
            card_effect =
              E.composite
                [
                  E.ext
                    (Prolog_types.Unify { variable = "X"; value = "target" });
                  E.add_cc ~target:T.this_personnel ~amount:Int.Positive.one;
                ];
          };
        Abilities.Activated
          {
            id = None;
            cc_cost = Int.one;
            condition = None;
            card_effect =
              E.ext (Prolog_types.Query { predicate = "member"; arity = 2 });
          };
        Abilities.Triggered
          {
            id = None;
            trigger = Abilities.Ext Prolog_types.On_unification_failure;
            limit = Some Abilities.Once_per_round;
            optionality = Abilities.Mandatory;
            condition = None;
            card_effect =
              E.composite
                [
                  E.ext Prolog_types.Backtrack;
                  E.draw ~player:T.you ~amount:Int.Positive.one;
                ];
          };
        Abilities.Triggered
          {
            id = None;
            trigger =
              Abilities.Ext
                (Prolog_types.On_goal_success { predicate = "solve" });
            limit = None;
            optionality = Abilities.Optional;
            condition = None;
            card_effect =
              E.ext (Prolog_types.Assert_fact { fact = "solved(problem)" });
          };
        Abilities.Passive
          {
            id = None;
            limit = None;
            condition = Some (Condition.personnel_count_in_sector Core.Alpha 1);
            card_effect = E.ext Prolog_types.Cut;
          };
        Abilities.Triggered
          {
            id = None;
            trigger = Abilities.Ext Prolog_types.On_backtrack;
            limit = None;
            optionality = Abilities.Optional;
            condition = None;
            card_effect =
              E.remove_breach_marker
                ~target:(T.entity_in_sector Core.Alpha)
                ~amount:Int.Positive.one;
          };
        Abilities.Burnout
          {
            id = None;
            card_effect =
              E.composite
                [
                  E.ext (Prolog_types.Retract_fact { fact = "all" });
                  E.remove_cc ~target:T.all_personnel ~amount:Int.Positive.two;
                ];
          };
      ];
  }

let () =
  let text_goldens = [ ("prolog_personnel_the_unifier", the_unifier) ] in

  run_text_golden_tests ~render:Prolog_renderer.personnel_to_string text_goldens;

  let prolog_speak_goldens =
    [ ("prolog_speak_personnel_the_unifier", the_unifier) ]
  in

  run_text_golden_tests ~render:Prolog_speak_renderer.personnel_to_string
    prolog_speak_goldens;

  let json_goldens = [ ("json_prolog_personnel_the_unifier", the_unifier) ] in

  run_json_golden_tests
    ~render:(fun p -> json_to_string (Prolog_json.personnel_to_json p))
    json_goldens;

  let json_prolog_speak_goldens =
    [ ("json_prolog_speak_personnel_the_unifier", the_unifier) ]
  in

  run_json_golden_tests
    ~render:(fun p -> json_to_string (Prolog_speak_json.personnel_to_json p))
    json_prolog_speak_goldens
