module J = Typefuckery.To_json
module E = Typefuckery.Engine
open Typefuckery.Core
open Typefuckery.Abilities
open Typefuckery.Cards
open Util

let test_personnel : core_personnel =
  {
    id = Card_id.of_string "test:sample";
    name = "Sample Personnel";
    division = Rust;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.three;
    abilities =
      [
        Passive
          {
            id = None;
            limit = Some Once_per_round;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:E.this_personnel ~amount:E.Amount.one;
          };
        Activated
          {
            id = None;
            cc_cost = E.CC.two;
            condition = None;
            card_effect = E.add_cc ~target:E.this_personnel ~amount:E.Amount.one;
          };
        Burnout
          {
            id = None;
            card_effect = E.add_cc ~target:E.all_personnel ~amount:E.Amount.one;
          };
      ];
  }

let test_entity : core_entity =
  {
    id = Card_id.of_string "test:entity";
    name = "Test Entity";
    division = Institute;
    lore = None;
    flavor_text = None;
    threat_level = Euclid;
    breach_timer = E.Timer.three;
    end_phase_effect = E.remove_cc ~target:E.all_personnel ~amount:E.Amount.one;
    breach_effect = E.remove_cc ~target:E.all_personnel ~amount:E.Amount.three;
    containment = { check = E.personnel_count_in_sector Alpha 2 };
  }

let () =
  let personnel_json = J.personnel_to_json test_personnel in
  let personnel_str = J.json_to_string personnel_json in

  assert (String.length personnel_str > 0);
  assert (String.sub personnel_str 0 1 = "{");

  assert_contains personnel_str "\"card_type_std\": \"personnel\""
    "personnel card_type_std";
  assert_contains personnel_str "\"card_type\": \"Personnel\""
    "personnel card_type";
  assert_contains personnel_str "\"id\": \"test:sample\"" "personnel id";
  assert_contains personnel_str "\"division_std\": \"rust\""
    "personnel division_std";
  assert_contains personnel_str "\"division\": \"Rust\"" "personnel division";
  assert_contains personnel_str "\"starting_cc\": 3" "personnel starting_cc";
  assert_contains personnel_str "\"abilities\":" "personnel abilities";

  let entity_json = J.entity_to_json test_entity in
  let entity_str = J.json_to_string entity_json in

  assert_contains entity_str "\"card_type_std\": \"entity\""
    "entity card_type_std";
  assert_contains entity_str "\"card_type\": \"Entity\"" "entity card_type";
  assert_contains entity_str "\"division_std\": \"institute\""
    "entity division_std";
  assert_contains entity_str "\"division\": \"Institute\"" "entity division";
  assert_contains entity_str "\"threat_level_std\": \"euclid\""
    "entity threat_level_std";
  assert_contains entity_str "\"threat_level\": \"Euclid\""
    "entity threat_level";
  assert_contains entity_str "\"breach_timer\": 3" "entity breach_timer"
