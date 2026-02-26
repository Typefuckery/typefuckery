module TS = Typefuckery.To_string
module Engine = Typefuckery.Engine
module Cards = Typefuckery.Cards
module Effects = Typefuckery.Effects
module Abilities = Typefuckery.Abilities
module Core = Typefuckery.Core
module Int = Typefuckery.Int
module Division_intf = Typefuckery.Division_intf
module Condition = Typefuckery.Condition
open Util

let assert_eq actual expected label =
  if actual <> expected then
    failwith (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let test_core_matches_detailed_english () =
  let card : Cards.core_card =
    Cards.Personnel
      {
        id = Core.Card_id.of_string "test:core-match";
        name = "Core Match Test";
        division = Core.Rust;
        lore = None;
        flavor_text = None;
        starting_cc = Int.three;
        abilities =
          [
            Abilities.Activated
              {
                id = None;
                cc_cost = Int.one;
                condition = None;
                card_effect =
                  Effects.Core_effect
                    (Effects.Add_CC
                       {
                         target = Typefuckery.Targets.This_personnel;
                         amount = Int.Positive.two;
                       });
              };
          ];
      }
  in
  let core_output = TS.Core.card_to_string card in
  let legacy_output = TS.Detailed_English.card_to_string card in
  assert_eq core_output legacy_output
    "Core and Detailed_English produce same output"

module Test_extension = struct
  type fx = Custom_effect of string | Another_effect of { value : int }
  type trig = Custom_trigger of string
  type div = Core.division

  let fx_to_string = function
    | Custom_effect msg -> Printf.sprintf "[CUSTOM: %s]" msg
    | Another_effect { value } -> Printf.sprintf "[ANOTHER: %d]" value

  let trig_to_string = function
    | Custom_trigger msg -> Printf.sprintf "[TRIG: %s]" msg

  let div_to_string = function
    | Core.Ada -> "Ada"
    | Core.Haskell -> "Haskell"
    | Core.OCaml -> "OCaml"
    | Core.Rust -> "Rust"
    | Core.Institute -> "Institute"
end

module Test_renderer = TS.MakeExt (Test_extension) (Typefuckery.English_phrases)

let test_custom_effect_delegation () =
  let custom_eff : Test_extension.fx Effects.t =
    Effects.Ext (Test_extension.Custom_effect "hello world")
  in
  let output = Test_renderer.effect_to_string custom_eff in
  assert_eq output "[CUSTOM: hello world]" "Custom effect delegated correctly"

let test_another_custom_effect () =
  let eff : Test_extension.fx Effects.t =
    Effects.Ext (Test_extension.Another_effect { value = 42 })
  in
  let output = Test_renderer.effect_to_string eff in
  assert_eq output "[ANOTHER: 42]" "Another custom effect delegated correctly"

let test_custom_trigger_delegation () =
  let trig : Test_extension.trig Abilities.trigger =
    Abilities.Ext (Test_extension.Custom_trigger "on fire")
  in
  let output = Test_renderer.trigger_to_string trig in
  assert_eq output "[TRIG: on fire]" "Custom trigger delegated correctly"

let test_composite_with_extension () =
  let eff : Test_extension.fx Effects.t =
    Effects.Composite
      [
        Effects.Core_effect
          (Effects.Add_CC
             {
               target = Typefuckery.Targets.This_personnel;
               amount = Int.Positive.one;
             });
        Effects.Ext (Test_extension.Custom_effect "bonus");
      ]
  in
  let output = Test_renderer.effect_to_string eff in
  assert_contains output "Add 1 CC" "Composite contains core effect";
  assert_contains output "[CUSTOM: bonus]" "Composite contains custom effect"

let test_if_possible_with_extension () =
  let eff : Test_extension.fx Effects.t =
    Effects.If_possible (Effects.Ext (Test_extension.Custom_effect "maybe"))
  in
  let output = Test_renderer.effect_to_string eff in
  assert_contains output "[CUSTOM: maybe]" "If_possible wraps custom effect";
  assert_contains output "If you can't" "If_possible adds fizzle text"

let test_personnel_with_extension () =
  let card :
      (Test_extension.div, Test_extension.fx, Test_extension.trig) Cards.card =
    Cards.Personnel
      {
        id = Core.Card_id.of_string "test:ext-personnel";
        name = "Extension Personnel";
        division = Core.Ada;
        lore = None;
        flavor_text = None;
        starting_cc = Int.two;
        abilities =
          [
            Abilities.Activated
              {
                id = None;
                cc_cost = Int.one;
                condition = None;
                card_effect =
                  Effects.Ext (Test_extension.Custom_effect "activate!");
              };
            Abilities.Triggered
              {
                id = None;
                trigger =
                  Abilities.Ext (Test_extension.Custom_trigger "something");
                limit = None;
                optionality = Abilities.Mandatory;
                condition = None;
                card_effect =
                  Effects.Core_effect
                    (Effects.Draw
                       {
                         player = Typefuckery.Targets.You;
                         amount = Int.Positive.one;
                       });
              };
          ];
      }
  in
  let output = Test_renderer.card_to_string card in
  assert_contains output "Extension Personnel" "Card name present";
  assert_contains output "[CUSTOM: activate!]" "Custom effect in ability";
  assert_contains output "[TRIG: something]"
    "Custom trigger in triggered ability";
  assert_contains output "Draw 1 card" "Core effect in triggered ability"

module Custom_phrases : TS.PHRASES = struct
  include Typefuckery.English_phrases

  let personnel = "PERSON"
end

module Custom_serializer = TS.Make (Custom_phrases)

let test_make_backward_compat () =
  let card : Cards.core_card =
    Cards.Personnel
      {
        id = Core.Card_id.of_string "test:compat";
        name = "Compat Test";
        division = Core.Haskell;
        lore = None;
        flavor_text = None;
        starting_cc = Int.one;
        abilities =
          [
            Abilities.Activated
              {
                id = None;
                cc_cost = Int.one;
                condition = None;
                card_effect =
                  Effects.Core_effect
                    (Effects.Add_CC
                       {
                         target = Typefuckery.Targets.This_personnel;
                         amount = Int.Positive.one;
                       });
              };
          ];
      }
  in
  let output = Custom_serializer.card_to_string card in
  assert_contains output "PERSON" "Custom PHRASES respected by Make functor"

let test_core_trigger_in_extension_renderer () =
  let trig : Test_extension.trig Abilities.trigger =
    Abilities.Core Abilities.End_of_round
  in
  let output = Test_renderer.trigger_to_string trig in
  assert_contains output "end of round"
    "Core trigger rendered by extension renderer"

let test_procedure_with_extension () =
  let card :
      (Test_extension.div, Test_extension.fx, Test_extension.trig) Cards.card =
    Cards.Procedure
      {
        id = Core.Card_id.of_string "test:ext-proc";
        name = "Extension Procedure";
        division = Core.OCaml;
        lore = None;
        flavor_text = None;
        card_effect = Effects.Ext (Test_extension.Another_effect { value = 99 });
      }
  in
  let output = Test_renderer.card_to_string card in
  assert_contains output "Extension Procedure" "Procedure name present";
  assert_contains output "[ANOTHER: 99]" "Custom effect in procedure"

let test_event_with_extension () =
  let card :
      (Test_extension.div, Test_extension.fx, Test_extension.trig) Cards.card =
    Cards.Event
      {
        id = Core.Card_id.of_string "test:ext-event";
        name = "Extension Event";
        division = Core.Institute;
        lore = None;
        flavor_text = None;
        card_effect =
          Effects.Composite
            [
              Effects.Ext (Test_extension.Custom_effect "event!"); Effects.Noop;
            ];
      }
  in
  let output = Test_renderer.card_to_string card in
  assert_contains output "Extension Event" "Event name present";
  assert_contains output "[CUSTOM: event!]" "Custom effect in event"

let test_entity_with_extension () =
  let card :
      (Test_extension.div, Test_extension.fx, Test_extension.trig) Cards.card =
    Cards.Entity
      {
        id = Core.Card_id.of_string "test:ext-entity";
        name = "Extension Entity";
        division = Core.Institute;
        lore = None;
        flavor_text = None;
        threat_level = Cards.Keter;
        breach_timer = Int.Positive.three;
        end_phase_effect = Effects.Ext (Test_extension.Custom_effect "breach!");
        breach_effect =
          Effects.Ext (Test_extension.Custom_effect "catastrophe!");
        containment = { check = Condition.always };
      }
  in
  let output = Test_renderer.card_to_string card in
  assert_contains output "Extension Entity" "Entity name present";
  assert_contains output "Keter" "Threat level present";
  assert_contains output "[CUSTOM: breach!]" "Custom effect in entity"

let () =
  test_core_matches_detailed_english ();

  test_custom_effect_delegation ();

  test_another_custom_effect ();

  test_custom_trigger_delegation ();

  test_composite_with_extension ();

  test_if_possible_with_extension ();

  test_personnel_with_extension ();

  test_make_backward_compat ();

  test_core_trigger_in_extension_renderer ();

  test_procedure_with_extension ();

  test_event_with_extension ();

  test_entity_with_extension ()
