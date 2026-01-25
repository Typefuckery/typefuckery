module R = Typefuckery.Registry
module Cards = Typefuckery.Cards
module Core = Typefuckery.Core
module Int = Typefuckery.Int
module TS = Typefuckery.To_string

let assert_some opt label =
  match opt with Some v -> v | None -> failwith ("Expected Some for " ^ label)

let assert_true = Util.assert_true

let () =
  let card_1 : Cards.core_card =
    Cards.Personnel
      {
        id = Core.Card_id.of_string "TEST:001";
        name = "Test Personnel";
        division = Core.Rust;
        lore = None;
        flavor_text = None;
        starting_cc = Int.zero;
        abilities = [];
      }
  in

  let registry_1 =
    match
      R.register_core_division R.empty ~id:"test-set" ~name:"Test Set"
        ~cards:[ card_1 ]
    with
    | Ok registry -> registry
    | Error _ -> failwith "Expected set registration to succeed"
  in

  assert_true
    (List.length (R.list_sets registry_1) = 1)
    "list_sets returns 1 set";

  let found_by_id =
    assert_some (R.find_set_by_id registry_1 "test-set") "find_set_by_id"
  in
  assert_true
    (found_by_id.metadata.name = "Test Set")
    "find_set_by_id has correct name";

  let found_by_name =
    assert_some (R.find_set_by_name registry_1 "Test Set") "find_set_by_name"
  in
  assert_true
    (found_by_name.metadata.id = "test-set")
    "find_set_by_name has correct id";

  let found_card =
    assert_some
      (R.find_card registry_1 (Core.Card_id.of_string "TEST:001"))
      "find_card"
  in
  assert_true
    (Core.Card_id.equal found_card.R.card_id
       (Core.Card_id.of_string "TEST:001"))
    "find_card returns the right card";
  assert_true
    (found_card.R.name = "Test Personnel")
    "registered card has correct name";
  assert_true
    (found_card.R.card_type = R.Personnel)
    "registered card has correct type";
  assert_true
    (String.length found_card.R.rendered_text > 0)
    "registered card has rendered text";

  (match
     R.register_core_division registry_1 ~id:"test-set-2" ~name:"Test Set 2"
       ~cards:[ card_1 ]
   with
  | Ok _ -> failwith "Expected duplicate card id across sets to fail"
  | Error (`Card_id_already_registered (cid, existing_sid, new_sid)) ->
      assert_true
        (Core.Card_id.equal cid (Core.Card_id.of_string "TEST:001"))
        "duplicate card_id reported";
      assert_true (existing_sid = "test-set") "existing_set_id reported";
      assert_true (new_sid = "test-set-2") "new_set_id reported"
  | Error (`Set_id_already_registered _) ->
      failwith "Expected Card_id_already_registered"
  | Error (`Set_name_already_registered _) ->
      failwith "Expected Card_id_already_registered"
  | Error (`Duplicate_card_id_within_set _) ->
      failwith "Expected Card_id_already_registered");

  let dup_card_a : Cards.core_card =
    Cards.Personnel
      {
        id = Core.Card_id.of_string "TEST:DUP";
        name = "Dup A";
        division = Core.Rust;
        lore = None;
        flavor_text = None;
        starting_cc = Int.one;
        abilities = [];
      }
  in
  let dup_card_b : Cards.core_card =
    Cards.Personnel
      {
        id = Core.Card_id.of_string "TEST:DUP";
        name = "Dup B";
        division = Core.Rust;
        lore = None;
        flavor_text = None;
        starting_cc = Int.one;
        abilities = [];
      }
  in
  (match
     R.register_core_division R.empty ~id:"dup-set" ~name:"Dup Set"
       ~cards:[ dup_card_a; dup_card_b ]
   with
  | Error (`Duplicate_card_id_within_set (sid, cid, idx)) ->
      assert_true (sid = "dup-set") "duplicate set id reported";
      assert_true
        (Core.Card_id.equal cid (Core.Card_id.of_string "TEST:DUP"))
        "duplicate card id reported";
      assert_true (idx = 1) "duplicate position is second entry"
  | Ok _ -> failwith "Expected intra-set duplicate to fail"
  | Error (`Set_id_already_registered _) ->
      failwith "Expected Duplicate_card_id_within_set"
  | Error (`Set_name_already_registered _) ->
      failwith "Expected Duplicate_card_id_within_set"
  | Error (`Card_id_already_registered _) ->
      failwith "Expected Duplicate_card_id_within_set");

  let custom_renderer card = "[CUSTOM] " ^ R.card_name card in
  let card_2 : Cards.core_card =
    Cards.Procedure
      {
        id = Core.Card_id.of_string "TEST:002";
        name = "Test Procedure";
        division = Core.Ada;
        lore = None;
        flavor_text = None;
        card_effect = Typefuckery.Effects.Noop;
      }
  in
  let registry_2 =
    match
      R.register_division registry_1 ~id:"custom-set" ~name:"Custom Set"
        ~cards:[ card_2 ] ~renderer:custom_renderer
    with
    | Ok registry -> registry
    | Error _ -> failwith "Expected custom set registration to succeed"
  in
  let found_custom =
    assert_some
      (R.find_card registry_2 (Core.Card_id.of_string "TEST:002"))
      "find custom card"
  in
  assert_true
    (found_custom.R.rendered_text = "[CUSTOM] Test Procedure")
    "custom renderer used"
