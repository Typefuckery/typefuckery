module R = Typefuckery.Registry
module Cards = Typefuckery.Cards
module Core = Typefuckery.Core
module Int = Typefuckery.Int
module Effects = Typefuckery.Effects

let assert_true = Util.assert_true

let make_personnel id_str name : Cards.core_card =
  Cards.Personnel
    {
      id = Core.Card_id.of_string id_str;
      name;
      division = Core.Rust;
      lore = None;
      flavor_text = None;
      starting_cc = Int.one;
      abilities = [];
    }

let test_duplicate_set_id () =
  let card1 = make_personnel "test:card1" "Card One" in
  let card2 = make_personnel "test:card2" "Card Two" in

  let registry =
    match
      R.register_core_division R.empty ~id:"my-set" ~name:"My Set"
        ~cards:[ card1 ]
    with
    | Ok r -> r
    | Error _ -> failwith "First registration should succeed"
  in

  match
    R.register_core_division registry ~id:"my-set" ~name:"Different Name"
      ~cards:[ card2 ]
  with
  | Ok _ -> failwith "Expected duplicate set_id error"
  | Error (`Set_id_already_registered id) ->
      assert_true (id = "my-set") "Correct set_id reported"
  | Error _ -> failwith "Expected Set_id_already_registered error"

let test_duplicate_set_name () =
  let card1 = make_personnel "test:card1" "Card One" in
  let card2 = make_personnel "test:card2" "Card Two" in

  let registry =
    match
      R.register_core_division R.empty ~id:"set-1" ~name:"Same Name"
        ~cards:[ card1 ]
    with
    | Ok r -> r
    | Error _ -> failwith "First registration should succeed"
  in

  match
    R.register_core_division registry ~id:"set-2" ~name:"Same Name"
      ~cards:[ card2 ]
  with
  | Ok _ -> failwith "Expected duplicate set_name error"
  | Error (`Set_name_already_registered (name, existing_id)) ->
      assert_true (name = "Same Name") "Correct set_name reported";
      assert_true (existing_id = "set-1") "Correct existing set_id reported"
  | Error _ -> failwith "Expected Set_name_already_registered error"

let test_duplicate_card_id_within_set () =
  let card1 = make_personnel "test:same_id" "Card One" in
  let card2 = make_personnel "test:same_id" "Card Two" in

  match
    R.register_core_division R.empty ~id:"my-set" ~name:"My Set"
      ~cards:[ card1; card2 ]
  with
  | Ok _ -> failwith "Expected duplicate card_id within set error"
  | Error (`Duplicate_card_id_within_set (set_id, card_id, idx)) ->
      assert_true (set_id = "my-set") "Correct set_id reported";
      assert_true
        (Core.Card_id.equal card_id (Core.Card_id.of_string "test:same_id"))
        "Correct card_id reported";
      assert_true (idx = 1) "Correct duplicate position reported"
  | Error _ -> failwith "Expected Duplicate_card_id_within_set error"

let test_duplicate_card_id_across_sets () =
  let card1 = make_personnel "test:shared_id" "Card in Set 1" in
  let card2 = make_personnel "test:shared_id" "Card in Set 2" in

  let registry =
    match
      R.register_core_division R.empty ~id:"set-1" ~name:"Set One"
        ~cards:[ card1 ]
    with
    | Ok r -> r
    | Error _ -> failwith "First registration should succeed"
  in

  match
    R.register_core_division registry ~id:"set-2" ~name:"Set Two"
      ~cards:[ card2 ]
  with
  | Ok _ -> failwith "Expected card_id already registered error"
  | Error (`Card_id_already_registered (card_id, existing_set, new_set)) ->
      assert_true
        (Core.Card_id.equal card_id (Core.Card_id.of_string "test:shared_id"))
        "Correct card_id reported";
      assert_true (existing_set = "set-1") "Correct existing set_id reported";
      assert_true (new_set = "set-2") "Correct new set_id reported"
  | Error _ -> failwith "Expected Card_id_already_registered error"

let test_empty_cards_list () =
  match
    R.register_core_division R.empty ~id:"empty-set" ~name:"Empty Set" ~cards:[]
  with
  | Ok registry -> (
      let sets = R.list_sets registry in
      assert_true (List.length sets = 1) "Empty set registered";
      let set = R.find_set_by_id registry "empty-set" in
      match set with
      | Some s -> assert_true (List.length s.cards = 0) "Set has 0 cards"
      | None -> failwith "Set not found")
  | Error _ -> failwith "Empty cards list should be allowed"

let test_multiple_valid_registrations () =
  let card1 = make_personnel "set1:card" "Card 1" in
  let card2 = make_personnel "set2:card" "Card 2" in
  let card3 = make_personnel "set3:card" "Card 3" in

  let registry =
    R.empty |> fun r ->
    Result.get_ok
      (R.register_core_division r ~id:"set-1" ~name:"Set One" ~cards:[ card1 ])
    |> fun r ->
    Result.get_ok
      (R.register_core_division r ~id:"set-2" ~name:"Set Two" ~cards:[ card2 ])
    |> fun r ->
    Result.get_ok
      (R.register_core_division r ~id:"set-3" ~name:"Set Three" ~cards:[ card3 ])
  in

  assert_true (List.length (R.list_sets registry) = 3) "3 sets registered";
  assert_true (List.length (R.list_cards registry) = 3) "3 cards registered"

let () =
  test_duplicate_set_id ();

  test_duplicate_set_name ();

  test_duplicate_card_id_within_set ();

  test_duplicate_card_id_across_sets ();

  test_empty_cards_list ();

  test_multiple_valid_registrations ()
