module E = Typefuckery.Engine
module R = Typefuckery.Registry
module TS = Typefuckery.To_string.Detailed_English
module J = Typefuckery.To_json
open Typefuckery.Core
open Typefuckery.Cards
open Typefuckery.Abilities
open Util

let card_manipulation_specialist : core_personnel =
  {
    id = Card_id.of_string "test:card_manipulation_specialist";
    name = "Card Manipulation Specialist";
    division = Institute;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.five;
    abilities =
      [
        Activated
          {
            id = None;
            cc_cost = E.CC.two;
            condition = None;
            card_effect = E.draw ~player:E.you ~amount:E.Amount.three;
          };
        Activated
          {
            id = None;
            cc_cost = E.CC.one;
            condition = None;
            card_effect =
              E.composite
                [
                  E.discard ~player:E.you ~amount:E.Amount.two;
                  E.draw ~player:E.you ~amount:E.Amount.two;
                ];
          };
        Triggered
          {
            id = None;
            trigger = Core (When_sector_breached Alpha);
            limit = Some Once_per_round;
            optionality = Mandatory;
            condition = None;
            card_effect = E.draw ~player:E.you ~amount:E.Amount.one;
          };
      ];
  }

let resource_redistributor : core_personnel =
  {
    id = Card_id.of_string "test:resource_redistributor";
    name = "Resource Redistributor";
    division = OCaml;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.four;
    abilities =
      [
        Activated
          {
            id = None;
            cc_cost = E.CC.three;
            condition = None;
            card_effect =
              E.let_ "Personnel"
                (E.choose_personnel ~filter:E.other_personnel ()) (fun donor ->
                  E.let_ "Recipient"
                    (E.choose_personnel ~filter:E.other_personnel ())
                    (fun recipient ->
                      E.composite
                        [
                          E.move_cc ~from:donor ~to_:E.this_personnel
                            ~amount:E.Amount.one;
                          E.move_cc ~from:E.this_personnel ~to_:recipient
                            ~amount:E.Amount.two;
                          E.add_cc ~target:recipient ~amount:E.Amount.one;
                        ]));
          };
      ];
  }

let temporal_strategist : core_personnel =
  {
    id = Card_id.of_string "test:temporal_strategist";
    name = "Temporal Strategist";
    division = Haskell;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.six;
    abilities =
      [
        Activated
          {
            id = None;
            cc_cost = E.CC.four;
            condition = None;
            card_effect =
              E.let_ "Personnel" (E.choose_personnel ()) (fun target ->
                  E.composite
                    [
                      E.add_cc ~target ~amount:E.Amount.one;
                      E.delayed ~window:End_phase ~scope:This_round
                        ~then_do:(E.add_cc ~target ~amount:E.Amount.two);
                      E.delayed ~window:End_phase ~scope:This_round
                        ~then_do:(E.remove_cc ~target ~amount:E.Amount.one);
                      E.delayed ~window:End_of_round ~scope:This_round
                        ~then_do:
                          (E.flip_sector ~target:(E.specific_sector Alpha)
                             ~state:Secure);
                    ]);
          };
      ];
  }

let conditional_operative : core_personnel =
  {
    id = Card_id.of_string "test:conditional_operative";
    name = "Conditional Operative";
    division = Ada;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.three;
    abilities =
      [
        Passive
          {
            id = None;
            limit = None;
            condition =
              Some
                (E.and_
                   (E.or_
                      (E.sector_is_breached Alpha)
                      (E.sector_is_breached Beta))
                   (E.not_ (E.sector_is_breached Lambda)));
            card_effect = E.add_cc ~target:E.this_personnel ~amount:E.Amount.two;
          };
        Activated
          {
            id = None;
            cc_cost = E.CC.two;
            condition =
              Some
                (E.and_
                   (E.personnel_count_in_sector Alpha 2)
                   (E.personnel_count_in_sector Beta 1));
            card_effect =
              E.flip_sector ~target:(E.choose_sector ()) ~state:Secure;
          };
      ];
  }

let custom_effect_engineer : core_personnel =
  {
    id = Card_id.of_string "test:custom_effect_engineer";
    name = "Custom Effect Engineer";
    division = Rust;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.four;
    abilities =
      [
        Activated
          {
            id = None;
            cc_cost = E.CC.three;
            condition = None;
            card_effect =
              E.composite
                [
                  E.log
                    "Initiate quantum flux protocol (intensity=high, \
                     target_sector=alpha)";
                  E.add_cc ~target:E.this_personnel ~amount:E.Amount.one;
                ];
          };
      ];
  }

let cascading_failure : core_entity =
  {
    id = Card_id.of_string "test:cascading_failure";
    name = "Cascading Failure";
    division = Institute;
    lore = None;
    flavor_text = None;
    threat_level = Keter;
    breach_timer = E.Timer.five;
    end_phase_effect =
      E.composite
        [
          E.remove_cc ~target:E.all_personnel ~amount:E.Amount.one;
          E.add_breach_marker ~target:(E.all_entities ()) ~amount:E.Amount.one;
        ];
    breach_effect =
      E.composite
        [
          E.remove_cc ~target:E.all_personnel ~amount:E.Amount.three;
          E.discard ~player:E.you ~amount:E.Amount.three;
        ];
    containment =
      { check = E.personnel_with_min_cc Gamma ~min_count:2 ~min_cc_each:2 };
  }

let deployment_coordinator : core_personnel =
  {
    id = Card_id.of_string "test:deployment_coordinator";
    name = "Deployment Coordinator";
    division = Institute;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.five;
    abilities =
      [
        Triggered
          {
            id = None;
            trigger = Core (When_personnel_deployed_in_sector Alpha);
            limit = None;
            optionality = Mandatory;
            condition = None;
            card_effect =
              E.composite
                [
                  E.add_cc ~target:E.this_personnel ~amount:E.Amount.one;
                  E.add_cc
                    ~target:(E.all_personnel_in_sector Alpha)
                    ~amount:E.Amount.one;
                ];
          };
        Triggered
          {
            id = None;
            trigger = Core (When_personnel_deployed_in_sector Beta);
            limit = Some Once_per_round;
            optionality = Optional;
            condition = None;
            card_effect = E.draw ~player:E.you ~amount:E.Amount.one;
          };
      ];
  }

let opportunistic_tactician : core_procedure =
  {
    id = Card_id.of_string "test:opportunistic_tactician";
    name = "Opportunistic Tactician";
    division = Haskell;
    lore = None;
    flavor_text = None;
    card_effect =
      E.composite
        [
          E.if_possible
            (E.move_cc ~from:(E.choose_personnel ())
               ~to_:(E.choose_personnel ()) ~amount:E.Amount.two);
          E.if_possible
            (E.add_breach_marker ~target:(E.choose_entity ())
               ~amount:E.Amount.one);
          E.add_cc ~target:(E.choose_personnel ()) ~amount:E.Amount.one;
        ];
  }

let coordinated_assault : core_procedure =
  {
    id = Card_id.of_string "test:coordinated_assault";
    name = "Coordinated Assault";
    division = Rust;
    lore = None;
    flavor_text = None;
    card_effect =
      E.let_ "Personnel" (E.choose_personnel ()) (fun target ->
          E.composite
            [
              E.add_cc ~target ~amount:E.Amount.three;
              E.move_personnel ~target ~to_sector:(E.choose_sector ());
              E.delayed ~window:End_phase ~scope:This_round
                ~then_do:(E.remove_cc ~target ~amount:E.Amount.two);
            ]);
  }

let selective_purge : core_event =
  {
    id = Card_id.of_string "test:selective_purge";
    name = "Selective Purge";
    division = Institute;
    lore = None;
    flavor_text = None;
    card_effect =
      E.composite
        [
          E.remove_cc
            ~target:
              (E.choose_personnel ~in_sector:Alpha
                 ~filter:(E.personnel_has_min_cc 3) ())
            ~amount:E.Amount.two;
          E.remove_cc
            ~target:
              (E.choose_personnel ~in_sector:Beta
                 ~filter:(E.personnel_has_min_cc 2) ())
            ~amount:E.Amount.one;
        ];
  }

let diplomatic_intervention : core_event =
  {
    id = Card_id.of_string "test:diplomatic_intervention";
    name = "Diplomatic Intervention";
    division = Institute;
    lore = None;
    flavor_text = None;
    card_effect =
      E.composite
        [
          E.move_personnel
            ~target:(E.choose_personnel ~chooser:(Some Starting_player) ())
            ~to_sector:(E.choose_sector ~chooser:(Some Starting_player) ());
          E.add_cc
            ~target:(E.choose_personnel ~chooser:(Some Controller) ())
            ~amount:E.Amount.two;
        ];
  }

let omni_responsive_agent : core_personnel =
  {
    id = Card_id.of_string "test:omni_responsive_agent";
    name = "Omni-Responsive Agent";
    division = Ada;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.seven;
    abilities =
      [
        Triggered
          {
            id = None;
            trigger = Core When_deployed;
            limit = None;
            optionality = Mandatory;
            condition = None;
            card_effect = E.add_cc ~target:E.this_personnel ~amount:E.Amount.one;
          };
        Triggered
          {
            id = None;
            trigger = Core End_phase;
            limit = None;
            optionality = Mandatory;
            condition = None;
            card_effect = E.add_cc ~target:E.this_personnel ~amount:E.Amount.one;
          };
        Triggered
          {
            id = None;
            trigger = Core End_of_round;
            limit = Some Once_per_round;
            optionality = Optional;
            condition = None;
            card_effect =
              E.remove_cc ~target:E.this_personnel ~amount:E.Amount.one;
          };
        Triggered
          {
            id = None;
            trigger = Core (When_sector_breached Alpha);
            limit = None;
            optionality = Mandatory;
            condition = None;
            card_effect =
              E.flip_sector ~target:(E.specific_sector Alpha) ~state:Secure;
          };
        Triggered
          {
            id = None;
            trigger = Core (When_entity_effect { in_sector = Some Beta });
            limit = None;
            optionality = Mandatory;
            condition = None;
            card_effect =
              E.add_cc
                ~target:(E.all_personnel_in_sector Beta)
                ~amount:E.Amount.one;
          };
      ];
  }

let reality_fragmenter : core_entity =
  {
    id = Card_id.of_string "test:reality_fragmenter";
    name = "Reality Fragmenter";
    division = Institute;
    lore = None;
    flavor_text = None;
    threat_level = Titan;
    breach_timer = E.Timer.seven;
    end_phase_effect =
      E.let_ "Personnel" (E.choose_personnel ~filter:E.personnel_in_play ())
        (fun victim ->
          E.composite
            [
              E.remove_cc ~target:victim ~amount:E.Amount.two;
              E.move_personnel ~target:victim ~to_sector:(E.choose_sector ());
              E.add_breach_marker ~target:(E.entity_in_sector Alpha)
                ~amount:E.Amount.one;
              E.flip_sector ~target:(E.choose_sector ()) ~state:Breached;
            ]);
    breach_effect =
      E.composite
        [
          E.send_to_abyss (E.choose_personnel ());
          E.send_to_abyss (E.choose_personnel ());
          E.send_to_abyss (E.choose_personnel ());
          E.flip_sector ~target:(E.choose_sector ()) ~state:Breached;
        ];
    containment =
      {
        check =
          E.and_
            (E.or_
               (E.personnel_count_in_sector Alpha 3)
               (E.personnel_count_in_sector Beta 3))
            (E.not_ (E.sector_is_breached Lambda));
      };
  }

let adaptive_defender : core_personnel =
  {
    id = Card_id.of_string "test:adaptive_defender";
    name = "Adaptive Defender";
    division = Rust;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.five;
    abilities =
      [
        Passive
          {
            id = None;
            limit = Some Once_per_round;
            condition = Some (E.sector_is_breached Alpha);
            card_effect =
              E.prevent_cc_loss
                ~target:(E.all_personnel_in_sector Alpha)
                ~amount:E.Amount.one;
          };
        Passive
          {
            id = None;
            limit = Some Once_per_round;
            condition = Some (E.sector_is_breached Beta);
            card_effect =
              E.prevent_cc_loss
                ~target:(E.all_personnel_in_sector Beta)
                ~amount:E.Amount.one;
          };
        Passive
          {
            id = None;
            limit = Some Once_per_round;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:E.this_personnel ~amount:E.Amount.one;
          };
      ];
  }

let sector_lockdown : core_procedure =
  {
    id = Card_id.of_string "test:sector_lockdown";
    name = "Sector Lockdown";
    division = Ada;
    lore = None;
    flavor_text = None;
    card_effect =
      E.composite
        [
          E.flip_sector ~target:(E.choose_sector ()) ~state:Secure;
          E.add_cc
            ~target:(E.all_personnel_in_sector Alpha)
            ~amount:E.Amount.one;
        ];
  }

let quantum_entangler : core_procedure =
  {
    id = Card_id.of_string "test:quantum_entangler";
    name = "Quantum Entangler";
    division = Haskell;
    lore = None;
    flavor_text = None;
    card_effect =
      E.let_ "Personnel" (E.choose_personnel ()) (fun first ->
          E.let_ "other Personnel"
            (E.choose_personnel ~filter:E.other_personnel ()) (fun second ->
              E.composite
                [
                  E.move_cc ~from:first ~to_:second ~amount:E.Amount.one;
                  E.delayed ~window:End_phase ~scope:This_round
                    ~then_do:
                      (E.move_cc ~from:second ~to_:first ~amount:E.Amount.one);
                  E.delayed ~window:End_phase ~scope:This_round
                    ~then_do:(E.add_cc ~target:first ~amount:E.Amount.one);
                  E.delayed ~window:End_phase ~scope:This_round
                    ~then_do:(E.add_cc ~target:second ~amount:E.Amount.one);
                ]));
  }

let () =
  let cards : core_card list =
    [
      Personnel card_manipulation_specialist;
      Personnel resource_redistributor;
      Personnel temporal_strategist;
      Personnel conditional_operative;
      Personnel custom_effect_engineer;
      Entity cascading_failure;
      Personnel deployment_coordinator;
      Procedure opportunistic_tactician;
      Procedure coordinated_assault;
      Event selective_purge;
      Event diplomatic_intervention;
      Personnel omni_responsive_agent;
      Entity reality_fragmenter;
      Personnel adaptive_defender;
      Procedure sector_lockdown;
      Procedure quantum_entangler;
    ]
  in

  let registry =
    match
      R.register_core_division R.empty ~id:"crazy-test" ~name:"Crazy Test Cards"
        ~cards
    with
    | Ok r -> r
    | Error _ -> failwith "Expected crazy test set registration to succeed"
  in

  let rendered_by_id =
    R.list_cards registry
    |> List.map (fun entry -> (entry.R.card_id, entry.R.rendered_text))
  in

  let find_rendered card_id_str =
    let card_id = Card_id.of_string card_id_str in
    match List.assoc_opt card_id rendered_by_id with
    | Some text -> text
    | None ->
        failwith
          (Printf.sprintf "Expected card %s to exist in registry" card_id_str)
  in

  let golden_files =
    [
      ( "crazy_personnel_card_manipulation_specialist",
        "test:card_manipulation_specialist" );
      ("crazy_personnel_resource_redistributor", "test:resource_redistributor");
      ("crazy_personnel_temporal_strategist", "test:temporal_strategist");
      ("crazy_personnel_conditional_operative", "test:conditional_operative");
      ("crazy_personnel_custom_effect_engineer", "test:custom_effect_engineer");
      ("crazy_entity_cascading_failure", "test:cascading_failure");
      ("crazy_personnel_deployment_coordinator", "test:deployment_coordinator");
      ("crazy_procedure_opportunistic_tactician", "test:opportunistic_tactician");
      ("crazy_procedure_coordinated_assault", "test:coordinated_assault");
      ("crazy_event_selective_purge", "test:selective_purge");
      ("crazy_event_diplomatic_intervention", "test:diplomatic_intervention");
      ("crazy_personnel_omni_responsive_agent", "test:omni_responsive_agent");
      ("crazy_entity_reality_fragmenter", "test:reality_fragmenter");
      ("crazy_personnel_adaptive_defender", "test:adaptive_defender");
      ("crazy_procedure_sector_lockdown", "test:sector_lockdown");
      ("crazy_procedure_quantum_entangler", "test:quantum_entangler");
    ]
  in

  run_text_golden_tests
    ~render:(fun card_id -> find_rendered card_id)
    golden_files;

  let json_golden_files =
    [
      ( "json_crazy_personnel_card_manipulation_specialist",
        Personnel card_manipulation_specialist );
      ( "json_crazy_personnel_resource_redistributor",
        Personnel resource_redistributor );
      ("json_crazy_personnel_temporal_strategist", Personnel temporal_strategist);
      ( "json_crazy_personnel_conditional_operative",
        Personnel conditional_operative );
      ( "json_crazy_personnel_custom_effect_engineer",
        Personnel custom_effect_engineer );
      ( "json_crazy_personnel_deployment_coordinator",
        Personnel deployment_coordinator );
      ( "json_crazy_personnel_omni_responsive_agent",
        Personnel omni_responsive_agent );
      ("json_crazy_personnel_adaptive_defender", Personnel adaptive_defender);
      ( "json_crazy_procedure_opportunistic_tactician",
        Procedure opportunistic_tactician );
      ("json_crazy_procedure_coordinated_assault", Procedure coordinated_assault);
      ("json_crazy_procedure_sector_lockdown", Procedure sector_lockdown);
      ("json_crazy_procedure_quantum_entangler", Procedure quantum_entangler);
      ("json_crazy_event_selective_purge", Event selective_purge);
      ("json_crazy_event_diplomatic_intervention", Event diplomatic_intervention);
      ("json_crazy_entity_cascading_failure", Entity cascading_failure);
      ("json_crazy_entity_reality_fragmenter", Entity reality_fragmenter);
    ]
  in

  run_json_golden_tests
    ~render:(fun card -> J.json_to_string (J.card_to_json card))
    json_golden_files
