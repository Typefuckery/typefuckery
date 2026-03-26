module E = Typefuckery.Engine
module T = Typefuckery.Targets
module Int = Typefuckery.Int
module Condition = Typefuckery.Condition
module R = Typefuckery.Registry
module TS = Typefuckery.To_string.Detailed_English
module J = Typefuckery.To_json
open Typefuckery.Core
open Typefuckery.Cards
open Typefuckery.Abilities
open Util

let card_manipulation_specialist =
  {
    id = Card_id.of_string "test:card_manipulation_specialist";
    name = "Card Manipulation Specialist";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.five;
    abilities =
      [
        Activated
          {
            id = None;
            cc_cost = Int.two;
            condition = None;
            card_effect = E.draw ~player:T.you ~amount:Int.Positive.three;
          };
        Activated
          {
            id = None;
            cc_cost = Int.one;
            condition = None;
            card_effect =
              E.composite
                [
                  E.discard ~player:T.you ~amount:Int.Positive.two;
                  E.draw ~player:T.you ~amount:Int.Positive.two;
                ];
          };
        Triggered
          {
            id = None;
            trigger = Core (When_sector_breached Alpha);
            limit = Some Once_per_round;
            optionality = Mandatory;
            condition = None;
            card_effect = E.draw ~player:T.you ~amount:Int.Positive.one;
          };
      ];
  }

let resource_redistributor =
  {
    id = Card_id.of_string "test:resource_redistributor";
    name = "Resource Redistributor";
    division = OCaml_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.four;
    abilities =
      [
        Activated
          {
            id = None;
            cc_cost = Int.three;
            condition = None;
            card_effect =
              E.let_ (T.choose_personnel ~filter:T.other_personnel ())
                (fun donor ->
                  E.let_ (T.choose_personnel ~filter:T.other_personnel ())
                    (fun recipient ->
                      E.composite
                        [
                          E.move_cc ~from:donor ~to_:T.this_personnel
                            ~amount:Int.Positive.one;
                          E.move_cc ~from:T.this_personnel ~to_:recipient
                            ~amount:Int.Positive.two;
                          E.add_cc ~target:recipient ~amount:Int.Positive.one;
                        ]));
          };
      ];
  }

let temporal_strategist =
  {
    id = Card_id.of_string "test:temporal_strategist";
    name = "Temporal Strategist";
    division = Haskell_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.six;
    abilities =
      [
        Activated
          {
            id = None;
            cc_cost = Int.four;
            condition = None;
            card_effect =
              E.let_ (T.choose_personnel ()) (fun target ->
                  E.composite
                    [
                      E.add_cc ~target ~amount:Int.Positive.one;
                      E.delayed ~window:End_phase ~scope:This_round
                        ~then_do:(E.add_cc ~target ~amount:Int.Positive.two);
                      E.delayed ~window:End_phase ~scope:This_round
                        ~then_do:(E.remove_cc ~target ~amount:Int.Positive.one);
                      E.delayed ~window:End_of_round ~scope:This_round
                        ~then_do:
                          (E.flip_sector ~target:(T.specific_sector Alpha)
                             ~state:Secure);
                    ]);
          };
      ];
  }

let conditional_operative =
  {
    id = Card_id.of_string "test:conditional_operative";
    name = "Conditional Operative";
    division = Ada_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.three;
    abilities =
      [
        Passive
          {
            id = None;
            limit = None;
            condition =
              Some
                (Condition.and_
                   (Condition.or_
                      (Condition.sector_is_breached Alpha)
                      (Condition.sector_is_breached Beta))
                   (Condition.not_ (Condition.sector_is_breached Lambda)));
            card_effect =
              E.add_cc ~target:T.this_personnel ~amount:Int.Positive.two;
          };
        Activated
          {
            id = None;
            cc_cost = Int.two;
            condition =
              Some
                (Condition.and_
                   (Condition.personnel_count_in_sector Alpha 2)
                   (Condition.personnel_count_in_sector Beta 1));
            card_effect =
              E.flip_sector ~target:(T.choose_sector ()) ~state:Secure;
          };
      ];
  }

let custom_effect_engineer =
  {
    id = Card_id.of_string "test:custom_effect_engineer";
    name = "Custom Effect Engineer";
    division = Rust_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.four;
    abilities =
      [
        Activated
          {
            id = None;
            cc_cost = Int.three;
            condition = None;
            card_effect =
              E.composite
                [
                  E.log
                    "Initiate quantum flux protocol (intensity=high, \
                     target_sector=alpha)";
                  E.add_cc ~target:T.this_personnel ~amount:Int.Positive.one;
                ];
          };
      ];
  }

let cascading_failure =
  {
    id = Card_id.of_string "test:cascading_failure";
    name = "Cascading Failure";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    threat_level = Keter;
    breach_timer = Int.Positive.five;
    end_phase_effect =
      E.composite
        [
          E.remove_cc ~target:T.all_personnel ~amount:Int.Positive.one;
          E.add_breach_marker ~target:(T.all_entities ())
            ~amount:Int.Positive.one;
        ];
    breach_effect =
      E.composite
        [
          E.remove_cc ~target:T.all_personnel ~amount:Int.Positive.three;
          E.discard ~player:T.you ~amount:Int.Positive.three;
        ];
    containment =
      {
        check =
          Condition.personnel_with_min_cc Gamma ~min_count:2 ~min_cc_each:2;
      };
  }

let deployment_coordinator =
  {
    id = Card_id.of_string "test:deployment_coordinator";
    name = "Deployment Coordinator";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.five;
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
                  E.add_cc ~target:T.this_personnel ~amount:Int.Positive.one;
                  E.add_cc
                    ~target:(T.all_personnel_in_sector Alpha)
                    ~amount:Int.Positive.one;
                ];
          };
        Triggered
          {
            id = None;
            trigger = Core (When_personnel_deployed_in_sector Beta);
            limit = Some Once_per_round;
            optionality = Optional;
            condition = None;
            card_effect = E.draw ~player:T.you ~amount:Int.Positive.one;
          };
      ];
  }

let opportunistic_tactician : haskell core_procedure =
  {
    id = Card_id.of_string "test:opportunistic_tactician";
    name = "Opportunistic Tactician";
    division = Haskell_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.composite
        [
          E.if_possible
            (E.move_cc ~from:(T.choose_personnel ())
               ~to_:(T.choose_personnel ()) ~amount:Int.Positive.two);
          E.if_possible
            (E.add_breach_marker ~target:(T.choose_entity ())
               ~amount:Int.Positive.one);
          E.add_cc ~target:(T.choose_personnel ()) ~amount:Int.Positive.one;
        ];
  }

let coordinated_assault : rust core_procedure =
  {
    id = Card_id.of_string "test:coordinated_assault";
    name = "Coordinated Assault";
    division = Rust_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.let_ (T.choose_personnel ()) (fun target ->
          E.composite
            [
              E.add_cc ~target ~amount:Int.Positive.three;
              E.move_personnel ~target ~to_sector:(T.choose_sector ());
              E.delayed ~window:End_phase ~scope:This_round
                ~then_do:(E.remove_cc ~target ~amount:Int.Positive.two);
            ]);
  }

let selective_purge : institute core_event =
  {
    id = Card_id.of_string "test:selective_purge";
    name = "Selective Purge";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.composite
        [
          E.remove_cc
            ~target:
              (T.choose_personnel ~in_sector:Alpha
                 ~filter:(T.personnel_has_min_cc 3) ())
            ~amount:Int.Positive.two;
          E.remove_cc
            ~target:
              (T.choose_personnel ~in_sector:Beta
                 ~filter:(T.personnel_has_min_cc 2) ())
            ~amount:Int.Positive.one;
        ];
  }

let diplomatic_intervention : institute core_event =
  {
    id = Card_id.of_string "test:diplomatic_intervention";
    name = "Diplomatic Intervention";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.composite
        [
          E.move_personnel
            ~target:(T.choose_personnel ~chooser:Starting_player ())
            ~to_sector:(T.choose_sector ~chooser:Starting_player ());
          E.add_cc
            ~target:(T.choose_personnel ~chooser:Controller ())
            ~amount:Int.Positive.two;
        ];
  }

let omni_responsive_agent =
  {
    id = Card_id.of_string "test:omni_responsive_agent";
    name = "Omni-Responsive Agent";
    division = Ada_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.seven;
    abilities =
      [
        Triggered
          {
            id = None;
            trigger = Core When_deployed;
            limit = None;
            optionality = Mandatory;
            condition = None;
            card_effect =
              E.add_cc ~target:T.this_personnel ~amount:Int.Positive.one;
          };
        Triggered
          {
            id = None;
            trigger = Core End_phase;
            limit = None;
            optionality = Mandatory;
            condition = None;
            card_effect =
              E.add_cc ~target:T.this_personnel ~amount:Int.Positive.one;
          };
        Triggered
          {
            id = None;
            trigger = Core End_of_round;
            limit = Some Once_per_round;
            optionality = Optional;
            condition = None;
            card_effect =
              E.remove_cc ~target:T.this_personnel ~amount:Int.Positive.one;
          };
        Triggered
          {
            id = None;
            trigger = Core (When_sector_breached Alpha);
            limit = None;
            optionality = Mandatory;
            condition = None;
            card_effect =
              E.flip_sector ~target:(T.specific_sector Alpha) ~state:Secure;
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
                ~target:(T.all_personnel_in_sector Beta)
                ~amount:Int.Positive.one;
          };
      ];
  }

let reality_fragmenter =
  {
    id = Card_id.of_string "test:reality_fragmenter";
    name = "Reality Fragmenter";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    threat_level = Titan;
    breach_timer = Int.Positive.seven;
    end_phase_effect =
      E.let_ (T.choose_personnel ~filter:T.personnel_in_play ()) (fun victim ->
          E.composite
            [
              E.remove_cc ~target:victim ~amount:Int.Positive.two;
              E.move_personnel ~target:victim ~to_sector:(T.choose_sector ());
              E.add_breach_marker ~target:(T.entity_in_sector Alpha)
                ~amount:Int.Positive.one;
              E.flip_sector ~target:(T.choose_sector ()) ~state:Breached;
            ]);
    breach_effect =
      E.composite
        [
          E.send_to_abyss (T.choose_personnel ());
          E.send_to_abyss (T.choose_personnel ());
          E.send_to_abyss (T.choose_personnel ());
          E.flip_sector ~target:(T.choose_sector ()) ~state:Breached;
        ];
    containment =
      {
        check =
          Condition.and_
            (Condition.or_
               (Condition.personnel_count_in_sector Alpha 3)
               (Condition.personnel_count_in_sector Beta 3))
            (Condition.not_ (Condition.sector_is_breached Lambda));
      };
  }

let adaptive_defender =
  {
    id = Card_id.of_string "test:adaptive_defender";
    name = "Adaptive Defender";
    division = Rust_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.five;
    abilities =
      [
        Passive
          {
            id = None;
            limit = Some Once_per_round;
            condition = Some (Condition.sector_is_breached Alpha);
            card_effect =
              E.prevent_cc_loss
                ~target:(T.all_personnel_in_sector Alpha)
                ~amount:Int.Positive.one;
          };
        Passive
          {
            id = None;
            limit = Some Once_per_round;
            condition = Some (Condition.sector_is_breached Beta);
            card_effect =
              E.prevent_cc_loss
                ~target:(T.all_personnel_in_sector Beta)
                ~amount:Int.Positive.one;
          };
        Passive
          {
            id = None;
            limit = Some Once_per_round;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:T.this_personnel
                ~amount:Int.Positive.one;
          };
      ];
  }

let sector_lockdown : ada core_procedure =
  {
    id = Card_id.of_string "test:sector_lockdown";
    name = "Sector Lockdown";
    division = Ada_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.composite
        [
          E.flip_sector ~target:(T.choose_sector ()) ~state:Secure;
          E.add_cc
            ~target:(T.all_personnel_in_sector Alpha)
            ~amount:Int.Positive.one;
        ];
  }

let quantum_entangler : haskell core_procedure =
  {
    id = Card_id.of_string "test:quantum_entangler";
    name = "Quantum Entangler";
    division = Haskell_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.let_ (T.choose_personnel ()) (fun first ->
          E.let_ (T.choose_personnel ~filter:T.other_personnel ())
            (fun second ->
              E.composite
                [
                  E.move_cc ~from:first ~to_:second ~amount:Int.Positive.one;
                  E.delayed ~window:End_phase ~scope:This_round
                    ~then_do:
                      (E.move_cc ~from:second ~to_:first
                         ~amount:Int.Positive.one);
                  E.delayed ~window:End_phase ~scope:This_round
                    ~then_do:(E.add_cc ~target:first ~amount:Int.Positive.one);
                  E.delayed ~window:End_phase ~scope:This_round
                    ~then_do:(E.add_cc ~target:second ~amount:Int.Positive.one);
                ]));
  }

let () =
  let cards =
    [
      pack_core_card (Personnel card_manipulation_specialist);
      pack_core_card (Personnel resource_redistributor);
      pack_core_card (Personnel temporal_strategist);
      pack_core_card (Personnel conditional_operative);
      pack_core_card (Personnel custom_effect_engineer);
      pack_core_card (Entity cascading_failure);
      pack_core_card (Personnel deployment_coordinator);
      pack_core_card (Procedure opportunistic_tactician);
      pack_core_card (Procedure coordinated_assault);
      pack_core_card (Event selective_purge);
      pack_core_card (Event diplomatic_intervention);
      pack_core_card (Personnel omni_responsive_agent);
      pack_core_card (Entity reality_fragmenter);
      pack_core_card (Personnel adaptive_defender);
      pack_core_card (Procedure sector_lockdown);
      pack_core_card (Procedure quantum_entangler);
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
        pack_core_card (Personnel card_manipulation_specialist) );
      ( "json_crazy_personnel_resource_redistributor",
        pack_core_card (Personnel resource_redistributor) );
      ( "json_crazy_personnel_temporal_strategist",
        pack_core_card (Personnel temporal_strategist) );
      ( "json_crazy_personnel_conditional_operative",
        pack_core_card (Personnel conditional_operative) );
      ( "json_crazy_personnel_custom_effect_engineer",
        pack_core_card (Personnel custom_effect_engineer) );
      ( "json_crazy_personnel_deployment_coordinator",
        pack_core_card (Personnel deployment_coordinator) );
      ( "json_crazy_personnel_omni_responsive_agent",
        pack_core_card (Personnel omni_responsive_agent) );
      ( "json_crazy_personnel_adaptive_defender",
        pack_core_card (Personnel adaptive_defender) );
      ( "json_crazy_procedure_opportunistic_tactician",
        pack_core_card (Procedure opportunistic_tactician) );
      ( "json_crazy_procedure_coordinated_assault",
        pack_core_card (Procedure coordinated_assault) );
      ( "json_crazy_procedure_sector_lockdown",
        pack_core_card (Procedure sector_lockdown) );
      ( "json_crazy_procedure_quantum_entangler",
        pack_core_card (Procedure quantum_entangler) );
      ( "json_crazy_event_selective_purge",
        pack_core_card (Event selective_purge) );
      ( "json_crazy_event_diplomatic_intervention",
        pack_core_card (Event diplomatic_intervention) );
      ( "json_crazy_entity_cascading_failure",
        pack_core_card (Entity cascading_failure) );
      ( "json_crazy_entity_reality_fragmenter",
        pack_core_card (Entity reality_fragmenter) );
    ]
  in

  run_json_golden_tests
    ~render:(fun card -> J.json_to_string (J.any_core_card_to_json card))
    json_golden_files
