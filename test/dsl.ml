module E = Typefuckery.Engine
module T = Typefuckery.Targets
module Int = Typefuckery.Int
module Condition = Typefuckery.Condition
module DSL = Typefuckery.Card_dsl
module TS = Typefuckery.To_string.Detailed_English
open Typefuckery.Core
open Typefuckery.Cards
open Typefuckery.Abilities
open Util
open DSL
open DSL.Syntax
open DSL.Conditions

let test_dsl_equivalence () =
  let functional_effect =
    E.add_cc ~target:T.this_personnel ~amount:Int.Positive.one
  in
  let dsl_effect = me +@ Int.Positive.one in
  assert_true
    (TS.card_effect_to_string functional_effect
    = TS.card_effect_to_string dsl_effect)
    "DSL me +@ equals E.add_cc this_personnel";

  let functional_composite =
    E.composite
      [
        E.add_cc ~target:T.this_personnel ~amount:Int.Positive.one;
        E.remove_cc ~target:T.all_personnel ~amount:Int.Positive.two;
      ]
  in
  let dsl_composite = me +@ Int.Positive.one &+ everyone -@ Int.Positive.two in
  assert_true
    (TS.card_effect_to_string functional_composite
    = TS.card_effect_to_string dsl_composite)
    "DSL &+ equals E.composite";

  let functional_transfer =
    E.move_cc ~from:T.this_personnel ~to_:(T.choose_personnel ())
      ~amount:Int.Positive.one
  in
  let dsl_transfer = me ==> anyone () in
  assert_true
    (TS.card_effect_to_string functional_transfer
    = TS.card_effect_to_string dsl_transfer)
    "DSL ==> equals E.move_cc";

  let functional_transfer_two =
    E.move_cc ~from:T.this_personnel ~to_:(T.choose_personnel ())
      ~amount:Int.Positive.two
  in
  let dsl_transfer_two = transfer Int.Positive.two me (anyone ()) in
  assert_true
    (TS.card_effect_to_string functional_transfer_two
    = TS.card_effect_to_string dsl_transfer_two)
    "DSL transfer equals E.move_cc with amount";

  let functional_move =
    E.move_personnel ~target:T.this_personnel
      ~to_sector:(T.specific_sector Lambda)
  in
  let dsl_move = me --> to_sector Lambda in
  assert_true
    (TS.card_effect_to_string functional_move
    = TS.card_effect_to_string dsl_move)
    "DSL --> equals E.move_personnel";

  let functional_move_choose =
    E.move_personnel ~target:(T.choose_personnel ())
      ~to_sector:(T.choose_sector ())
  in
  let dsl_move_choose = anyone () --> any_sector () in
  assert_true
    (TS.card_effect_to_string functional_move_choose
    = TS.card_effect_to_string dsl_move_choose)
    "DSL --> any_sector equals E.move_personnel choose";

  let let_star_effect =
    let* target = anyone_else () in
    me ==> target &+ target +@ Int.Positive.one
  in
  let functional_let =
    E.let_ (T.choose_personnel ~filter:T.other_personnel ()) (fun target ->
        E.composite
          [
            E.move_cc ~from:T.this_personnel ~to_:target
              ~amount:Int.Positive.one;
            E.add_cc ~target ~amount:Int.Positive.one;
          ])
  in
  assert_true
    (TS.card_effect_to_string let_star_effect
    = TS.card_effect_to_string functional_let)
    "DSL let* equals E.let_";

  let functional_add_breach =
    E.add_breach_marker ~target:T.entity_in_this_sector ~amount:Int.Positive.one
  in
  let dsl_add_breach = entity_here |+ Int.Positive.one in
  assert_true
    (TS.card_effect_to_string functional_add_breach
    = TS.card_effect_to_string dsl_add_breach)
    "DSL |+ equals E.add_breach_marker";

  let functional_remove_breach =
    E.remove_breach_marker ~target:(T.choose_entity ()) ~amount:Int.Positive.two
  in
  let dsl_remove_breach = any_entity () |- Int.Positive.two in
  assert_true
    (TS.card_effect_to_string functional_remove_breach
    = TS.card_effect_to_string dsl_remove_breach)
    "DSL |- equals E.remove_breach_marker"

let test_condition_equivalence () =
  let and_cond = sector_breached Alpha &&? sector_breached Beta in
  let functional_and =
    Condition.and_
      (Condition.sector_is_breached Alpha)
      (Condition.sector_is_breached Beta)
  in
  assert_true
    (and_cond.kind = functional_and.kind)
    "DSL &&? equals Condition.and_";

  let or_cond = sector_breached Alpha ||? sector_breached Beta in
  let functional_or =
    Condition.or_
      (Condition.sector_is_breached Alpha)
      (Condition.sector_is_breached Beta)
  in
  assert_true (or_cond.kind = functional_or.kind) "DSL ||? equals Condition.or_";

  let not_cond = !?(sector_breached Lambda) in
  let functional_not = Condition.not_ (Condition.sector_is_breached Lambda) in
  assert_true
    (not_cond.kind = functional_not.kind)
    "DSL !? equals Condition.not_";

  let complex_cond =
    sector_breached Alpha ||? sector_breached Beta
    &&? !?(sector_breached Lambda)
  in
  let functional_complex =
    Condition.and_
      (Condition.or_
         (Condition.sector_is_breached Alpha)
         (Condition.sector_is_breached Beta))
      (Condition.not_ (Condition.sector_is_breached Lambda))
  in
  assert_true
    (complex_cond.kind = functional_complex.kind)
    "DSL complex condition equals functional equivalent";

  assert_true
    (always.kind = Condition.always.kind)
    "DSL always equals Condition.always";
  assert_true
    (never.kind = Condition.never.kind)
    "DSL never equals Condition.never";

  let count_cond = personnel_count Alpha 2 in
  let functional_count = Condition.personnel_count_in_sector Alpha 2 in
  assert_true
    (count_cond.kind = functional_count.kind)
    "DSL personnel_count equals Condition.personnel_count_in_sector";

  let cc_cond = personnel_with_cc Beta ~min_count:3 ~min_cc_each:2 in
  let functional_cc =
    Condition.personnel_with_min_cc Beta ~min_count:3 ~min_cc_each:2
  in
  assert_true
    (cc_cond.kind = functional_cc.kind)
    "DSL personnel_with_cc equals Condition.personnel_with_min_cc";

  assert_true (here = T.this_sector) "DSL here equals T.this_sector"

let dsl_operative : core_personnel =
  {
    id = Card_id.of_string "test:dsl_operative";
    name = "DSL Operative";
    division = Institute;
    lore = None;
    flavor_text = None;
    starting_cc = Int.three;
    abilities =
      [
        Passive
          {
            id = None;
            limit = None;
            condition = None;
            card_effect = me +@ Int.Positive.one;
          };
        Activated
          {
            id = None;
            cc_cost = Int.one;
            condition = None;
            card_effect = anyone () +@ Int.Positive.two;
          };
        Triggered
          {
            id = None;
            trigger = Core When_deployed;
            limit = None;
            optionality = Mandatory;
            condition = None;
            card_effect = everyone_in Alpha -@ Int.Positive.one;
          };
        Burnout { id = None; card_effect = everyone -@ Int.Positive.one };
      ];
  }

let dsl_composite_test : core_personnel =
  {
    id = Card_id.of_string "test:dsl_composite";
    name = "DSL Composite Test";
    division = Rust;
    lore = None;
    flavor_text = None;
    starting_cc = Int.four;
    abilities =
      [
        Activated
          {
            id = None;
            cc_cost = Int.two;
            condition = None;
            card_effect =
              me +@ Int.Positive.one &+ everyone_here -@ Int.Positive.one;
          };
        Activated
          {
            id = None;
            cc_cost = Int.one;
            condition = None;
            card_effect =
              anyone_else () +@ Int.Positive.one
              &+ me -@ Int.Positive.one
              &+ everyone_in Beta +@ Int.Positive.two;
          };
      ];
  }

let dsl_sector_procedure : core_procedure =
  {
    id = Card_id.of_string "test:dsl_sector_procedure";
    name = "DSL Sector Procedure";
    division = Institute;
    lore = None;
    flavor_text = None;
    card_effect =
      everyone_in Lambda +@ Int.Positive.one &+ my_sector -@ Int.Positive.one;
  }

let terse_operative =
  personnel "test:terse_operative" "Terse Operative" Institute ~cc:CC.three
    [
      passive (me +@ one);
      activated ~cost:CC.one (anyone () +@ two);
      on_deploy (everyone_in Alpha -@ one);
      burnout (everyone -@ one);
    ]

let terse_conditional =
  personnel "test:terse_conditional" "Terse Conditional" Ada ~cc:CC.three
    [
      passive
        ~when_:
          (sector_breached Alpha ||? sector_breached Beta
          &&? !?(sector_breached Lambda))
        (me +@ two);
      activated ~cost:CC.two
        ~when_:(personnel_count Alpha 2 &&? personnel_count Beta 1)
        (secure (any_sector ()));
    ]

let terse_defender =
  personnel "test:terse_defender" "Terse Defender" Rust ~cc:CC.five
    [
      passive ~limit:once_per_round ~when_:(sector_breached Alpha)
        (shield (everyone_in Alpha) one);
      passive ~limit:once_per_round ~when_:(sector_breached Beta)
        (shield (everyone_in Beta) one);
      passive ~limit:once_per_round (shield me one);
    ]

let terse_temporal =
  personnel "test:terse_temporal" "Terse Temporal" Haskell ~cc:CC.six
    [
      activated ~cost:CC.four
        (let* target = anyone () in
         seq
           [
             target +@ one;
             at_end_phase (target +@ two);
             at_end_round (secure (to_sector Alpha));
           ]);
    ]

let terse_responder =
  personnel "test:terse_responder" "Terse Responder" Ada ~cc:CC.seven
    [
      on_deploy (me +@ one);
      on_end_phase (me +@ one);
      on_end_round ~limit:once_per_round ~optional:true (me -@ one);
      on_breach Alpha (secure (to_sector Alpha));
      on_entity_effect ~in_sector:Beta (everyone_in Beta +@ one);
    ]

let terse_coordinator =
  personnel "test:terse_coordinator" "Terse Coordinator" Institute ~cc:CC.five
    [
      on_deployed_in Alpha (seq [ me +@ one; everyone_in Alpha +@ one ]);
      on_deployed_in Beta ~limit:once_per_round ~optional:true (draw one);
    ]

let terse_memguard =
  personnel "test:terse_memguard" "Terse Memguard" Rust ~cc:CC.four
    [
      passive (shield me one);
      on_cc_loss ~not_from_spend:true me ~limit:once_per_round ~optional:true
        (shield me two);
      activated ~cost:CC.three (shield (anyone ()) one);
      burnout (shield my_sector one);
    ]

let terse_card_master =
  personnel "test:terse_card_master" "Terse Card Master" Institute ~cc:CC.five
    [
      activated ~cost:CC.two (draw three);
      activated ~cost:CC.one (seq [ discard two; draw two ]);
      on_breach Alpha ~limit:once_per_round (draw one);
    ]

let terse_protocol =
  procedure "test:terse_protocol" "Terse Protocol" Institute
    (seq [ anyone () +@ one; any_entity () |- one ])

let terse_assault =
  procedure "test:terse_assault" "Terse Assault" Rust
    (let* target = anyone () in
     seq
       [
         target +@ three; target --> any_sector (); at_end_phase (target -@ two);
       ])

let sigkill_engagement =
  procedure "institute:sigkill_engagement" "Sigkill Engagement" Institute
    ~flavor_text:"Sigkill takes what containment cannot."
    (let* source = anyone ~filter:controlled_by_you () in
     seq [ source -@ two; any_entity () |+ one ])

let operation_execute =
  procedure "institute:operation_execute" "Operation Execute" Institute
    ~flavor_text:"No process survives a SIGKILL. Neither do we."
    (let* source = anyone ~filter:controlled_by_you () in
     seq [ source -@ three; all_entities () |+ one ])

let containment_override =
  procedure "institute:containment_override" "Containment Override" Institute
    ~flavor_text:
      "SIGKILL -9: reality. The memetic field flatlines. All sectors report \
       nominal."
    (let* source = anyone ~filter:controlled_by_you () in
     seq
       [
         source -@ four;
         secure (to_sector Alpha);
         secure (to_sector Beta);
         secure (to_sector Lambda);
         secure (to_sector Gamma);
       ])

let emergency_containment =
  procedure "institute:emergency_containment" "Emergency Containment" Institute
    ~flavor_text:"Temporary measures for permanent problems."
    (contain entity_here)

let targeted_containment =
  procedure "institute:targeted_containment" "Targeted Containment" Institute
    ~flavor_text:"Focus the suppression field on sector Alpha."
    (contain (entity_in Alpha))

let mass_containment =
  procedure "institute:mass_containment" "Mass Containment" Institute
    ~flavor_text:"When one containment isn't enough."
    (seq [ contain entity_here; contain (any_entity ()) ])

let terse_violation =
  event "test:terse_violation" "Terse Violation" Rust (anyone () -@ three)

let terse_retreat =
  event "test:terse_retreat" "Terse Retreat" Institute
    (seq [ anyone () --> any_sector (); secure (any_sector ()) ])

let terse_overflow =
  entity "test:terse_overflow" "Terse Overflow" Institute ~threat:keter
    ~timer:four ~on_end_phase:(everyone -@ one) ~on_breach:(everyone -@ three)
    ~contained:(personnel_count Alpha 2) ()

let terse_fragmenter =
  entity "test:terse_fragmenter" "Terse Fragmenter" Institute ~threat:titan
    ~timer:Timer.seven
    ~on_end_phase:
      (let* victim = anyone () in
       seq
         [
           victim -@ two;
           victim --> any_sector ();
           entity_in Alpha |+ one;
           breach_sector (any_sector ());
         ])
    ~on_breach:
      (seq
         [
           abyss (anyone ());
           abyss (anyone ());
           abyss (anyone ());
           breach_sector (any_sector ());
         ])
    ~contained:
      (personnel_count Alpha 3 ||? personnel_count Beta 3
      &&? !?(sector_breached Lambda))
    ()

let quantum_specialist =
  personnel "test:quantum_specialist" "Quantum Specialist" Haskell ~cc:CC.five
    [
      passive ~when_:(sector_breached Gamma) (shield everyone_here two);
      activated ~cost:CC.two
        (let* target = anyone_else () in
         seq [ me ==> target; target +@ one; at_end_phase (target ==> me) ]);
      on_deployed_in Lambda ~limit:once_per_round
        (seq [ everyone_in Lambda +@ one; entity_in Lambda |- one ]);
      on_cc_loss ~not_from_spend:true me ~optional:true (me --> any_sector ());
      burnout (seq [ everyone -@ one; secure here ]);
    ]

let personnel_golden_tests =
  [
    ("dsl_operative", dsl_operative);
    ("dsl_composite", dsl_composite_test);
    ("terse_operative", terse_operative);
    ("terse_conditional", terse_conditional);
    ("terse_defender", terse_defender);
    ("terse_temporal", terse_temporal);
    ("terse_responder", terse_responder);
    ("terse_coordinator", terse_coordinator);
    ("terse_memguard", terse_memguard);
    ("terse_card_master", terse_card_master);
    ("quantum_specialist", quantum_specialist);
  ]

let procedure_golden_tests =
  [
    ("dsl_sector_procedure", dsl_sector_procedure);
    ("terse_protocol", terse_protocol);
    ("terse_assault", terse_assault);
    ("sigkill_engagement", sigkill_engagement);
    ("operation_execute", operation_execute);
    ("containment_override", containment_override);
    ("emergency_containment", emergency_containment);
    ("targeted_containment", targeted_containment);
    ("mass_containment", mass_containment);
  ]

let event_golden_tests =
  [ ("terse_violation", terse_violation); ("terse_retreat", terse_retreat) ]

let entity_golden_tests =
  [ ("terse_overflow", terse_overflow); ("terse_fragmenter", terse_fragmenter) ]

let () =
  test_dsl_equivalence ();
  test_condition_equivalence ();

  run_text_golden_tests ~render:TS.personnel_to_string personnel_golden_tests;
  run_text_golden_tests ~render:TS.procedure_to_string procedure_golden_tests;
  run_text_golden_tests ~render:TS.event_to_string event_golden_tests;
  run_text_golden_tests ~render:TS.entity_to_string entity_golden_tests
