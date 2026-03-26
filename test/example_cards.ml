module E = Typefuckery.Engine
module T = Typefuckery.Targets
module Int = Typefuckery.Int
module Condition = Typefuckery.Condition
module R = Typefuckery.Registry
module TS = Typefuckery.To_string.Detailed_English
module TS_Rust = Typefuckery.To_string.Rust
module TS_Haskell = Typefuckery.To_string.Haskell
module TS_Ada = Typefuckery.To_string.Ada
module TS_OCaml = Typefuckery.To_string.OCaml
module J = Typefuckery.To_json
open Typefuckery.Core
open Typefuckery.Cards
open Typefuckery.Abilities
open Util

let borrow_checker_chaplain =
  {
    id = Card_id.of_string "rust:borrow_checker_chaplain";
    name = "Borrow-Checker Chaplain";
    division = Rust_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.three;
    abilities =
      [
        Triggered
          {
            id = None;
            trigger =
              Core
                (When_cc_would_reduce
                   { target = T.this_personnel_sector; exclude_source = None });
            limit = Some Once_per_round;
            optionality = Optional;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:T.this_personnel_sector
                ~amount:Int.Positive.one;
          };
        Activated
          {
            id = None;
            cc_cost = Int.zero;
            condition = None;
            card_effect =
              E.let_ (T.choose_personnel ~filter:T.other_personnel ())
                (fun personnel ->
                  E.composite
                    [
                      E.move_cc ~from:T.this_personnel ~to_:personnel
                        ~amount:Int.Positive.one;
                      E.delayed ~window:End_phase ~scope:This_round
                        ~then_do:
                          (E.move_cc ~from:personnel ~to_:T.this_personnel
                             ~amount:Int.Positive.one);
                    ]);
          };
        Activated
          {
            id = None;
            cc_cost = Int.two;
            condition = None;
            card_effect =
              E.add_breach_marker ~target:T.entity_in_this_sector
                ~amount:Int.Positive.one;
          };
        Burnout
          {
            id = None;
            card_effect =
              E.add_breach_marker ~target:T.entity_in_this_sector
                ~amount:Int.Positive.one;
          };
      ];
  }

let ada_safety_engineer =
  {
    id = Card_id.of_string "ada:ada_safety_engineer";
    name = "Ada Safety Engineer";
    division = Ada_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.four;
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
        Activated
          {
            id = None;
            cc_cost = Int.one;
            condition = None;
            card_effect =
              E.flip_sector ~target:(T.choose_sector ()) ~state:Secure;
          };
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
        Burnout
          {
            id = None;
            card_effect = E.flip_sector ~target:T.this_sector ~state:Secure;
          };
      ];
  }

let haskell_lazy_evaluator =
  {
    id = Card_id.of_string "haskell:haskell_lazy_evaluator";
    name = "Haskell Lazy Evaluator";
    division = Haskell_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.two;
    abilities =
      [
        Passive
          {
            id = None;
            limit = None;
            condition = None;
            card_effect =
              E.add_cc ~target:T.this_personnel ~amount:Int.Positive.one;
          };
        Activated
          {
            id = None;
            cc_cost = Int.zero;
            condition = Some (Condition.personnel_count_in_sector Lambda 2);
            card_effect =
              E.move_personnel ~target:T.this_personnel
                ~to_sector:(T.choose_sector ());
          };
        Triggered
          {
            id = None;
            trigger = Core (When_entity_effect { in_sector = Some Gamma });
            limit = None;
            optionality = Mandatory;
            condition = None;
            card_effect =
              E.add_cc
                ~target:(T.all_personnel_in_sector Gamma)
                ~amount:Int.Positive.one;
          };
        Burnout
          {
            id = None;
            card_effect =
              E.add_breach_marker ~target:T.entity_in_this_sector
                ~amount:Int.Positive.one;
          };
      ];
  }

let ocaml_module_architect =
  {
    id = Card_id.of_string "ocaml:ocaml_module_architect";
    name = "OCaml Module Architect";
    division = OCaml_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.three;
    abilities =
      [
        Passive
          {
            id = None;
            limit = Some Once_per_round;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:T.this_personnel
                ~amount:Int.Positive.one;
          };
        Activated
          {
            id = None;
            cc_cost = Int.one;
            condition = Some (Condition.personnel_count_in_sector Beta 2);
            card_effect =
              E.composite
                [
                  E.move_personnel
                    ~target:(T.choose_personnel ~in_sector:Beta ())
                    ~to_sector:(T.specific_sector Lambda);
                  E.add_cc ~target:T.this_personnel ~amount:Int.Positive.one;
                ];
          };
        Triggered
          {
            id = None;
            trigger = Core (When_entity_effect { in_sector = None });
            limit = None;
            optionality = Mandatory;
            condition = None;
            card_effect = E.log "Recompute module graph";
          };
        Burnout { id = None; card_effect = E.send_to_abyss T.this_personnel };
      ];
  }

let institute_operative =
  {
    id = Card_id.of_string "institute:institute_operative";
    name = "Institute Operative";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.three;
    abilities =
      [
        Passive
          {
            id = None;
            limit = Some Once_per_round;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:T.this_personnel
                ~amount:Int.Positive.one;
          };
        Activated
          {
            id = None;
            cc_cost = Int.one;
            condition = None;
            card_effect =
              E.add_cc ~target:(T.choose_personnel ()) ~amount:Int.Positive.one;
          };
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
        Burnout
          { id = None; card_effect = E.log "File a final incident report" };
      ];
  }

let shift_supervisor =
  {
    id = Card_id.of_string "institute:shift_supervisor";
    name = "Shift Supervisor";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.four;
    abilities =
      [
        Passive
          {
            id = None;
            limit = Some Once_per_round;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:T.this_personnel_sector
                ~amount:Int.Positive.one;
          };
        Activated
          {
            id = None;
            cc_cost = Int.two;
            condition = None;
            card_effect =
              E.flip_sector ~target:(T.choose_sector ()) ~state:Secure;
          };
        Burnout
          {
            id = None;
            card_effect =
              E.remove_breach_marker ~target:(T.choose_entity ())
                ~amount:Int.Positive.one;
          };
      ];
  }

let lifetime_extension : rust core_procedure =
  {
    id = Card_id.of_string "rust:lifetime_extension";
    name = "Lifetime Extension";
    division = Rust_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.add_breach_marker ~target:(T.choose_entity ()) ~amount:Int.Positive.one;
  }

let ownership_transfer : rust core_procedure =
  {
    id = Card_id.of_string "rust:ownership_transfer";
    name = "Ownership Transfer";
    division = Rust_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.move_personnel ~target:(T.choose_personnel ())
        ~to_sector:(T.choose_sector ());
  }

let mutable_borrow : rust core_procedure =
  {
    id = Card_id.of_string "rust:mutable_borrow";
    name = "Mutable Borrow";
    division = Rust_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.if_possible
        (E.move_cc_between_pair
           ~pair:(T.choose_personnel_pair ())
           ~amount:Int.Positive.one);
  }

let rapid_response : institute core_procedure =
  {
    id = Card_id.of_string "institute:rapid_response";
    name = "Rapid Response";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.move_personnel ~target:(T.choose_personnel ())
        ~to_sector:(T.choose_sector ());
  }

let standard_protocol : institute core_procedure =
  {
    id = Card_id.of_string "institute:standard_protocol";
    name = "Standard Protocol";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.composite
        [
          E.add_cc ~target:(T.choose_personnel ()) ~amount:Int.Positive.one;
          E.remove_breach_marker ~target:(T.choose_entity ())
            ~amount:Int.Positive.one;
        ];
  }

let lifetime_violation : rust core_event =
  {
    id = Card_id.of_string "rust:lifetime_violation";
    name = "Lifetime Violation";
    division = Rust_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.remove_cc
        ~target:(T.choose_personnel ~filter:T.personnel_in_play ())
        ~amount:Int.Positive.three;
  }

let borrow_checker_rejection : rust core_event =
  {
    id = Card_id.of_string "rust:borrow_checker_rejection";
    name = "Borrow Checker Rejection";
    division = Rust_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.remove_cc
        ~target:(T.all_personnel_in_sector Alpha)
        ~amount:Int.Positive.one;
  }

let unsafe_code_panic : rust core_event =
  {
    id = Card_id.of_string "rust:unsafe_code_panic";
    name = "Unsafe Code Panic";
    division = Rust_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.add_breach_marker ~target:(T.choose_entity ()) ~amount:Int.Positive.one;
  }

let buffer_overflow =
  {
    id = Card_id.of_string "institute:buffer_overflow";
    name = "Buffer Overflow";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    threat_level = Keter;
    breach_timer = Int.Positive.four;
    end_phase_effect =
      E.remove_cc ~target:T.all_personnel ~amount:Int.Positive.one;
    breach_effect =
      E.remove_cc ~target:T.all_personnel ~amount:Int.Positive.three;
    containment = { check = Condition.personnel_count_in_sector Alpha 2 };
  }

let deadlock_demon =
  {
    id = Card_id.of_string "institute:deadlock_demon";
    name = "Deadlock Demon";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    threat_level = Euclid;
    breach_timer = Int.Positive.three;
    end_phase_effect =
      E.flip_sector ~target:(T.choose_sector ()) ~state:Breached;
    breach_effect = E.send_to_abyss (T.choose_personnel ());
    containment = { check = Condition.personnel_count_in_sector Beta 1 };
  }

let infinite_loop =
  {
    id = Card_id.of_string "institute:infinite_loop";
    name = "Infinite Loop";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    threat_level = Safe;
    breach_timer = Int.Positive.five;
    end_phase_effect =
      E.add_breach_marker ~target:(T.all_entities ()) ~amount:Int.Positive.one;
    breach_effect = E.discard ~player:T.you ~amount:Int.Positive.two;
    containment = { check = Condition.sector_is_breached Gamma };
  }

let memory_leak =
  {
    id = Card_id.of_string "institute:memory_leak";
    name = "Memory Leak";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    threat_level = Euclid;
    breach_timer = Int.Positive.four;
    end_phase_effect =
      E.remove_cc
        ~target:(T.all_personnel_in_sector Lambda)
        ~amount:Int.Positive.one;
    breach_effect = E.remove_cc ~target:T.all_personnel ~amount:Int.Positive.two;
    containment =
      {
        check =
          Condition.personnel_with_min_cc Lambda ~min_count:2 ~min_cc_each:1;
      };
  }

let null_pointer =
  {
    id = Card_id.of_string "institute:null_pointer";
    name = "Null Pointer";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    threat_level = Keter;
    breach_timer = Int.Positive.three;
    end_phase_effect = E.send_to_abyss (T.choose_personnel ());
    breach_effect =
      E.composite
        [
          E.send_to_abyss (T.choose_personnel ());
          E.send_to_abyss (T.choose_personnel ());
        ];
    containment = { check = Condition.always };
  }

let race_hazard =
  {
    id = Card_id.of_string "institute:race_hazard";
    name = "Race Hazard";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    threat_level = Titan;
    breach_timer = Int.Positive.six;
    end_phase_effect =
      E.move_cc ~from:(T.choose_personnel ()) ~to_:(T.choose_personnel ())
        ~amount:Int.Positive.one;
    breach_effect =
      E.remove_cc ~target:T.all_personnel ~amount:Int.Positive.five;
    containment =
      {
        check =
          Condition.or_
            (Condition.sector_is_breached Alpha)
            (Condition.sector_is_breached Beta);
      };
  }

let syntax_glitch =
  {
    id = Card_id.of_string "institute:syntax_glitch";
    name = "Syntax Glitch";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    threat_level = Safe;
    breach_timer = Int.Positive.one;
    end_phase_effect = E.noop;
    breach_effect = E.discard ~player:T.you ~amount:Int.Positive.one;
    containment = { check = Condition.always };
  }

let heisenbug =
  {
    id = Card_id.of_string "institute:heisenbug";
    name = "Heisenbug";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    threat_level = Euclid;
    breach_timer = Int.Positive.two;
    end_phase_effect =
      E.composite
        [
          E.remove_cc ~target:T.all_personnel_in_this_sector
            ~amount:Int.Positive.one;
          E.add_breach_marker ~target:T.entity_in_this_sector
            ~amount:Int.Positive.one;
        ];
    breach_effect = E.remove_cc ~target:T.all_personnel ~amount:Int.Positive.two;
    containment = { check = Condition.personnel_count_in_sector Alpha 1 };
  }

let memguard =
  {
    id = Card_id.of_string "rust:memguard";
    name = "Memguard";
    division = Rust_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.four;
    abilities =
      [
        Passive
          {
            id = None;
            limit = Some Once_per_round;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:T.this_personnel
                ~amount:Int.Positive.one;
          };
        Triggered
          {
            id = None;
            trigger = E.when_cc_would_reduce_not_from_spend T.this_personnel;
            limit = Some Once_per_round;
            optionality = Optional;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:T.this_personnel
                ~amount:Int.Positive.two;
          };
        Activated
          {
            id = None;
            cc_cost = Int.three;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:(T.choose_personnel ())
                ~amount:Int.Positive.one;
          };
        Burnout
          {
            id = None;
            card_effect =
              E.prevent_cc_loss ~target:T.this_personnel_sector
                ~amount:Int.Positive.one;
          };
      ];
  }

let temporary_boost : rust core_procedure =
  {
    id = Card_id.of_string "rust:temporary_boost";
    name = "Temporary Boost";
    division = Rust_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.let_ (T.choose_personnel ()) (fun personnel ->
          E.composite
            [
              E.add_cc ~target:personnel ~amount:Int.Positive.one;
              E.before_end_phase_step_1_this_round
                (E.remove_cc ~target:personnel ~amount:Int.Positive.one);
            ]);
  }

let forced_knowledge : institute core_procedure =
  {
    id = Card_id.of_string "institute:forced_knowledge";
    name = "Forced Knowledge";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    card_effect = E.draw ~player:T.another_player ~amount:Int.Positive.three;
  }

let e_acc =
  {
    id = Card_id.of_string "institute:e_acc";
    name = "e/acc";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    starting_cc = Int.four;
    abilities =
      [
        Activated
          {
            id = None;
            cc_cost = Int.one;
            condition = None;
            card_effect =
              E.composite
                [
                  E.remove_breach_marker ~target:T.entity_in_this_sector
                    ~amount:Int.Positive.one;
                  E.add_cc ~target:T.this_personnel_sector
                    ~amount:Int.Positive.one;
                ];
          };
        Activated
          {
            id = None;
            cc_cost = Int.two;
            condition = None;
            card_effect =
              E.add_cc ~target:T.all_personnel ~amount:Int.Positive.one;
          };
        Burnout
          {
            id = None;
            card_effect = E.draw ~player:T.you ~amount:Int.Positive.one;
          };
      ];
  }

let strategic_retreat =
  {
    id = Card_id.of_string "institute:strategic_retreat";
    name = "Strategic Retreat";
    division = Institute_div;
    lore = None;
    flavor_text = None;
    card_effect =
      E.composite
        [
          E.move_personnel
            ~target:(T.choose_personnel ~chooser:Starting_player ())
            ~to_sector:(T.choose_sector ());
        ];
  }

let () =
  let cards =
    [
      pack_core_card (Personnel borrow_checker_chaplain);
      pack_core_card (Personnel ada_safety_engineer);
      pack_core_card (Personnel haskell_lazy_evaluator);
      pack_core_card (Personnel ocaml_module_architect);
      pack_core_card (Personnel institute_operative);
      pack_core_card (Personnel shift_supervisor);
      pack_core_card (Procedure lifetime_extension);
      pack_core_card (Procedure ownership_transfer);
      pack_core_card (Procedure mutable_borrow);
      pack_core_card (Procedure rapid_response);
      pack_core_card (Procedure standard_protocol);
      pack_core_card (Event lifetime_violation);
      pack_core_card (Event borrow_checker_rejection);
      pack_core_card (Event unsafe_code_panic);
      pack_core_card (Entity buffer_overflow);
      pack_core_card (Entity deadlock_demon);
      pack_core_card (Entity infinite_loop);
      pack_core_card (Entity memory_leak);
      pack_core_card (Entity null_pointer);
      pack_core_card (Entity race_hazard);
      pack_core_card (Entity syntax_glitch);
      pack_core_card (Entity heisenbug);
      pack_core_card (Personnel memguard);
      pack_core_card (Procedure temporary_boost);
      pack_core_card (Procedure forced_knowledge);
      pack_core_card (Personnel e_acc);
      pack_core_card (Event strategic_retreat);
    ]
  in

  let registry =
    match
      R.register_core_division R.empty ~id:"example-phase5a"
        ~name:"Example Phase 5a Cards" ~cards
    with
    | Ok r -> r
    | Error _ ->
        failwith "Expected example Phase 5a set registration to succeed"
  in

  let rendered_by_id =
    R.list_cards registry
    |> List.map (fun entry ->
        (Card_id.to_string entry.R.card_id, entry.R.rendered_text))
  in

  let find_rendered card_id =
    match List.assoc_opt card_id rendered_by_id with
    | Some text -> text
    | None ->
        failwith
          (Printf.sprintf "Expected card %s to exist in registry" card_id)
  in

  let golden_files =
    [
      ("entity_buffer_overflow", "institute:buffer_overflow");
      ("entity_deadlock_demon", "institute:deadlock_demon");
      ("entity_infinite_loop", "institute:infinite_loop");
      ("entity_memory_leak", "institute:memory_leak");
      ("entity_null_pointer", "institute:null_pointer");
      ("entity_race_hazard", "institute:race_hazard");
      ("entity_syntax_glitch", "institute:syntax_glitch");
      ("entity_heisenbug", "institute:heisenbug");
      ("personnel_borrow_checker_chaplain", "rust:borrow_checker_chaplain");
      ("personnel_ada_safety_engineer", "ada:ada_safety_engineer");
      ("personnel_haskell_lazy_evaluator", "haskell:haskell_lazy_evaluator");
      ("personnel_ocaml_module_architect", "ocaml:ocaml_module_architect");
      ("personnel_institute_operative", "institute:institute_operative");
      ("personnel_shift_supervisor", "institute:shift_supervisor");
      ("event_borrow_checker_rejection", "rust:borrow_checker_rejection");
      ("procedure_lifetime_extension", "rust:lifetime_extension");
      ("event_lifetime_violation", "rust:lifetime_violation");
      ("procedure_mutable_borrow", "rust:mutable_borrow");
      ("procedure_ownership_transfer", "rust:ownership_transfer");
      ("procedure_rapid_response", "institute:rapid_response");
      ("procedure_standard_protocol", "institute:standard_protocol");
      ("event_unsafe_code_panic", "rust:unsafe_code_panic");
      ("personnel_memguard", "rust:memguard");
      ("procedure_temporary_boost", "rust:temporary_boost");
      ("procedure_forced_knowledge", "institute:forced_knowledge");
      ("personnel_e_acc", "institute:e_acc");
      ("event_strategic_retreat", "institute:strategic_retreat");
    ]
  in

  run_text_golden_tests
    ~render:(fun card_id -> find_rendered card_id)
    golden_files;

  let personnel_golden_tests =
    [
      ( pack_core_personnel borrow_checker_chaplain,
        "personnel_borrow_checker_chaplain" );
      (pack_core_personnel ada_safety_engineer, "personnel_ada_safety_engineer");
      ( pack_core_personnel haskell_lazy_evaluator,
        "personnel_haskell_lazy_evaluator" );
      ( pack_core_personnel ocaml_module_architect,
        "personnel_ocaml_module_architect" );
      (pack_core_personnel institute_operative, "personnel_institute_operative");
      (pack_core_personnel shift_supervisor, "personnel_shift_supervisor");
      (pack_core_personnel memguard, "personnel_memguard");
    ]
  in

  List.iter
    (fun (Any_core_personnel p, golden_filename) ->
      let actual_full = TS.personnel_to_string p in
      let expected_full = load_golden_file golden_filename in
      assert_true
        (actual_full = expected_full)
        (Printf.sprintf
           "Personnel golden test match: %s\nExpected:\n%s\nActual:\n%s"
           (Card_id.to_string p.id) expected_full actual_full);
      List.iter
        (fun ability ->
          let ability_str = TS.ability_to_string ability in
          assert_true
            (contains_substring ~haystack:actual_full ~needle:ability_str)
            (Printf.sprintf "Personnel contains derived ability: %s (%s)"
               (Card_id.to_string p.id) ability_str))
        p.abilities)
    personnel_golden_tests;

  let procedure_golden_tests =
    [
      (pack_core_procedure lifetime_extension, "procedure_lifetime_extension");
      (pack_core_procedure ownership_transfer, "procedure_ownership_transfer");
      (pack_core_procedure mutable_borrow, "procedure_mutable_borrow");
      (pack_core_procedure rapid_response, "procedure_rapid_response");
      (pack_core_procedure standard_protocol, "procedure_standard_protocol");
    ]
  in

  List.iter
    (fun (Any_core_procedure proc, golden_filename) ->
      let actual_full = TS.procedure_to_string proc in
      let expected_full = load_golden_file golden_filename in
      let effect_str = TS.card_effect_to_string proc.card_effect in
      assert_true
        (actual_full = expected_full)
        (Printf.sprintf
           "Procedure golden test match: %s\nExpected:\n%s\nActual:\n%s"
           (Card_id.to_string proc.id)
           expected_full actual_full);
      assert_true
        (contains_substring ~haystack:actual_full ~needle:effect_str)
        (Printf.sprintf "Procedure contains derived effect: %s"
           (Card_id.to_string proc.id)))
    procedure_golden_tests;

  let event_golden_tests =
    [
      (pack_core_event lifetime_violation, "event_lifetime_violation");
      ( pack_core_event borrow_checker_rejection,
        "event_borrow_checker_rejection" );
      (pack_core_event unsafe_code_panic, "event_unsafe_code_panic");
    ]
  in

  List.iter
    (fun (Any_core_event evt, golden_filename) ->
      let actual_full = TS.event_to_string evt in
      let expected_full = load_golden_file golden_filename in
      let effect_str = TS.card_effect_to_string evt.card_effect in
      assert_true
        (actual_full = expected_full)
        (Printf.sprintf
           "Event golden test match: %s\nExpected:\n%s\nActual:\n%s"
           (Card_id.to_string evt.id) expected_full actual_full);
      assert_true
        (contains_substring ~haystack:actual_full ~needle:effect_str)
        (Printf.sprintf "Event contains derived effect: %s"
           (Card_id.to_string evt.id)))
    event_golden_tests;

  let entity_golden_tests =
    [
      (pack_core_entity buffer_overflow, "entity_buffer_overflow");
      (pack_core_entity deadlock_demon, "entity_deadlock_demon");
      (pack_core_entity infinite_loop, "entity_infinite_loop");
      (pack_core_entity memory_leak, "entity_memory_leak");
      (pack_core_entity null_pointer, "entity_null_pointer");
      (pack_core_entity race_hazard, "entity_race_hazard");
      (pack_core_entity syntax_glitch, "entity_syntax_glitch");
    ]
  in

  List.iter
    (fun (Any_core_entity ent, golden_filename) ->
      let actual_full = TS.entity_to_string ent in
      let expected_full = load_golden_file golden_filename in
      let effect_str = TS.card_effect_to_string ent.end_phase_effect in
      let containment_header = "Containment Requirement:" in
      let requirement_desc =
        TS.containment_requirement_to_string ent.containment
      in
      assert_true
        (actual_full = expected_full)
        (Printf.sprintf
           "Entity golden test match: %s\nExpected:\n%s\nActual:\n%s"
           (Card_id.to_string ent.id) expected_full actual_full);
      assert_true
        (contains_substring ~haystack:actual_full ~needle:effect_str)
        (Printf.sprintf "Entity contains derived effect: %s"
           (Card_id.to_string ent.id));
      assert_true
        (contains_substring ~haystack:actual_full ~needle:containment_header)
        (Printf.sprintf "Entity contains derived containment header: %s"
           (Card_id.to_string ent.id));
      assert_true
        (contains_substring ~haystack:actual_full ~needle:requirement_desc)
        (Printf.sprintf "Entity contains containment requirement: %s"
           (Card_id.to_string ent.id)))
    entity_golden_tests;

  let json_golden_files =
    [
      ( "json_personnel_borrow_checker_chaplain",
        pack_core_card (Personnel borrow_checker_chaplain) );
      ( "json_personnel_ada_safety_engineer",
        pack_core_card (Personnel ada_safety_engineer) );
      ( "json_personnel_haskell_lazy_evaluator",
        pack_core_card (Personnel haskell_lazy_evaluator) );
      ( "json_personnel_ocaml_module_architect",
        pack_core_card (Personnel ocaml_module_architect) );
      ( "json_personnel_institute_operative",
        pack_core_card (Personnel institute_operative) );
      ( "json_personnel_shift_supervisor",
        pack_core_card (Personnel shift_supervisor) );
      ("json_personnel_memguard", pack_core_card (Personnel memguard));
      ("json_personnel_e_acc", pack_core_card (Personnel e_acc));
      ( "json_procedure_lifetime_extension",
        pack_core_card (Procedure lifetime_extension) );
      ( "json_procedure_mutable_borrow",
        pack_core_card (Procedure mutable_borrow) );
      ( "json_procedure_ownership_transfer",
        pack_core_card (Procedure ownership_transfer) );
      ( "json_procedure_rapid_response",
        pack_core_card (Procedure rapid_response) );
      ( "json_procedure_standard_protocol",
        pack_core_card (Procedure standard_protocol) );
      ( "json_procedure_temporary_boost",
        pack_core_card (Procedure temporary_boost) );
      ( "json_procedure_forced_knowledge",
        pack_core_card (Procedure forced_knowledge) );
      ( "json_event_borrow_checker_rejection",
        pack_core_card (Event borrow_checker_rejection) );
      ( "json_event_lifetime_violation",
        pack_core_card (Event lifetime_violation) );
      ("json_event_unsafe_code_panic", pack_core_card (Event unsafe_code_panic));
      ("json_event_strategic_retreat", pack_core_card (Event strategic_retreat));
      ("json_entity_buffer_overflow", pack_core_card (Entity buffer_overflow));
      ("json_entity_deadlock_demon", pack_core_card (Entity deadlock_demon));
      ("json_entity_infinite_loop", pack_core_card (Entity infinite_loop));
      ("json_entity_memory_leak", pack_core_card (Entity memory_leak));
      ("json_entity_null_pointer", pack_core_card (Entity null_pointer));
      ("json_entity_race_hazard", pack_core_card (Entity race_hazard));
      ("json_entity_syntax_glitch", pack_core_card (Entity syntax_glitch));
      ("json_entity_heisenbug", pack_core_card (Entity heisenbug));
    ]
  in

  run_json_golden_tests
    ~render:(fun card -> J.json_to_string (J.any_core_card_to_json card))
    json_golden_files;

  let language_json_items =
    [
      ( "json_rust_personnel_borrow_checker_chaplain",
        pack_core_card (Personnel borrow_checker_chaplain) );
      ( "json_rust_procedure_lifetime_extension",
        pack_core_card (Procedure lifetime_extension) );
      ( "json_ada_personnel_ada_safety_engineer",
        pack_core_card (Personnel ada_safety_engineer) );
      ( "json_haskell_personnel_haskell_lazy_evaluator",
        pack_core_card (Personnel haskell_lazy_evaluator) );
      ( "json_ocaml_personnel_ocaml_module_architect",
        pack_core_card (Personnel ocaml_module_architect) );
      ( "json_institute_entity_buffer_overflow",
        pack_core_card (Entity buffer_overflow) );
    ]
  in

  let run_language_json_tests ~(render : any_core_card -> string) ~prefix
      (items : (string * any_core_card) list) =
    let prefixed_items =
      List.map (fun (name, card) -> (prefix ^ "_" ^ name, card)) items
    in
    run_golden_tests
      ~load:(fun f -> load_file ~ext:".json" f)
      ~write:(fun f c -> write_file ~ext:".json" f c)
      ~render ~label:"Language JSON" prefixed_items
  in

  run_language_json_tests ~render:TS_Rust.any_core_card_to_string ~prefix:"rust"
    language_json_items;

  run_language_json_tests ~render:TS_Haskell.any_core_card_to_string
    ~prefix:"haskell" language_json_items;

  run_language_json_tests ~render:TS_Ada.any_core_card_to_string ~prefix:"ada"
    language_json_items;

  run_language_json_tests ~render:TS_OCaml.any_core_card_to_string
    ~prefix:"ocaml" language_json_items;

  let language_test_items =
    [
      ( "personnel_borrow_checker_chaplain",
        Any_core_card (Personnel borrow_checker_chaplain) );
      ( "procedure_lifetime_extension",
        Any_core_card (Procedure lifetime_extension) );
      ( "personnel_ada_safety_engineer",
        Any_core_card (Personnel ada_safety_engineer) );
      ( "personnel_haskell_lazy_evaluator",
        Any_core_card (Personnel haskell_lazy_evaluator) );
      ( "personnel_ocaml_module_architect",
        Any_core_card (Personnel ocaml_module_architect) );
      ("entity_buffer_overflow", Any_core_card (Entity buffer_overflow));
    ]
  in

  let run_language_golden_tests ~(render : any_core_card -> string) ~prefix
      (items : (string * any_core_card) list) =
    let labeled_items =
      List.map (fun (name, card) -> (prefix ^ "_" ^ name, card)) items
    in
    run_text_golden_tests ~render labeled_items
  in

  run_language_golden_tests ~render:TS_Rust.any_core_card_to_string
    ~prefix:"rust" language_test_items;

  run_language_golden_tests ~render:TS_Haskell.any_core_card_to_string
    ~prefix:"haskell" language_test_items;

  run_language_golden_tests ~render:TS_Ada.any_core_card_to_string ~prefix:"ada"
    language_test_items;

  run_language_golden_tests ~render:TS_OCaml.any_core_card_to_string
    ~prefix:"ocaml" language_test_items
