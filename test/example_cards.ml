module E = Typefuckery.Engine
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

let borrow_checker_chaplain : core_personnel =
  {
    id = Card_id.of_string "rust:borrow_checker_chaplain";
    name = "Borrow-Checker Chaplain";
    division = Rust;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.three;
    abilities =
      [
        Triggered
          {
            id = None;
            trigger =
              Core
                (When_cc_would_reduce
                   { target = E.this_personnel_sector; exclude_source = None });
            limit = Some Once_per_round;
            optionality = Optional;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:E.this_personnel_sector
                ~amount:E.Amount.one;
          };
        Activated
          {
            id = None;
            cc_cost = E.CC.zero;
            condition = None;
            card_effect =
              E.let_ "Personnel"
                (E.choose_personnel ~filter:E.other_personnel ())
                (fun personnel ->
                  E.composite
                    [
                      E.move_cc ~from:E.this_personnel ~to_:personnel
                        ~amount:E.Amount.one;
                      E.delayed ~window:End_phase ~scope:This_round
                        ~then_do:
                          (E.move_cc ~from:personnel ~to_:E.this_personnel
                             ~amount:E.Amount.one);
                    ]);
          };
        Activated
          {
            id = None;
            cc_cost = E.CC.two;
            condition = None;
            card_effect =
              E.add_breach_marker ~target:E.entity_in_this_sector
                ~amount:E.Amount.one;
          };
        Burnout
          {
            id = None;
            card_effect =
              E.add_breach_marker ~target:E.entity_in_this_sector
                ~amount:E.Amount.one;
          };
      ];
  }

let ada_safety_engineer : core_personnel =
  {
    id = Card_id.of_string "ada:ada_safety_engineer";
    name = "Ada Safety Engineer";
    division = Ada;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.four;
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
        Activated
          {
            id = None;
            cc_cost = E.CC.one;
            condition = None;
            card_effect =
              E.flip_sector ~target:(E.choose_sector ()) ~state:Secure;
          };
        Triggered
          {
            id = None;
            trigger = Core When_deployed;
            limit = None;
            optionality = Mandatory;
            condition = None;
            card_effect = E.add_cc ~target:E.this_personnel ~amount:E.Amount.one;
          };
        Burnout
          {
            id = None;
            card_effect = E.flip_sector ~target:E.this_sector ~state:Secure;
          };
      ];
  }

let haskell_lazy_evaluator : core_personnel =
  {
    id = Card_id.of_string "haskell:haskell_lazy_evaluator";
    name = "Haskell Lazy Evaluator";
    division = Haskell;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.two;
    abilities =
      [
        Passive
          {
            id = None;
            limit = None;
            condition = None;
            card_effect = E.add_cc ~target:E.this_personnel ~amount:E.Amount.one;
          };
        Activated
          {
            id = None;
            cc_cost = E.CC.zero;
            condition = Some (E.personnel_count_in_sector Lambda 2);
            card_effect =
              E.move_personnel ~target:E.this_personnel
                ~to_sector:(E.choose_sector ());
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
                ~target:(E.all_personnel_in_sector Gamma)
                ~amount:E.Amount.one;
          };
        Burnout
          {
            id = None;
            card_effect =
              E.add_breach_marker ~target:E.entity_in_this_sector
                ~amount:E.Amount.one;
          };
      ];
  }

let ocaml_module_architect : core_personnel =
  {
    id = Card_id.of_string "ocaml:ocaml_module_architect";
    name = "OCaml Module Architect";
    division = OCaml;
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
            cc_cost = E.CC.one;
            condition = Some (E.personnel_count_in_sector Beta 2);
            card_effect =
              E.composite
                [
                  E.move_personnel
                    ~target:(E.choose_personnel ~in_sector:Beta ())
                    ~to_sector:(E.specific_sector Lambda);
                  E.add_cc ~target:E.this_personnel ~amount:E.Amount.one;
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
        Burnout { id = None; card_effect = E.send_to_abyss E.this_personnel };
      ];
  }

let institute_operative : core_personnel =
  {
    id = Card_id.of_string "institute:institute_operative";
    name = "Institute Operative";
    division = Institute;
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
            cc_cost = E.CC.one;
            condition = None;
            card_effect =
              E.add_cc ~target:(E.choose_personnel ()) ~amount:E.Amount.one;
          };
        Triggered
          {
            id = None;
            trigger = Core When_deployed;
            limit = None;
            optionality = Mandatory;
            condition = None;
            card_effect = E.add_cc ~target:E.this_personnel ~amount:E.Amount.one;
          };
        Burnout
          { id = None; card_effect = E.log "File a final incident report" };
      ];
  }

let shift_supervisor : core_personnel =
  {
    id = Card_id.of_string "institute:shift_supervisor";
    name = "Shift Supervisor";
    division = Institute;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.four;
    abilities =
      [
        Passive
          {
            id = None;
            limit = Some Once_per_round;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:E.this_personnel_sector
                ~amount:E.Amount.one;
          };
        Activated
          {
            id = None;
            cc_cost = E.CC.two;
            condition = None;
            card_effect =
              E.flip_sector ~target:(E.choose_sector ()) ~state:Secure;
          };
        Burnout
          {
            id = None;
            card_effect =
              E.remove_breach_marker ~target:(E.choose_entity ())
                ~amount:E.Amount.one;
          };
      ];
  }

let lifetime_extension : core_procedure =
  {
    id = Card_id.of_string "rust:lifetime_extension";
    name = "Lifetime Extension";
    division = Rust;
    lore = None;
    flavor_text = None;
    card_effect =
      E.add_breach_marker ~target:(E.choose_entity ()) ~amount:E.Amount.one;
  }

let ownership_transfer : core_procedure =
  {
    id = Card_id.of_string "rust:ownership_transfer";
    name = "Ownership Transfer";
    division = Rust;
    lore = None;
    flavor_text = None;
    card_effect =
      E.move_personnel ~target:(E.choose_personnel ())
        ~to_sector:(E.choose_sector ());
  }

let mutable_borrow : core_procedure =
  {
    id = Card_id.of_string "rust:mutable_borrow";
    name = "Mutable Borrow";
    division = Rust;
    lore = None;
    flavor_text = None;
    card_effect =
      E.if_possible
        (E.move_cc_between_pair
           ~pair:(E.choose_personnel_pair ())
           ~amount:E.Amount.one);
  }

let rapid_response : core_procedure =
  {
    id = Card_id.of_string "institute:rapid_response";
    name = "Rapid Response";
    division = Institute;
    lore = None;
    flavor_text = None;
    card_effect =
      E.move_personnel ~target:(E.choose_personnel ())
        ~to_sector:(E.choose_sector ());
  }

let standard_protocol : core_procedure =
  {
    id = Card_id.of_string "institute:standard_protocol";
    name = "Standard Protocol";
    division = Institute;
    lore = None;
    flavor_text = None;
    card_effect =
      E.composite
        [
          E.add_cc ~target:(E.choose_personnel ()) ~amount:E.Amount.one;
          E.remove_breach_marker ~target:(E.choose_entity ())
            ~amount:E.Amount.one;
        ];
  }

let lifetime_violation : core_event =
  {
    id = Card_id.of_string "rust:lifetime_violation";
    name = "Lifetime Violation";
    division = Rust;
    lore = None;
    flavor_text = None;
    card_effect =
      E.remove_cc
        ~target:(E.choose_personnel ~filter:E.personnel_in_play ())
        ~amount:E.Amount.three;
  }

let borrow_checker_rejection : core_event =
  {
    id = Card_id.of_string "rust:borrow_checker_rejection";
    name = "Borrow Checker Rejection";
    division = Rust;
    lore = None;
    flavor_text = None;
    card_effect =
      E.remove_cc ~target:(E.all_personnel_in_sector Alpha) ~amount:E.Amount.one;
  }

let unsafe_code_panic : core_event =
  {
    id = Card_id.of_string "rust:unsafe_code_panic";
    name = "Unsafe Code Panic";
    division = Rust;
    lore = None;
    flavor_text = None;
    card_effect =
      E.add_breach_marker ~target:(E.choose_entity ()) ~amount:E.Amount.one;
  }

let buffer_overflow : core_entity =
  {
    id = Card_id.of_string "institute:buffer_overflow";
    name = "Buffer Overflow";
    division = Institute;
    lore = None;
    flavor_text = None;
    threat_level = Keter;
    breach_timer = E.Timer.four;
    end_phase_effect = E.remove_cc ~target:E.all_personnel ~amount:E.Amount.one;
    breach_effect = E.remove_cc ~target:E.all_personnel ~amount:E.Amount.three;
    containment = { check = E.personnel_count_in_sector Alpha 2 };
  }

let deadlock_demon : core_entity =
  {
    id = Card_id.of_string "institute:deadlock_demon";
    name = "Deadlock Demon";
    division = Institute;
    lore = None;
    flavor_text = None;
    threat_level = Euclid;
    breach_timer = E.Timer.three;
    end_phase_effect =
      E.flip_sector ~target:(E.choose_sector ()) ~state:Breached;
    breach_effect = E.send_to_abyss (E.choose_personnel ());
    containment = { check = E.personnel_count_in_sector Beta 1 };
  }

let infinite_loop : core_entity =
  {
    id = Card_id.of_string "institute:infinite_loop";
    name = "Infinite Loop";
    division = Institute;
    lore = None;
    flavor_text = None;
    threat_level = Safe;
    breach_timer = E.Timer.five;
    end_phase_effect =
      E.add_breach_marker ~target:(E.all_entities ()) ~amount:E.Amount.one;
    breach_effect = E.discard ~player:E.you ~amount:E.Amount.two;
    containment = { check = E.sector_is_breached Gamma };
  }

let memory_leak : core_entity =
  {
    id = Card_id.of_string "institute:memory_leak";
    name = "Memory Leak";
    division = Institute;
    lore = None;
    flavor_text = None;
    threat_level = Euclid;
    breach_timer = E.Timer.four;
    end_phase_effect =
      E.remove_cc
        ~target:(E.all_personnel_in_sector Lambda)
        ~amount:E.Amount.one;
    breach_effect = E.remove_cc ~target:E.all_personnel ~amount:E.Amount.two;
    containment =
      { check = E.personnel_with_min_cc Lambda ~min_count:2 ~min_cc_each:1 };
  }

let null_pointer : core_entity =
  {
    id = Card_id.of_string "institute:null_pointer";
    name = "Null Pointer";
    division = Institute;
    lore = None;
    flavor_text = None;
    threat_level = Keter;
    breach_timer = E.Timer.three;
    end_phase_effect = E.send_to_abyss (E.choose_personnel ());
    breach_effect =
      E.composite
        [
          E.send_to_abyss (E.choose_personnel ());
          E.send_to_abyss (E.choose_personnel ());
        ];
    containment = { check = E.always };
  }

let race_hazard : core_entity =
  {
    id = Card_id.of_string "institute:race_hazard";
    name = "Race Hazard";
    division = Institute;
    lore = None;
    flavor_text = None;
    threat_level = Titan;
    breach_timer = E.Timer.six;
    end_phase_effect =
      E.move_cc ~from:(E.choose_personnel ()) ~to_:(E.choose_personnel ())
        ~amount:E.Amount.one;
    breach_effect = E.remove_cc ~target:E.all_personnel ~amount:E.Amount.five;
    containment =
      { check = E.or_ (E.sector_is_breached Alpha) (E.sector_is_breached Beta) };
  }

let syntax_glitch : core_entity =
  {
    id = Card_id.of_string "institute:syntax_glitch";
    name = "Syntax Glitch";
    division = Institute;
    lore = None;
    flavor_text = None;
    threat_level = Safe;
    breach_timer = E.Timer.one;
    end_phase_effect = E.noop;
    breach_effect = E.discard ~player:E.you ~amount:E.Amount.one;
    containment = { check = E.always };
  }

let heisenbug : core_entity =
  {
    id = Card_id.of_string "institute:heisenbug";
    name = "Heisenbug";
    division = Institute;
    lore = None;
    flavor_text = None;
    threat_level = Euclid;
    breach_timer = E.Timer.two;
    end_phase_effect =
      E.composite
        [
          E.remove_cc ~target:E.all_personnel_in_this_sector
            ~amount:E.Amount.one;
          E.add_breach_marker ~target:E.entity_in_this_sector
            ~amount:E.Amount.one;
        ];
    breach_effect = E.remove_cc ~target:E.all_personnel ~amount:E.Amount.two;
    containment = { check = E.personnel_count_in_sector Alpha 1 };
  }

let memguard : core_personnel =
  {
    id = Card_id.of_string "rust:memguard";
    name = "Memguard";
    division = Rust;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.four;
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
        Triggered
          {
            id = None;
            trigger = E.when_cc_would_reduce_not_from_spend E.this_personnel;
            limit = Some Once_per_round;
            optionality = Optional;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:E.this_personnel ~amount:E.Amount.two;
          };
        Activated
          {
            id = None;
            cc_cost = E.CC.three;
            condition = None;
            card_effect =
              E.prevent_cc_loss ~target:(E.choose_personnel ())
                ~amount:E.Amount.one;
          };
        Burnout
          {
            id = None;
            card_effect =
              E.prevent_cc_loss ~target:E.this_personnel_sector
                ~amount:E.Amount.one;
          };
      ];
  }

let temporary_boost : core_procedure =
  {
    id = Card_id.of_string "rust:temporary_boost";
    name = "Temporary Boost";
    division = Rust;
    lore = None;
    flavor_text = None;
    card_effect =
      E.let_ "Personnel" (E.choose_personnel ()) (fun personnel ->
          E.composite
            [
              E.add_cc ~target:personnel ~amount:E.Amount.one;
              E.before_end_phase_step_1_this_round
                (E.remove_cc ~target:personnel ~amount:E.Amount.one);
            ]);
  }

let forced_knowledge : core_procedure =
  {
    id = Card_id.of_string "institute:forced_knowledge";
    name = "Forced Knowledge";
    division = Institute;
    lore = None;
    flavor_text = None;
    card_effect = E.draw ~player:E.another_player ~amount:E.Amount.three;
  }

let e_acc : core_personnel =
  {
    id = Card_id.of_string "institute:e_acc";
    name = "e/acc";
    division = Institute;
    lore = None;
    flavor_text = None;
    starting_cc = E.CC.four;
    abilities =
      [
        Activated
          {
            id = None;
            cc_cost = E.CC.one;
            condition = None;
            card_effect =
              E.composite
                [
                  E.remove_breach_marker ~target:E.entity_in_this_sector
                    ~amount:E.Amount.one;
                  E.add_cc ~target:E.this_personnel_sector ~amount:E.Amount.one;
                ];
          };
        Activated
          {
            id = None;
            cc_cost = E.CC.two;
            condition = None;
            card_effect = E.add_cc ~target:E.all_personnel ~amount:E.Amount.one;
          };
        Burnout
          { id = None; card_effect = E.draw ~player:E.you ~amount:E.Amount.one };
      ];
  }

let strategic_retreat : core_event =
  {
    id = Card_id.of_string "institute:strategic_retreat";
    name = "Strategic Retreat";
    division = Institute;
    lore = None;
    flavor_text = None;
    card_effect =
      E.composite
        [
          E.move_personnel
            ~target:(E.choose_personnel ~chooser:(Some Starting_player) ())
            ~to_sector:(E.choose_sector ());
        ];
  }

let () =
  let cards : core_card list =
    [
      Personnel borrow_checker_chaplain;
      Personnel ada_safety_engineer;
      Personnel haskell_lazy_evaluator;
      Personnel ocaml_module_architect;
      Personnel institute_operative;
      Personnel shift_supervisor;
      Procedure lifetime_extension;
      Procedure ownership_transfer;
      Procedure mutable_borrow;
      Procedure rapid_response;
      Procedure standard_protocol;
      Event lifetime_violation;
      Event borrow_checker_rejection;
      Event unsafe_code_panic;
      Entity buffer_overflow;
      Entity deadlock_demon;
      Entity infinite_loop;
      Entity memory_leak;
      Entity null_pointer;
      Entity race_hazard;
      Entity syntax_glitch;
      Entity heisenbug;
      Personnel memguard;
      Procedure temporary_boost;
      Procedure forced_knowledge;
      Personnel e_acc;
      Event strategic_retreat;
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
      (borrow_checker_chaplain, "personnel_borrow_checker_chaplain");
      (ada_safety_engineer, "personnel_ada_safety_engineer");
      (haskell_lazy_evaluator, "personnel_haskell_lazy_evaluator");
      (ocaml_module_architect, "personnel_ocaml_module_architect");
      (institute_operative, "personnel_institute_operative");
      (shift_supervisor, "personnel_shift_supervisor");
      (memguard, "personnel_memguard");
    ]
  in

  List.iter
    (fun (p, golden_filename) ->
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
      (lifetime_extension, "procedure_lifetime_extension");
      (ownership_transfer, "procedure_ownership_transfer");
      (mutable_borrow, "procedure_mutable_borrow");
      (rapid_response, "procedure_rapid_response");
      (standard_protocol, "procedure_standard_protocol");
    ]
  in

  List.iter
    (fun (proc, golden_filename) ->
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
      (lifetime_violation, "event_lifetime_violation");
      (borrow_checker_rejection, "event_borrow_checker_rejection");
      (unsafe_code_panic, "event_unsafe_code_panic");
    ]
  in

  List.iter
    (fun (evt, golden_filename) ->
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
      (buffer_overflow, "entity_buffer_overflow");
      (deadlock_demon, "entity_deadlock_demon");
      (infinite_loop, "entity_infinite_loop");
      (memory_leak, "entity_memory_leak");
      (null_pointer, "entity_null_pointer");
      (race_hazard, "entity_race_hazard");
      (syntax_glitch, "entity_syntax_glitch");
    ]
  in

  List.iter
    (fun (ent, golden_filename) ->
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
        Personnel borrow_checker_chaplain );
      ("json_personnel_ada_safety_engineer", Personnel ada_safety_engineer);
      ("json_personnel_haskell_lazy_evaluator", Personnel haskell_lazy_evaluator);
      ("json_personnel_ocaml_module_architect", Personnel ocaml_module_architect);
      ("json_personnel_institute_operative", Personnel institute_operative);
      ("json_personnel_shift_supervisor", Personnel shift_supervisor);
      ("json_personnel_memguard", Personnel memguard);
      ("json_personnel_e_acc", Personnel e_acc);
      ("json_procedure_lifetime_extension", Procedure lifetime_extension);
      ("json_procedure_mutable_borrow", Procedure mutable_borrow);
      ("json_procedure_ownership_transfer", Procedure ownership_transfer);
      ("json_procedure_rapid_response", Procedure rapid_response);
      ("json_procedure_standard_protocol", Procedure standard_protocol);
      ("json_procedure_temporary_boost", Procedure temporary_boost);
      ("json_procedure_forced_knowledge", Procedure forced_knowledge);
      ("json_event_borrow_checker_rejection", Event borrow_checker_rejection);
      ("json_event_lifetime_violation", Event lifetime_violation);
      ("json_event_unsafe_code_panic", Event unsafe_code_panic);
      ("json_event_strategic_retreat", Event strategic_retreat);
      ("json_entity_buffer_overflow", Entity buffer_overflow);
      ("json_entity_deadlock_demon", Entity deadlock_demon);
      ("json_entity_infinite_loop", Entity infinite_loop);
      ("json_entity_memory_leak", Entity memory_leak);
      ("json_entity_null_pointer", Entity null_pointer);
      ("json_entity_race_hazard", Entity race_hazard);
      ("json_entity_syntax_glitch", Entity syntax_glitch);
      ("json_entity_heisenbug", Entity heisenbug);
    ]
  in

  run_json_golden_tests
    ~render:(fun card -> J.json_to_string (J.card_to_json card))
    json_golden_files;

  let language_json_items =
    [
      ( "json_rust_personnel_borrow_checker_chaplain",
        Personnel borrow_checker_chaplain );
      ("json_rust_procedure_lifetime_extension", Procedure lifetime_extension);
      ("json_ada_personnel_ada_safety_engineer", Personnel ada_safety_engineer);
      ( "json_haskell_personnel_haskell_lazy_evaluator",
        Personnel haskell_lazy_evaluator );
      ( "json_ocaml_personnel_ocaml_module_architect",
        Personnel ocaml_module_architect );
      ("json_institute_entity_buffer_overflow", Entity buffer_overflow);
    ]
  in

  let run_language_json_tests ~render ~prefix items =
    let prefixed_items =
      List.map (fun (name, card) -> (prefix ^ "_" ^ name, card)) items
    in
    run_golden_tests
      ~load:(fun f -> load_file ~ext:".json" f)
      ~write:(fun f c -> write_file ~ext:".json" f c)
      ~render ~label:"Language JSON" prefixed_items
  in

  run_language_json_tests
    ~render:(fun card -> TS_Rust.card_to_string card)
    ~prefix:"rust" language_json_items;

  run_language_json_tests
    ~render:(fun card -> TS_Haskell.card_to_string card)
    ~prefix:"haskell" language_json_items;

  run_language_json_tests
    ~render:(fun card -> TS_Ada.card_to_string card)
    ~prefix:"ada" language_json_items;

  run_language_json_tests
    ~render:(fun card -> TS_OCaml.card_to_string card)
    ~prefix:"ocaml" language_json_items;

  let language_test_items =
    [
      ("personnel_borrow_checker_chaplain", Personnel borrow_checker_chaplain);
      ("procedure_lifetime_extension", Procedure lifetime_extension);
      ("personnel_ada_safety_engineer", Personnel ada_safety_engineer);
      ("personnel_haskell_lazy_evaluator", Personnel haskell_lazy_evaluator);
      ("personnel_ocaml_module_architect", Personnel ocaml_module_architect);
      ("entity_buffer_overflow", Entity buffer_overflow);
    ]
  in

  let run_language_golden_tests ~render ~prefix items =
    let labeled_items =
      List.map (fun (name, card) -> (prefix ^ "_" ^ name, card)) items
    in
    run_text_golden_tests ~render labeled_items
  in

  run_language_golden_tests ~render:TS_Rust.card_to_string ~prefix:"rust"
    language_test_items;

  run_language_golden_tests ~render:TS_Haskell.card_to_string ~prefix:"haskell"
    language_test_items;

  run_language_golden_tests ~render:TS_Ada.card_to_string ~prefix:"ada"
    language_test_items;

  run_language_golden_tests ~render:TS_OCaml.card_to_string ~prefix:"ocaml"
    language_test_items
