module Core = Typefuckery.Core
module Cards = Typefuckery.Cards
module Effects = Typefuckery.Effects
module Abilities = Typefuckery.Abilities
module Engine = Typefuckery.Engine
module TS = Typefuckery.To_string
module R = Typefuckery.Registry
module Int = Typefuckery.Int
open Util

module type ELIXIR_PHRASES = sig
  include TS.PHRASES

  val spawn_process : int -> string
  val send_message_to_sector : Core.sector -> string
  val supervisor_restart : string
  val on_process_crash : string
  val on_message_received : Core.sector -> string
  val elixir_division_name : string
end

module Elixir_english_phrases : ELIXIR_PHRASES = struct
  include Typefuckery.English_phrases

  let spawn_process count =
    Printf.sprintf "Spawn %d worker process%s" count
      (if count = 1 then "" else "es")

  let send_message_to_sector sector =
    Printf.sprintf "Send a message to %s sector" (sector_name sector)

  let supervisor_restart = "Restart all crashed processes under this supervisor"
  let on_process_crash = "When a process crashes"

  let on_message_received sector =
    Printf.sprintf "When a message is received in %s sector"
      (sector_name sector)

  let elixir_division_name = "Elixir"
end

module Elixir_types = struct
  type fx =
    | Spawn_process of { count : int }
    | Send_message of { to_sector : Core.sector }
    | Supervisor_restart

  type trig = On_process_crash | On_message_received of Core.sector
  type div = Core of Core.division | Elixir [@@warning "-37"]
end

module Make_elixir_extension (P : ELIXIR_PHRASES) :
  Typefuckery.Division_intf.EXTENSION
    with type fx = Elixir_types.fx
     and type trig = Elixir_types.trig
     and type div = Elixir_types.div = struct
  include Elixir_types

  let fx_to_string = function
    | Spawn_process { count } -> P.spawn_process count
    | Send_message { to_sector } -> P.send_message_to_sector to_sector
    | Supervisor_restart -> P.supervisor_restart

  let trig_to_string = function
    | On_process_crash -> P.on_process_crash
    | On_message_received sector -> P.on_message_received sector

  let div_to_string = function
    | Core d -> P.division_name d
    | Elixir -> P.elixir_division_name
end

module Elixir_extension = Make_elixir_extension (Elixir_english_phrases)
module E = Engine.Make (Elixir_extension)
module Elixir_renderer = TS.MakeExt (Elixir_extension) (Elixir_english_phrases)

type elixir_card =
  (Elixir_extension.div, Elixir_extension.fx, Elixir_extension.trig) Cards.card

module type PHOENIX_PHRASES = sig
  include ELIXIR_PHRASES

  val live_view_attach : string -> string
  val channel_broadcast : string -> Core.sector -> string
  val presence_track : string
  val on_channel_join : Core.sector -> string
  val on_live_view_mount : string
  val phoenix_division_name : string
end

module Phoenix_english_phrases : PHOENIX_PHRASES = struct
  include Elixir_english_phrases

  let live_view_attach component =
    Printf.sprintf "Attach LiveView component \"%s\"" component

  let channel_broadcast channel to_sector =
    Printf.sprintf "Broadcast on channel \"%s\" to %s sector" channel
      (sector_name to_sector)

  let presence_track = "Track presence for all connected clients"

  let on_channel_join sector =
    Printf.sprintf "When a client joins a channel in %s sector"
      (sector_name sector)

  let on_live_view_mount = "When a LiveView component mounts"
  let phoenix_division_name = "Phoenix"
end

module Phoenix_types = struct
  type fx =
    | Elixir of Elixir_types.fx
    | Live_view_attach of { component : string }
    | Channel_broadcast of { channel : string; to_sector : Core.sector }
    | Presence_track

  type trig =
    | Elixir of Elixir_types.trig
    | On_channel_join of Core.sector
    | On_live_view_mount

  type div = Elixir of Elixir_types.div | Phoenix [@@warning "-37"]
end

module Make_phoenix_extension (P : PHOENIX_PHRASES) :
  Typefuckery.Division_intf.EXTENSION
    with type fx = Phoenix_types.fx
     and type trig = Phoenix_types.trig
     and type div = Phoenix_types.div = struct
  type fx = Phoenix_types.fx
  type trig = Phoenix_types.trig
  type div = Phoenix_types.div

  module Elixir_ext = Make_elixir_extension (P)

  let fx_to_string : fx -> string = function
    | Phoenix_types.Elixir e -> Elixir_ext.fx_to_string e
    | Phoenix_types.Live_view_attach { component } ->
        P.live_view_attach component
    | Phoenix_types.Channel_broadcast { channel; to_sector } ->
        P.channel_broadcast channel to_sector
    | Phoenix_types.Presence_track -> P.presence_track

  let trig_to_string : trig -> string = function
    | Phoenix_types.Elixir t -> Elixir_ext.trig_to_string t
    | Phoenix_types.On_channel_join sector -> P.on_channel_join sector
    | Phoenix_types.On_live_view_mount -> P.on_live_view_mount

  let div_to_string : div -> string = function
    | Phoenix_types.Elixir d -> Elixir_ext.div_to_string d
    | Phoenix_types.Phoenix -> P.phoenix_division_name
end

module Phoenix_extension = Make_phoenix_extension (Phoenix_english_phrases)
module Phoenix_E = Engine.Make (Phoenix_extension)

module Phoenix_renderer =
  TS.MakeExt (Phoenix_extension) (Phoenix_english_phrases)

type phoenix_card =
  ( Phoenix_extension.div,
    Phoenix_extension.fx,
    Phoenix_extension.trig )
  Cards.card

type json = Util.Json.t

open Util.Json

let json_to_string = Util.Json.to_string

module Elixir_json = struct
  let limit_to_json = function
    | None -> Null
    | Some Abilities.Once_per_round -> String "once_per_round"

  let passive_ability_to_json
      (a : Elixir_extension.fx Abilities.passive_ability) : json =
    let effect_text =
      match a.condition with
      | None -> Elixir_renderer.effect_to_string a.card_effect
      | Some cond ->
          Printf.sprintf "If %s: %s"
            (Elixir_renderer.condition_to_string cond)
            (Elixir_renderer.effect_to_string a.card_effect)
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
      (a : Elixir_extension.fx Abilities.activated_ability) : json =
    let cc_cost = Int.to_int a.cc_cost in
    let effect_text =
      match a.condition with
      | None -> Elixir_renderer.effect_to_string a.card_effect
      | Some cond ->
          Printf.sprintf "If %s: %s"
            (Elixir_renderer.condition_to_string cond)
            (Elixir_renderer.effect_to_string a.card_effect)
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
        (Elixir_extension.fx, Elixir_extension.trig) Abilities.triggered_ability)
      : json =
    let trigger_text = Elixir_renderer.trigger_to_string a.trigger in
    let effect_text =
      match a.condition with
      | None -> Elixir_renderer.effect_to_string a.card_effect
      | Some cond ->
          Printf.sprintf "if %s: %s"
            (Elixir_renderer.condition_to_string cond)
            (Elixir_renderer.effect_to_string a.card_effect)
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
      (a : Elixir_extension.fx Abilities.burnout_ability) : json =
    Object
      [
        ("type", String "burnout");
        ("limit", Null);
        ("cost_cc", Null);
        ("optional", Null);
        ("trigger_text", String "When this Personnel's CC is reduced to 0");
        ("effect_text", String (Elixir_renderer.effect_to_string a.card_effect));
      ]

  let ability_to_json :
      (Elixir_extension.fx, Elixir_extension.trig) Abilities.ability -> json =
    function
    | Abilities.Passive a -> passive_ability_to_json a
    | Abilities.Activated a -> activated_ability_to_json a
    | Abilities.Triggered a -> triggered_ability_to_json a
    | Abilities.Burnout a -> burnout_ability_to_json a

  let threat_level_to_json (t : Cards.threat_level) : json =
    String (String.lowercase_ascii (Elixir_renderer.threat_level_to_string t))

  let personnel_to_json
      (p :
        ( Elixir_extension.div,
          Elixir_extension.fx,
          Elixir_extension.trig )
        Cards.personnel) : json =
    Object
      [
        ("card_type", String "personnel");
        ("id", String (Core.Card_id.to_string p.id));
        ("name", String p.name);
        ( "division",
          String
            (String.lowercase_ascii
               (Elixir_renderer.division_to_string p.division)) );
        ("starting_cc", Int (Int.to_int p.starting_cc));
        ("abilities", Array (List.map ability_to_json p.abilities));
      ]

  let procedure_to_json
      (p : (Elixir_extension.div, Elixir_extension.fx) Cards.procedure) : json =
    Object
      [
        ("card_type", String "procedure");
        ("id", String (Core.Card_id.to_string p.id));
        ("name", String p.name);
        ( "division",
          String
            (String.lowercase_ascii
               (Elixir_renderer.division_to_string p.division)) );
        ("effect_text", String (Elixir_renderer.effect_to_string p.card_effect));
      ]

  let entity_to_json
      (e : (Elixir_extension.div, Elixir_extension.fx) Cards.entity) : json =
    Object
      [
        ("card_type", String "entity");
        ("id", String (Core.Card_id.to_string e.id));
        ("name", String e.name);
        ( "division",
          String
            (String.lowercase_ascii
               (Elixir_renderer.division_to_string e.division)) );
        ("threat_level", threat_level_to_json e.threat_level);
        ("breach_timer", Int (Int.to_int e.breach_timer));
        ( "end_phase_effect_text",
          String (Elixir_renderer.effect_to_string e.end_phase_effect) );
        ( "breach_effect_text",
          String (Elixir_renderer.effect_to_string e.breach_effect) );
        ( "containment_text",
          String
            (Elixir_renderer.containment_requirement_to_string e.containment) );
      ]

  let card_to_json :
      ( Elixir_extension.div,
        Elixir_extension.fx,
        Elixir_extension.trig )
      Cards.card ->
      json = function
    | Cards.Personnel p -> personnel_to_json p
    | Cards.Procedure p -> procedure_to_json p
    | Cards.Event _ ->
        failwith "Elixir_json.card_to_json: Event not implemented"
    | Cards.Entity e -> entity_to_json e
end

module Phoenix_json = struct
  let limit_to_json = function
    | None -> Null
    | Some Abilities.Once_per_round -> String "once_per_round"

  let passive_ability_to_json
      (a : Phoenix_extension.fx Abilities.passive_ability) : json =
    let effect_text =
      match a.condition with
      | None -> Phoenix_renderer.effect_to_string a.card_effect
      | Some cond ->
          Printf.sprintf "If %s: %s"
            (Phoenix_renderer.condition_to_string cond)
            (Phoenix_renderer.effect_to_string a.card_effect)
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
      (a : Phoenix_extension.fx Abilities.activated_ability) : json =
    let cc_cost = Int.to_int a.cc_cost in
    let effect_text =
      match a.condition with
      | None -> Phoenix_renderer.effect_to_string a.card_effect
      | Some cond ->
          Printf.sprintf "If %s: %s"
            (Phoenix_renderer.condition_to_string cond)
            (Phoenix_renderer.effect_to_string a.card_effect)
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
        ( Phoenix_extension.fx,
          Phoenix_extension.trig )
        Abilities.triggered_ability) : json =
    let trigger_text = Phoenix_renderer.trigger_to_string a.trigger in
    let effect_text =
      match a.condition with
      | None -> Phoenix_renderer.effect_to_string a.card_effect
      | Some cond ->
          Printf.sprintf "if %s: %s"
            (Phoenix_renderer.condition_to_string cond)
            (Phoenix_renderer.effect_to_string a.card_effect)
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
      (a : Phoenix_extension.fx Abilities.burnout_ability) : json =
    Object
      [
        ("type", String "burnout");
        ("limit", Null);
        ("cost_cc", Null);
        ("optional", Null);
        ("trigger_text", String "When this Personnel's CC is reduced to 0");
        ("effect_text", String (Phoenix_renderer.effect_to_string a.card_effect));
      ]

  let ability_to_json :
      (Phoenix_extension.fx, Phoenix_extension.trig) Abilities.ability -> json =
    function
    | Abilities.Passive a -> passive_ability_to_json a
    | Abilities.Activated a -> activated_ability_to_json a
    | Abilities.Triggered a -> triggered_ability_to_json a
    | Abilities.Burnout a -> burnout_ability_to_json a

  let threat_level_to_json (t : Cards.threat_level) : json =
    String (String.lowercase_ascii (Phoenix_renderer.threat_level_to_string t))

  let personnel_to_json
      (p :
        ( Phoenix_extension.div,
          Phoenix_extension.fx,
          Phoenix_extension.trig )
        Cards.personnel) : json =
    Object
      [
        ("card_type", String "personnel");
        ("id", String (Core.Card_id.to_string p.id));
        ("name", String p.name);
        ( "division",
          String
            (String.lowercase_ascii
               (Phoenix_renderer.division_to_string p.division)) );
        ("starting_cc", Int (Int.to_int p.starting_cc));
        ("abilities", Array (List.map ability_to_json p.abilities));
      ]

  let entity_to_json
      (e : (Phoenix_extension.div, Phoenix_extension.fx) Cards.entity) : json =
    Object
      [
        ("card_type", String "entity");
        ("id", String (Core.Card_id.to_string e.id));
        ("name", String e.name);
        ( "division",
          String
            (String.lowercase_ascii
               (Phoenix_renderer.division_to_string e.division)) );
        ("threat_level", threat_level_to_json e.threat_level);
        ("breach_timer", Int (Int.to_int e.breach_timer));
        ( "end_phase_effect_text",
          String (Phoenix_renderer.effect_to_string e.end_phase_effect) );
        ( "breach_effect_text",
          String (Phoenix_renderer.effect_to_string e.breach_effect) );
        ( "containment_text",
          String
            (Phoenix_renderer.containment_requirement_to_string e.containment)
        );
      ]

  let card_to_json :
      ( Phoenix_extension.div,
        Phoenix_extension.fx,
        Phoenix_extension.trig )
      Cards.card ->
      json = function
    | Cards.Personnel p -> personnel_to_json p
    | Cards.Procedure _ ->
        failwith "Phoenix_json.card_to_json: Procedure not implemented"
    | Cards.Event _ ->
        failwith "Phoenix_json.card_to_json: Event not implemented"
    | Cards.Entity e -> entity_to_json e
end

let genserver :
    ( Elixir_extension.div,
      Elixir_extension.fx,
      Elixir_extension.trig )
    Cards.personnel =
  {
    id = Core.Card_id.of_string "elixir:genserver";
    name = "GenServer";
    division = Elixir_types.Elixir;
    lore = None;
    flavor_text = None;
    starting_cc = Int.four;
    abilities =
      [
        Abilities.Activated
          {
            id = None;
            cc_cost = Int.two;
            condition = None;
            card_effect = E.ext (Elixir_types.Spawn_process { count = 3 });
          };
        Abilities.Triggered
          {
            id = None;
            trigger = Abilities.Ext Elixir_types.On_process_crash;
            limit = Some Abilities.Once_per_round;
            optionality = Abilities.Mandatory;
            condition = None;
            card_effect = E.ext Elixir_types.Supervisor_restart;
          };
        Abilities.Triggered
          {
            id = None;
            trigger = Abilities.Core Abilities.When_deployed;
            limit = None;
            optionality = Abilities.Mandatory;
            condition = None;
            card_effect = E.ext (Elixir_types.Spawn_process { count = 1 });
          };
        Abilities.Triggered
          {
            id = None;
            trigger =
              Abilities.Ext (Elixir_types.On_message_received Core.Alpha);
            limit = None;
            optionality = Abilities.Optional;
            condition = None;
            card_effect = E.add_cc ~target:E.this_personnel ~amount:E.Amount.one;
          };
      ];
  }

let message_passing :
    (Elixir_extension.div, Elixir_extension.fx) Cards.procedure =
  {
    id = Core.Card_id.of_string "elixir:message_passing";
    name = "Message Passing";
    division = Elixir_types.Elixir;
    lore = None;
    flavor_text = None;
    card_effect =
      E.composite
        [
          E.ext (Elixir_types.Send_message { to_sector = Core.Beta });
          E.add_cc
            ~target:(E.all_personnel_in_sector Core.Beta)
            ~amount:E.Amount.one;
        ];
  }

let otp_supervisor : (Elixir_extension.div, Elixir_extension.fx) Cards.entity =
  {
    id = Core.Card_id.of_string "elixir:otp_supervisor";
    name = "OTP Supervisor";
    division = Elixir_types.Elixir;
    lore = None;
    flavor_text = None;
    threat_level = Cards.Euclid;
    breach_timer = Int.Positive.four;
    end_phase_effect =
      E.composite
        [
          E.ext (Elixir_types.Spawn_process { count = 2 });
          E.remove_cc ~target:E.all_personnel ~amount:E.Amount.one;
        ];
    breach_effect =
      E.composite
        [
          E.ext (Elixir_types.Spawn_process { count = 10 });
          E.remove_cc ~target:E.all_personnel ~amount:E.Amount.three;
        ];
    containment = { check = E.personnel_count_in_sector Core.Lambda 2 };
  }

let elixir_cards : elixir_card list =
  [
    Cards.Personnel genserver;
    Cards.Procedure message_passing;
    Cards.Entity otp_supervisor;
  ]

module Phoenix_helpers = struct
  let spawn_process ~count =
    Phoenix_E.ext (Phoenix_types.Elixir (Elixir_types.Spawn_process { count }))

  let send_message ~to_sector =
    Phoenix_E.ext
      (Phoenix_types.Elixir (Elixir_types.Send_message { to_sector }))

  let live_view ~component =
    Phoenix_E.ext (Phoenix_types.Live_view_attach { component })

  let broadcast ~channel ~to_sector =
    Phoenix_E.ext (Phoenix_types.Channel_broadcast { channel; to_sector })

  let presence_track = Phoenix_E.ext Phoenix_types.Presence_track

  let on_process_crash : Phoenix_extension.trig Abilities.trigger =
    Abilities.Ext (Phoenix_types.Elixir Elixir_types.On_process_crash)

  let on_channel_join sector =
    Abilities.Ext (Phoenix_types.On_channel_join sector)

  let on_live_view_mount = Abilities.Ext Phoenix_types.On_live_view_mount
end

let live_view_coordinator :
    ( Phoenix_extension.div,
      Phoenix_extension.fx,
      Phoenix_extension.trig )
    Cards.personnel =
  {
    id = Core.Card_id.of_string "phoenix:live_view_coordinator";
    name = "LiveView Coordinator";
    division = Phoenix_types.Phoenix;
    lore = None;
    flavor_text = None;
    starting_cc = Int.five;
    abilities =
      [
        Abilities.Activated
          {
            id = None;
            cc_cost = Int.one;
            condition = None;
            card_effect = Phoenix_helpers.spawn_process ~count:2;
          };
        Abilities.Activated
          {
            id = None;
            cc_cost = Int.two;
            condition = None;
            card_effect = Phoenix_helpers.live_view ~component:"DashboardLive";
          };
        Abilities.Triggered
          {
            id = None;
            trigger = Phoenix_helpers.on_process_crash;
            limit = None;
            optionality = Abilities.Mandatory;
            condition = None;
            card_effect =
              Phoenix_helpers.broadcast ~channel:"errors" ~to_sector:Core.Alpha;
          };
        Abilities.Triggered
          {
            id = None;
            trigger = Phoenix_helpers.on_live_view_mount;
            limit = Some Abilities.Once_per_round;
            optionality = Abilities.Optional;
            condition = None;
            card_effect = Phoenix_helpers.spawn_process ~count:1;
          };
      ];
  }

let channel_router :
    ( Phoenix_extension.div,
      Phoenix_extension.fx,
      Phoenix_extension.trig )
    Cards.personnel =
  {
    id = Core.Card_id.of_string "phoenix:channel_router";
    name = "Channel Router";
    division = Phoenix_types.Phoenix;
    lore = None;
    flavor_text = None;
    starting_cc = Int.three;
    abilities =
      [
        Abilities.Triggered
          {
            id = None;
            trigger = Phoenix_helpers.on_channel_join Core.Beta;
            limit = None;
            optionality = Abilities.Mandatory;
            condition = None;
            card_effect =
              Phoenix_E.composite
                [
                  Phoenix_helpers.presence_track;
                  Phoenix_E.add_cc ~target:Phoenix_E.this_personnel
                    ~amount:Phoenix_E.Amount.one;
                ];
          };
        Abilities.Activated
          {
            id = None;
            cc_cost = Int.two;
            condition = None;
            card_effect =
              Phoenix_E.composite
                [
                  Phoenix_helpers.send_message ~to_sector:Core.Lambda;
                  Phoenix_helpers.broadcast ~channel:"updates"
                    ~to_sector:Core.Lambda;
                ];
          };
      ];
  }

let pubsub_storm : (Phoenix_extension.div, Phoenix_extension.fx) Cards.entity =
  {
    id = Core.Card_id.of_string "phoenix:pubsub_storm";
    name = "PubSub Storm";
    division = Phoenix_types.Phoenix;
    lore = None;
    flavor_text = None;
    threat_level = Cards.Keter;
    breach_timer = Int.Positive.three;
    end_phase_effect =
      Phoenix_E.composite
        [
          Phoenix_helpers.broadcast ~channel:"chaos" ~to_sector:Core.Gamma;
          Phoenix_helpers.spawn_process ~count:5;
          Phoenix_E.remove_cc ~target:Phoenix_E.all_personnel
            ~amount:Phoenix_E.Amount.two;
        ];
    breach_effect =
      Phoenix_E.composite
        [
          Phoenix_helpers.broadcast ~channel:"catastrophe" ~to_sector:Core.Alpha;
          Phoenix_E.send_to_abyss (Phoenix_E.choose_personnel ());
          Phoenix_E.send_to_abyss (Phoenix_E.choose_personnel ());
        ];
    containment = { check = Phoenix_E.personnel_count_in_sector Core.Gamma 3 };
  }

let phoenix_cards : phoenix_card list =
  [
    Cards.Personnel live_view_coordinator;
    Cards.Personnel channel_router;
    Cards.Entity pubsub_storm;
  ]

let test_register_elixir_division () =
  let registry = R.empty in
  match
    R.register_division registry ~id:"elixir" ~name:"Elixir Division"
      ~cards:elixir_cards ~renderer:Elixir_renderer.card_to_string
  with
  | Ok r -> r
  | Error e ->
      let msg =
        match e with
        | `Set_id_already_registered id -> "Set_id_already_registered: " ^ id
        | `Set_name_already_registered (name, _) ->
            "Set_name_already_registered: " ^ name
        | `Duplicate_card_id_within_set (set_id, _, _) ->
            "Duplicate_card_id_within_set: " ^ set_id
        | `Card_id_already_registered (_, set1, set2) ->
            "Card_id_already_registered: " ^ set1 ^ " vs " ^ set2
      in
      failwith ("Elixir division registration failed: " ^ msg)

let test_rendered_text_contains_custom_effects registry =
  let genserver_entry =
    match R.find_card registry (Core.Card_id.of_string "elixir:genserver") with
    | Some entry -> entry
    | None -> failwith "GenServer card not found"
  in
  assert_contains genserver_entry.rendered_text "Spawn 3 worker processes"
    "GenServer spawn text present";
  assert_contains genserver_entry.rendered_text "When a process crashes"
    "GenServer trigger text present"

let test_registry_metadata registry =
  let set =
    match R.find_set_by_id registry "elixir" with
    | Some s -> s
    | None -> failwith "Elixir set not found"
  in
  assert_true (set.metadata.name = "Elixir Division") "Elixir set name correct";
  assert_true
    (List.length set.cards = List.length elixir_cards)
    "Elixir card count matches"

let test_register_phoenix_division registry =
  match
    R.register_division registry ~id:"phoenix" ~name:"Phoenix Division"
      ~cards:phoenix_cards ~renderer:Phoenix_renderer.card_to_string
  with
  | Ok r -> r
  | Error e ->
      let msg =
        match e with
        | `Set_id_already_registered id -> "Set_id_already_registered: " ^ id
        | `Set_name_already_registered (name, _) ->
            "Set_name_already_registered: " ^ name
        | `Duplicate_card_id_within_set (set_id, _, _) ->
            "Duplicate_card_id_within_set: " ^ set_id
        | `Card_id_already_registered (_, set1, set2) ->
            "Card_id_already_registered: " ^ set1 ^ " vs " ^ set2
      in
      failwith ("Phoenix division registration failed: " ^ msg)

let test_phoenix_inherits_elixir_effects registry =
  let coordinator_entry =
    match
      R.find_card registry
        (Core.Card_id.of_string "phoenix:live_view_coordinator")
    with
    | Some entry -> entry
    | None -> failwith "LiveView Coordinator card not found"
  in

  assert_contains coordinator_entry.rendered_text "Spawn 2 worker processes"
    "Phoenix card contains inherited Elixir spawn effect";
  assert_contains coordinator_entry.rendered_text "When a process crashes"
    "Phoenix card contains inherited Elixir trigger";

  assert_contains coordinator_entry.rendered_text
    "Attach LiveView component \"DashboardLive\""
    "Phoenix card contains LiveView effect";
  assert_contains coordinator_entry.rendered_text
    "Broadcast on channel \"errors\" to Alpha sector"
    "Phoenix card contains broadcast effect";
  assert_contains coordinator_entry.rendered_text
    "When a LiveView component mounts"
    "Phoenix card contains LiveView mount trigger"

let test_phoenix_mixed_inheritance registry =
  let router_entry =
    match
      R.find_card registry (Core.Card_id.of_string "phoenix:channel_router")
    with
    | Some entry -> entry
    | None -> failwith "Channel Router card not found"
  in

  assert_contains router_entry.rendered_text
    "When a client joins a channel in Beta sector"
    "Channel Router contains Phoenix channel trigger";

  assert_contains router_entry.rendered_text "Track presence"
    "Channel Router contains presence tracking";

  assert_contains router_entry.rendered_text "Send a message to Lambda sector"
    "Channel Router contains inherited Elixir send_message";

  assert_contains router_entry.rendered_text "Broadcast on channel \"updates\""
    "Channel Router contains Phoenix broadcast"

let test_phoenix_entity_mixed_effects registry =
  let storm_entry =
    match
      R.find_card registry (Core.Card_id.of_string "phoenix:pubsub_storm")
    with
    | Some entry -> entry
    | None -> failwith "PubSub Storm card not found"
  in

  assert_contains storm_entry.rendered_text "Broadcast on channel \"chaos\""
    "PubSub Storm contains Phoenix broadcast";
  assert_contains storm_entry.rendered_text "Spawn 5 worker processes"
    "PubSub Storm contains inherited Elixir spawn";
  assert_contains storm_entry.rendered_text "Remove 2 CC"
    "PubSub Storm contains core effect"

let test_phoenix_registry_metadata registry =
  let set =
    match R.find_set_by_id registry "phoenix" with
    | Some s -> s
    | None -> failwith "Phoenix set not found"
  in

  assert_true
    (set.metadata.name = "Phoenix Division")
    "Phoenix set name correct";
  assert_true (List.length set.cards = 3) "Three Phoenix cards"

let test_elixir_division_renders_correctly registry =
  let genserver_entry =
    match R.find_card registry (Core.Card_id.of_string "elixir:genserver") with
    | Some entry -> entry
    | None -> failwith "GenServer card not found"
  in

  assert_contains genserver_entry.rendered_text "Elixir"
    "GenServer shows Elixir division"

let test_phoenix_division_renders_correctly registry =
  let coordinator_entry =
    match
      R.find_card registry
        (Core.Card_id.of_string "phoenix:live_view_coordinator")
    with
    | Some entry -> entry
    | None -> failwith "LiveView Coordinator card not found"
  in

  assert_contains coordinator_entry.rendered_text "Phoenix"
    "LiveView Coordinator shows Phoenix division"

let () =
  let registry = test_register_elixir_division () in
  test_rendered_text_contains_custom_effects registry;
  test_registry_metadata registry;
  test_elixir_division_renders_correctly registry;

  let registry = test_register_phoenix_division registry in
  test_phoenix_inherits_elixir_effects registry;
  test_phoenix_mixed_inheritance registry;
  test_phoenix_entity_mixed_effects registry;
  test_phoenix_registry_metadata registry;
  test_phoenix_division_renders_correctly registry;

  let all_sets = R.list_sets registry in
  assert_true (List.length all_sets = 2) "Both divisions registered";

  let elixir_text_goldens =
    [
      ("elixir_personnel_genserver", Cards.Personnel genserver);
      ("elixir_procedure_message_passing", Cards.Procedure message_passing);
      ("elixir_entity_otp_supervisor", Cards.Entity otp_supervisor);
    ]
  in

  run_text_golden_tests ~render:Elixir_renderer.card_to_string
    elixir_text_goldens;

  let elixir_json_goldens =
    [
      ("json_elixir_personnel_genserver", Cards.Personnel genserver);
      ("json_elixir_procedure_message_passing", Cards.Procedure message_passing);
      ("json_elixir_entity_otp_supervisor", Cards.Entity otp_supervisor);
    ]
  in

  run_json_golden_tests
    ~render:(fun card -> json_to_string (Elixir_json.card_to_json card))
    elixir_json_goldens;

  let phoenix_text_goldens =
    [
      ( "phoenix_personnel_live_view_coordinator",
        Cards.Personnel live_view_coordinator );
      ("phoenix_personnel_channel_router", Cards.Personnel channel_router);
      ("phoenix_entity_pubsub_storm", Cards.Entity pubsub_storm);
    ]
  in

  run_text_golden_tests ~render:Phoenix_renderer.card_to_string
    phoenix_text_goldens;

  let phoenix_json_goldens =
    [
      ( "json_phoenix_personnel_live_view_coordinator",
        Cards.Personnel live_view_coordinator );
      ("json_phoenix_personnel_channel_router", Cards.Personnel channel_router);
      ("json_phoenix_entity_pubsub_storm", Cards.Entity pubsub_storm);
    ]
  in

  run_json_golden_tests
    ~render:(fun card -> json_to_string (Phoenix_json.card_to_json card))
    phoenix_json_goldens
