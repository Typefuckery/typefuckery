open Core
open Targets
open Effects
open Abilities
open Cards

type verb_forms = Phrases_common.verb_forms = {
  infinitive : string;
  imperative : string;
  third_person : string;
}

module type PHRASES = sig
  val this : string
  val the : string
  val a : string
  val all : string
  val each : string
  val another : string
  val chosen : string
  val personnel : string
  val personnel_possessive : string
  val procedure : string
  val event : string
  val entity : string
  val entities : string
  val sector : string
  val card : string
  val cards : string
  val player : string
  val cc : string
  val breach_marker : string
  val breach_markers : string
  val abyss : string
  val hand : string
  val battlefield : string
  val stack : string
  val deck : string
  val from : string
  val to_ : string
  val in_ : string
  val on : string
  val for_ : string
  val up_to : string
  val then_ : string
  val and_ : string
  val or_ : string
  val if_ : string
  val when_ : string
  val at : string
  val that : string
  val one_to_another : string -> string
  val one_of_to_the_other : string -> string
  val add : verb_forms
  val remove : verb_forms
  val move : verb_forms
  val draw : verb_forms
  val discard : verb_forms
  val deploy : verb_forms
  val send : verb_forms
  val flip : verb_forms
  val prevent : verb_forms
  val choose : verb_forms
  val spend : verb_forms
  val reset : verb_forms
  val contain : verb_forms
  val breach_markers_to_start : string
  val you : string
  val you_may : string
  val another_target_player : string
  val a_target_player : string
  val each_player : string
  val their_hand : string
  val no_effect : string
  val if_you_cant_do_nothing : string
  val loss : string
  val passive : string
  val burnout : string
  val cc_cost_label : string
  val at_end_of_round : string
  val at_end_of_phase : string
  val when_deployed : string
  val when_cc_reduced_to_zero : string
  val once_per_round : string
  val before_end_phase : string
  val before_end_phase_step_2 : string
  val at_start_of_end_phase : string
  val at_end_of_round_timing : string
  val this_round : string
  val future_rounds : string
  val not_from_spending_cc : string
  val not_from_card_effect : string
  val not_from_entity_effect : string
  val not_from_breach : string
  val chosen_by_active_player : string
  val chosen_by_starting_player : string
  val chosen_by_controller : string
  val label_deck : string
  val label_id : string
  val label_starting_cc : string
  val label_abilities : string
  val label_effect : string
  val label_resolve_immediately : string
  val label_threat_level : string
  val label_breach_timer : string
  val label_end_phase_effect : string
  val label_breach_effect : string
  val label_containment : string
  val none_placeholder : string
  val bonus : string
  val have : string
  val total_cc : string
  val is_ : string
  val this_sector : string
  val an_entity_possessive : string
  val a_card_effect_would_reduce : string
  val is_deployed : string
  val is_breached : string
  val is_not_breached : string
  val always_contained : string
  val cannot_be_contained : string
  val not_ : string
  val other_than : string
  val with_ : string
  val effect_resolves : string
  val or_more : int -> string
  val capitalize : string -> string
  val division_name : division -> string
  val sector_name : sector -> string
  val sector_state_name : sector_state -> string
  val threat_level_name : threat_level -> string
  val has_min_cc : min_cc:int -> string
  val filter_in_sector : sector -> string
  val filter_and : string -> string -> string
  val in_same_sector_as_this_personnel : string
  val other_personnel_description : string
  val other_personnel_in_sector : string
  val filter_in_play : string
  val filter_uncontained : string
  val controlled_by_active_player : string
  val controlled_by_chooser : string
  val bound_that : string
  val bound_each_personnel_in_this_sector : string
  val that_entity : string
  val that_sector : string
  val chooses_one : string
  val either : string
end

module type TEXT_SERIALIZER = sig
  val personnel : string
  val procedure : string
  val event : string
  val entity : string
  val int_to_string : _ Int.t -> string
  val division_to_string : division -> string
  val sector_to_string : sector -> string
  val sector_state_to_string : sector_state -> string
  val zone_to_string : zone -> string
  val player_id_to_string : Id.player -> string
  val obj_id_to_string : Id.obj -> string
  val card_id_to_string : Card_id.t -> string
  val personnel_state_to_string : personnel_state -> string
  val entity_state_to_string : entity_state -> string
  val ctx_to_string : ctx -> string
  val condition_to_string : condition -> string
  val personnel_filter_to_string : personnel_filter -> string
  val target_personnel_to_string : target_personnel -> string
  val target_entity_to_string : target_entity -> string
  val target_sector_to_string : target_sector -> string
  val core_effect_to_string : core_effect -> string
  val card_effect_to_string : Effects.core -> string
  val core_trigger_to_string : core_trigger -> string
  val trigger_to_string : trigger_timing -> string
  val passive_ability_to_string : core_passive_ability -> string
  val activated_ability_to_string : core_activated_ability -> string
  val triggered_ability_to_string : core_triggered_ability -> string
  val burnout_ability_to_string : core_burnout_ability -> string
  val ability_to_string : core_ability -> string
  val threat_level_to_string : threat_level -> string
  val containment_requirement_to_string : containment_requirement -> string
  val personnel_to_string : core_personnel -> string
  val procedure_to_string : core_procedure -> string
  val event_to_string : core_event -> string
  val entity_to_string : core_entity -> string
  val card_to_string : core_card -> string
end

module type S = sig
  type fx
  type trig
  type div
  type effect_t = fx Effects.t
  type trigger_t = trig Abilities.trigger
  type passive_ability_t = fx Abilities.passive_ability
  type activated_ability_t = fx Abilities.activated_ability
  type triggered_ability_t = (fx, trig) Abilities.triggered_ability
  type burnout_ability_t = fx Abilities.burnout_ability
  type ability_t = (fx, trig) Abilities.ability
  type personnel_t = (div, fx, trig) Cards.personnel
  type procedure_t = (div, fx) Cards.procedure
  type event_t = (div, fx) Cards.event
  type entity_t = (div, fx) Cards.entity
  type card_t = (div, fx, trig) Cards.card

  val int_to_string : _ Int.t -> string
  val division_to_string : div -> string
  val sector_to_string : sector -> string
  val sector_state_to_string : sector_state -> string
  val zone_to_string : zone -> string
  val player_id_to_string : Id.player -> string
  val obj_id_to_string : Id.obj -> string
  val card_id_to_string : Card_id.t -> string
  val personnel_state_to_string : personnel_state -> string
  val entity_state_to_string : entity_state -> string
  val ctx_to_string : ctx -> string
  val condition_to_string : condition -> string
  val personnel_filter_to_string : personnel_filter -> string
  val target_personnel_to_string : target_personnel -> string
  val target_entity_to_string : target_entity -> string
  val target_sector_to_string : target_sector -> string
  val core_effect_to_string : core_effect -> string
  val effect_to_string : effect_t -> string
  val core_trigger_to_string : core_trigger -> string
  val trigger_to_string : trigger_t -> string
  val passive_ability_to_string : passive_ability_t -> string
  val activated_ability_to_string : activated_ability_t -> string
  val triggered_ability_to_string : triggered_ability_t -> string
  val burnout_ability_to_string : burnout_ability_t -> string
  val ability_to_string : ability_t -> string
  val threat_level_to_string : threat_level -> string
  val containment_requirement_to_string : containment_requirement -> string
  val personnel_to_string : personnel_t -> string
  val procedure_to_string : procedure_t -> string
  val event_to_string : event_t -> string
  val entity_to_string : entity_t -> string
  val card_to_string : card_t -> string
end

let bool_to_string b = if b then "true" else "false"

let sort_kv_pairs (pairs : (string * string) list) : (string * string) list =
  List.sort Stdlib.compare pairs

module MakeExt (Ext : Division_intf.EXTENSION) (P : PHRASES) :
  S with type fx = Ext.fx and type trig = Ext.trig and type div = Ext.div =
struct
  type fx = Ext.fx
  type trig = Ext.trig
  type div = Ext.div
  type effect_t = fx Effects.t
  type trigger_t = trig Abilities.trigger
  type passive_ability_t = fx Abilities.passive_ability
  type activated_ability_t = fx Abilities.activated_ability
  type triggered_ability_t = (fx, trig) Abilities.triggered_ability
  type burnout_ability_t = fx Abilities.burnout_ability
  type ability_t = (fx, trig) Abilities.ability
  type personnel_t = (div, fx, trig) Cards.personnel
  type procedure_t = (div, fx) Cards.procedure
  type event_t = (div, fx) Cards.event
  type entity_t = (div, fx) Cards.entity
  type card_t = (div, fx, trig) Cards.card
  type verb_mood = Infinitive | Imperative

  let int_to_string n = Int.to_int n |> string_of_int
  let division_to_string = Ext.div_to_string
  let sector_to_string = P.sector_name
  let sector_state_to_string = P.sector_state_name

  let zone_to_string = function
    | Hand -> P.hand
    | Battlefield sector ->
        Printf.sprintf "%s(%s)" P.battlefield (sector_to_string sector)
    | Abyss -> P.abyss
    | Stack -> P.stack
    | Deck -> P.deck

  let player_id_to_string (id : Id.player) = string_of_int id
  let obj_id_to_string (id : Id.obj) = string_of_int id
  let card_id_to_string (id : Card_id.t) = Card_id.to_string id

  let personnel_state_to_string (p : personnel_state) =
    Printf.sprintf
      "{id=%s; controller=%s; card_id=%s; sector=%s; current_cc=%d; \
       activated_this_round=%s}"
      (obj_id_to_string p.id)
      (player_id_to_string p.controller)
      (card_id_to_string p.card_id)
      (sector_to_string p.sector)
      p.current_cc
      (bool_to_string p.activated_this_round)

  let entity_state_to_string (e : entity_state) =
    Printf.sprintf
      "{entity_card_id=%s; entity_sector=%s; contained=%s; \
       breach_markers_remaining=%d}"
      (card_id_to_string e.entity_card_id)
      (sector_to_string e.entity_sector)
      (bool_to_string e.contained)
      e.breach_markers_remaining

  let ctx_to_string (c : ctx) =
    let sectors =
      SectorMap.bindings c.sectors
      |> List.sort (fun (s1, _) (s2, _) -> Core.compare_sector s1 s2)
      |> List.map (fun (s, st) ->
          Printf.sprintf "%s=%s" (sector_to_string s)
            (sector_state_to_string st))
      |> String.concat ", "
    in
    let personnel =
      ObjMap.bindings c.personnel
      |> List.sort (fun (id1, _) (id2, _) -> Stdlib.Int.compare id1 id2)
      |> List.map (fun (id, p) ->
          Printf.sprintf "%s:%s" (obj_id_to_string id)
            (personnel_state_to_string p))
      |> String.concat ", "
    in
    let entities =
      SectorMap.bindings c.entities
      |> List.sort (fun (s1, _) (s2, _) -> Core.compare_sector s1 s2)
      |> List.map (fun (s, e) ->
          Printf.sprintf "%s:%s" (sector_to_string s) (entity_state_to_string e))
      |> String.concat ", "
    in
    let breach_markers =
      SectorMap.bindings c.breach_markers
      |> List.sort (fun (s1, _) (s2, _) -> Core.compare_sector s1 s2)
      |> List.map (fun (s, n) -> Printf.sprintf "%s=%d" (sector_to_string s) n)
      |> String.concat ", "
    in
    let custom_state =
      sort_kv_pairs c.custom_state
      |> List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v)
      |> String.concat ", "
    in
    Printf.sprintf
      "{sectors=[%s]; personnel=[%s]; entities=[%s]; breach_markers=[%s]; \
       custom_state=[%s]; source_obj_id=%s; active_player=%s}"
      sectors personnel entities breach_markers custom_state
      (match c.source_obj_id with
      | None -> "None"
      | Some id -> string_of_int id)
      (player_id_to_string c.active_player)

  let core_condition_kind_to_string = function
    | Condition.Always -> P.always_contained
    | Condition.Never -> P.cannot_be_contained
    | Condition.Sector_breached sector ->
        Printf.sprintf "%s %s %s" P.sector (sector_to_string sector)
          P.is_breached
    | Condition.Personnel_count { sector; min_count } ->
        Printf.sprintf "%s %s %s %s %s" (P.or_more min_count) P.personnel P.in_
          P.sector (sector_to_string sector)
    | Condition.Personnel_with_min_cc { sector; min_count; min_cc } ->
        Printf.sprintf "%s %s %s %s %s, %s %s %s %s" (P.or_more min_count)
          P.personnel P.in_ P.sector (sector_to_string sector) P.each P.with_
          (P.or_more min_cc) P.cc
    | Condition.Total_cc_in_sector { sector; total } ->
        Printf.sprintf "%s %s %s %s %s %s" P.total_cc P.in_ P.sector
          (sector_to_string sector) P.is_ (string_of_int total)
    | Condition.Total_cc_in_this_sector { total } ->
        Printf.sprintf "%s %s %s %s %s" P.total_cc P.in_ P.this_sector P.is_
          (string_of_int total)

  let rec condition_kind_to_string = function
    | Condition.Core_cond core -> core_condition_kind_to_string core
    | Condition.Ext (_ : No_ext.t) -> .
    | Condition.Not (Condition.Core_cond (Condition.Sector_breached sector)) ->
        Printf.sprintf "%s %s %s" P.sector (sector_to_string sector)
          P.is_not_breached
    | Condition.Not kind ->
        Printf.sprintf "%s (%s)" P.not_ (condition_kind_to_string kind)
    | Condition.And (k1, k2) ->
        Printf.sprintf "(%s %s %s)"
          (condition_kind_to_string k1)
          P.and_
          (condition_kind_to_string k2)
    | Condition.Or (k1, k2) ->
        Printf.sprintf "%s %s %s"
          (condition_kind_to_string k1)
          P.or_
          (condition_kind_to_string k2)

  let condition_to_string (c : condition) = condition_kind_to_string c.kind

  let rec personnel_filter_kind_to_string = function
    | Targets.Has_min_cc min_cc -> P.has_min_cc ~min_cc
    | Targets.In_sector sector -> P.filter_in_sector sector
    | Targets.Filter_and (k1, k2) ->
        P.filter_and
          (personnel_filter_kind_to_string k1)
          (personnel_filter_kind_to_string k2)
    | Targets.In_same_sector_as_this -> P.in_same_sector_as_this_personnel
    | Targets.Exclude_this -> P.other_personnel_description
    | Targets.Other_in_sector -> P.other_personnel_in_sector
    | Targets.Filter_in_play -> P.filter_in_play
    | Targets.Controlled_by_active_player -> P.controlled_by_active_player

  let personnel_filter_to_string (f : personnel_filter) =
    personnel_filter_kind_to_string (Targets.personnel_filter_kind f)

  let entity_filter_kind_to_string = function
    | Targets.Uncontained -> P.filter_uncontained

  let entity_filter_to_string (f : entity_filter) =
    entity_filter_kind_to_string (Targets.entity_filter_kind f)

  let sector_filter_kind_to_string = function
    | Targets.Sector_filter_placeholder -> P.sector
    | Targets.Other_than_source_sector ->
        Printf.sprintf "%s %s" P.other_than P.that_sector

  let sector_filter_to_string (f : sector_filter) =
    sector_filter_kind_to_string (Targets.sector_filter_kind f)

  let bound_kind_to_string = function
    | Targets.Bound_that -> P.bound_that
    | Targets.Each_personnel_in_this_sector ->
        P.bound_each_personnel_in_this_sector
    | Targets.In_bound_entity_sector ->
        Printf.sprintf "%s %s %s %s" P.all P.personnel P.in_ P.that_sector

  let describe_in_sector = function
    | None -> None
    | Some sector ->
        Some
          (Printf.sprintf "%s %s %s" P.in_ P.sector (sector_to_string sector))

  let describe_filter = function
    | None -> None
    | Some filter -> (
        let desc = personnel_filter_to_string filter in
        match Targets.personnel_filter_tag filter with
        | Other -> Some desc
        | In_play -> Some desc
        | Generic -> Some (Printf.sprintf "%s %s" P.that desc))

  let is_other_in_play_filter (filter : personnel_filter option) =
    match filter with
    | Some f -> (
        match Targets.personnel_filter_kind f with
        | Filter_and (Exclude_this, Filter_in_play)
        | Filter_and (Filter_in_play, Exclude_this) ->
            true
        | _ -> false)
    | None -> false

  let describe_entity_filter = function
    | None -> None
    | Some filter -> Some (entity_filter_to_string filter)

  let describe_sector_filter = function
    | None -> None
    | Some filter -> Some (sector_filter_to_string filter)

  let chooser_to_string = function
    | None -> None
    | Some Active_player -> Some P.chosen_by_active_player
    | Some Starting_player -> Some P.chosen_by_starting_player
    | Some Controller -> Some P.chosen_by_controller

  let with_clauses (base : string) (clauses : string option list) : string =
    let parts = List.filter_map Fun.id clauses in
    match parts with
    | [] -> base
    | _ -> Printf.sprintf "%s %s" base (String.concat " " parts)

  let target_personnel_to_string = function
    | This_personnel -> Printf.sprintf "%s %s" P.this P.personnel
    | This_personnel_sector ->
        Printf.sprintf "%s %s %s %s %s %s" P.all P.personnel P.in_ P.this
          P.personnel_possessive P.sector
    | Choose_personnel_each_player { in_sector } ->
        let clauses = [ describe_in_sector in_sector ] in
        with_clauses
          (Printf.sprintf "%s %s %s" P.a P.personnel P.controlled_by_chooser)
          clauses
    | Choose_personnel { in_sector; filter; chooser } ->
        let base =
          match filter with
          | Some f -> (
              let desc = personnel_filter_to_string f in
              match Targets.personnel_filter_kind f with
              | Controlled_by_active_player ->
                  Printf.sprintf "%s %s %s" P.a P.personnel desc
              | _ -> (
                  match Targets.personnel_filter_tag f with
                  | Other -> Printf.sprintf "%s %s" P.another desc
                  | In_play -> Printf.sprintf "%s %s %s" P.a P.personnel desc
                  | Generic ->
                      Printf.sprintf "%s %s %s %s %s" P.a P.chosen P.personnel
                        P.that desc))
          | None -> Printf.sprintf "%s %s" P.a P.personnel
        in
        let clauses =
          [ describe_in_sector in_sector; chooser_to_string chooser ]
        in
        with_clauses base clauses
    | Choose_personnel_pair { in_sector; filter; chooser } ->
        let clauses =
          [
            describe_in_sector in_sector;
            describe_filter filter;
            chooser_to_string chooser;
          ]
        in
        with_clauses (Printf.sprintf "2 %s %s" P.chosen P.personnel) clauses
    | All_personnel { in_sector; filter } ->
        if is_other_in_play_filter filter then
          with_clauses
            (Printf.sprintf "%s %s %s" P.all P.other_personnel_description
               P.personnel)
            [ describe_in_sector in_sector ]
        else
          with_clauses
            (Printf.sprintf "%s %s" P.all P.personnel)
            [ describe_in_sector in_sector; describe_filter filter ]
    | Bound { index = _; kind } -> bound_kind_to_string kind

  let target_personnel_each_to_string = function
    | All_personnel { in_sector; filter } ->
        if is_other_in_play_filter filter then
          with_clauses
            (Printf.sprintf "%s %s %s" P.each P.other_personnel_description
               P.personnel)
            [ describe_in_sector in_sector ]
        else
          let base = Printf.sprintf "%s %s" P.each P.personnel in
          let clauses =
            [ describe_in_sector in_sector; describe_filter filter ]
          in
          with_clauses base clauses
    | This_personnel_sector ->
        Printf.sprintf "%s %s %s %s %s %s" P.each P.personnel P.in_ P.this
          P.personnel_possessive P.sector
    | target -> target_personnel_to_string target

  let target_entity_to_string = function
    | Entity_in_this_sector ->
        Printf.sprintf "%s %s %s %s %s" P.the P.entity P.in_ P.this P.sector
    | Entity_in_sector sector ->
        Printf.sprintf "%s %s %s %s %s" P.the P.entity P.in_ P.sector
          (sector_to_string sector)
    | Choose_entity { filter } ->
        with_clauses
          (Printf.sprintf "%s %s %s" P.a P.chosen P.entity)
          [ describe_entity_filter filter ]
    | All_entities { filter } ->
        with_clauses
          (Printf.sprintf "%s %s" P.all P.entities)
          [ describe_entity_filter filter ]
    | Bound_entity _ -> P.that_entity

  let target_sector_to_string = function
    | This_sector -> Printf.sprintf "%s %s" P.this P.sector
    | Specific sector ->
        Printf.sprintf "%s %s" P.sector (sector_to_string sector)
    | Choose_sector { filter; chooser } ->
        let base =
          match chooser with
          | Some _ -> Printf.sprintf "%s %s" P.a P.sector
          | None -> Printf.sprintf "%s %s %s" P.a P.chosen P.sector
        in
        let clauses =
          [ describe_sector_filter filter; chooser_to_string chooser ]
        in
        with_clauses base clauses
    | Bound_entity_sector _ -> P.that_sector

  let breach_marker_label amount =
    match Int.to_int amount with 1 -> P.breach_marker | _ -> P.breach_markers

  let timing_window_to_string : timing_window -> string = function
    | End_phase -> P.before_end_phase
    | End_of_round -> P.at_end_of_round_timing

  let timing_scope_to_string = function
    | This_round -> P.this_round
    | Future_round -> P.future_rounds

  let verb_for_mood verb = function
    | Imperative -> P.capitalize verb.imperative
    | Infinitive -> verb.infinitive

  let render_player_card_action ~verb_forms ~player ~mood ~amount_str ~card_noun
      =
    match (player, mood) with
    | You, Infinitive ->
        Printf.sprintf "%s %s %s" verb_forms.infinitive amount_str card_noun
    | You, Imperative ->
        Printf.sprintf "%s %s %s"
          (P.capitalize verb_forms.imperative)
          amount_str card_noun
    | Another_player, Infinitive ->
        Printf.sprintf "%s %s %s %s %s" P.have P.another_target_player
          verb_forms.infinitive amount_str card_noun
    | Another_player, Imperative ->
        Printf.sprintf "%s %s %s %s"
          (P.capitalize P.another_target_player)
          verb_forms.third_person amount_str card_noun
    | Any_player, Infinitive ->
        Printf.sprintf "%s %s %s %s %s" P.have P.a_target_player
          verb_forms.infinitive amount_str card_noun
    | Any_player, Imperative ->
        Printf.sprintf "%s %s %s %s"
          (P.capitalize P.a_target_player)
          verb_forms.third_person amount_str card_noun
    | Each_player, Infinitive ->
        Printf.sprintf "%s %s %s %s %s" P.have P.each_player
          verb_forms.infinitive amount_str card_noun
    | Each_player, Imperative ->
        Printf.sprintf "%s %s %s %s"
          (P.capitalize P.each_player)
          verb_forms.third_person amount_str card_noun

  let rec effect_to_string_with_mood mood (eff : effect_t) =
    match eff with
    | Core_effect core -> core_effect_to_string_with_mood mood core
    | Ext ext -> Ext.fx_to_string ext
    | If_possible eff ->
        Printf.sprintf "%s. %s"
          (effect_to_string_with_mood mood eff)
          (P.capitalize P.if_you_cant_do_nothing)
    | Let { name = _; target; body } ->
        let target_desc = target_personnel_to_string target in
        let body_str = effect_to_string_with_mood mood body in
        let verb = verb_for_mood P.choose mood in
        Printf.sprintf "%s %s. %s" verb target_desc body_str
    | Let_entity { name = _; target; body } ->
        let target_desc = target_entity_to_string target in
        let body_str = effect_to_string_with_mood mood body in
        let verb = verb_for_mood P.choose mood in
        Printf.sprintf "%s %s. %s" verb target_desc body_str
    | Delayed { window; scope; then_do } ->
        let window_str = timing_window_to_string window in
        let scope_str = timing_scope_to_string scope in
        let effect_str = effect_to_string_with_mood mood then_do in
        Printf.sprintf "%s %s: %s" window_str scope_str effect_str
    | Composite effects -> (
        let rec take_flip sectors state = function
          | Core_effect (Flip_sector { target = Specific sector; to_state })
            :: rest -> (
              match state with
              | None -> take_flip (sector :: sectors) (Some to_state) rest
              | Some state' when state' = to_state ->
                  take_flip (sector :: sectors) state rest
              | _ -> (List.rev sectors, state, rest))
          | rest -> (List.rev sectors, state, rest)
        in
        let all_sectors = [ Alpha; Beta; Gamma; Lambda ] in
        let flip_sectors, flip_state, rest = take_flip [] None effects in
        let sort = List.sort compare in
        let compact_flip =
          match flip_state with
          | Some to_state when sort flip_sectors = sort all_sectors ->
              Some
                (Printf.sprintf "%s %s %s %s %s"
                   (verb_for_mood P.flip mood)
                   P.each P.sector P.to_
                   (sector_state_to_string to_state))
          | _ -> None
        in
        let rendered =
          match compact_flip with
          | Some compact ->
              compact :: List.map (effect_to_string_with_mood mood) rest
          | None -> List.map (effect_to_string_with_mood mood) effects
        in
        match (compact_flip, rendered) with
        | None, [ e1; e2 ] -> Printf.sprintf "%s, %s %s" e1 P.then_ e2
        | _ -> String.concat "; " rendered)
    | Player_choice { player; option_a; option_b } ->
        let player_str =
          match player with
          | You -> P.you
          | Another_player -> P.another_target_player
          | Any_player -> P.a_target_player
          | Each_player -> P.each_player
        in
        Printf.sprintf "%s %s: %s %s, %s %s" (P.capitalize player_str)
          P.chooses_one P.either
          (effect_to_string_with_mood mood option_a)
          P.or_
          (effect_to_string_with_mood mood option_b)
    | Log msg -> msg
    | Noop -> P.capitalize P.no_effect

  and core_effect_to_string_with_mood mood eff =
    let verb v = verb_for_mood v mood in
    match eff with
    | Add_CC { target; amount } ->
        Printf.sprintf "%s %s %s %s %s" (verb P.add) (int_to_string amount) P.cc
          P.to_
          (target_personnel_each_to_string target)
    | Remove_CC { target; amount } ->
        Printf.sprintf "%s %s %s %s %s" (verb P.remove) (int_to_string amount)
          P.cc P.from
          (target_personnel_each_to_string target)
    | Move_CC { from; to_; amount } ->
        let amount_str = int_to_string amount in
        let from_str = target_personnel_to_string from in
        let to_str = target_personnel_to_string to_ in
        let a_personnel = Printf.sprintf "%s %s" P.a P.personnel in
        if from_str = a_personnel && to_str = a_personnel then
          Printf.sprintf "%s %s %s %s" (verb P.move) amount_str P.cc
            (P.one_to_another P.personnel)
        else
          Printf.sprintf "%s %s %s %s %s %s %s" (verb P.move) amount_str P.cc
            P.from from_str P.to_ to_str
    | Move_CC_between_pair { pair; amount } ->
        Printf.sprintf "%s %s %s %s" (verb P.move) (int_to_string amount) P.cc
          (P.one_of_to_the_other (target_personnel_to_string pair))
    | Deploy_personnel { card; to_sector; bonus_cc } ->
        Printf.sprintf "%s %s %s %s %s %s (%s %s %s)" (verb P.deploy)
          P.personnel (card_id_to_string card) P.to_ P.sector
          (sector_to_string to_sector)
          P.bonus (int_to_string bonus_cc) P.cc
    | Move_personnel
        { target = Choose_personnel_each_player { in_sector }; to_sector } ->
        let target_desc =
          target_personnel_to_string
            (Choose_personnel_each_player { in_sector })
        in
        Printf.sprintf "%s %s %s %s %s %s %s %s"
          (P.capitalize P.each_player)
          P.choose.third_person target_desc P.and_ P.move.third_person
          P.bound_that P.to_
          (target_sector_to_string to_sector)
    | Move_personnel { target; to_sector } ->
        Printf.sprintf "%s %s %s %s" (verb P.move)
          (target_personnel_to_string target)
          P.to_
          (target_sector_to_string to_sector)
    | Send_to_abyss { target } ->
        Printf.sprintf "%s %s %s %s" (verb P.send)
          (target_personnel_to_string target)
          P.to_ P.abyss
    | Add_breach_marker { target; amount } ->
        Printf.sprintf "%s %s %s %s %s" (verb P.add) (int_to_string amount)
          (breach_marker_label amount)
          P.to_
          (target_entity_to_string target)
    | Remove_breach_marker { target; amount } ->
        Printf.sprintf "%s %s %s %s %s" (verb P.remove) (int_to_string amount)
          (breach_marker_label amount)
          P.from
          (target_entity_to_string target)
    | Reset_breach_markers { target } ->
        Printf.sprintf "%s %s %s" (verb P.reset)
          (target_entity_to_string target)
          P.breach_markers_to_start
    | Flip_sector { target; to_state } ->
        Printf.sprintf "%s %s %s %s" (verb P.flip)
          (target_sector_to_string target)
          P.to_
          (sector_state_to_string to_state)
    | Draw { player; amount } ->
        render_player_card_action ~verb_forms:P.draw ~player ~mood
          ~amount_str:(int_to_string amount)
          ~card_noun:(if Int.to_int amount = 1 then P.card else P.cards)
    | Discard { player; amount } ->
        render_player_card_action ~verb_forms:P.discard ~player ~mood
          ~amount_str:(int_to_string amount)
          ~card_noun:(if Int.to_int amount = 1 then P.card else P.cards)
    | Discard_hand { player } -> (
        let hand_str = Printf.sprintf "%s %s" P.their_hand P.hand in
        match (player, mood) with
        | You, Infinitive ->
            Printf.sprintf "%s %s" P.discard.infinitive hand_str
        | You, Imperative ->
            Printf.sprintf "%s %s" (P.capitalize P.discard.imperative) hand_str
        | Another_player, Infinitive ->
            Printf.sprintf "%s %s %s %s" P.have P.another_target_player
              P.discard.infinitive hand_str
        | Another_player, Imperative ->
            Printf.sprintf "%s %s %s"
              (P.capitalize P.another_target_player)
              P.discard.third_person hand_str
        | Any_player, Infinitive ->
            Printf.sprintf "%s %s %s %s" P.have P.a_target_player
              P.discard.infinitive hand_str
        | Any_player, Imperative ->
            Printf.sprintf "%s %s %s"
              (P.capitalize P.a_target_player)
              P.discard.third_person hand_str
        | Each_player, Infinitive ->
            Printf.sprintf "%s %s %s %s" P.have P.each_player
              P.discard.infinitive hand_str
        | Each_player, Imperative ->
            Printf.sprintf "%s %s %s"
              (P.capitalize P.each_player)
              P.discard.third_person hand_str)
    | Prevent_CC_loss { target; amount } ->
        Printf.sprintf "%s %s %s %s %s %s %s" (verb P.prevent) P.up_to
          (int_to_string amount) P.cc P.loss P.on
          (target_personnel_to_string target)
    | Contain_entity { target } ->
        Printf.sprintf "%s %s %s" (verb P.contain)
          (target_entity_to_string target)
          P.this_round

  let effect_to_string eff = effect_to_string_with_mood Imperative eff

  let core_effect_to_string core =
    core_effect_to_string_with_mood Imperative core

  let core_trigger_to_string = function
    | End_of_round -> P.at_end_of_round
    | End_phase -> P.at_end_of_phase
    | When_cc_would_reduce { target; exclude_source } -> (
        let base =
          Printf.sprintf "%s %s %s %s %s" P.when_ P.a_card_effect_would_reduce
            P.cc P.for_
            (target_personnel_to_string target)
        in
        match exclude_source with
        | None -> base
        | Some Spend_CC -> base ^ " " ^ P.not_from_spending_cc
        | Some Card_Effect -> base ^ " " ^ P.not_from_card_effect
        | Some Entity_Effect -> base ^ " " ^ P.not_from_entity_effect
        | Some Breach -> base ^ " " ^ P.not_from_breach)
    | When_deployed -> P.when_deployed
    | When_personnel_deployed_in_sector sector ->
        Printf.sprintf "%s %s %s %s %s %s" P.when_ P.personnel P.is_deployed
          P.in_ P.sector (sector_to_string sector)
    | When_entity_effect { in_sector } ->
        with_clauses
          (Printf.sprintf "%s %s %s" P.when_ P.an_entity_possessive
             P.effect_resolves)
          [ describe_in_sector in_sector ]
    | When_sector_breached sector ->
        Printf.sprintf "%s %s %s %s" P.when_ P.sector (sector_to_string sector)
          P.is_breached

  let trigger_to_string (t : trigger_t) =
    match t with
    | Core core -> core_trigger_to_string core
    | Ext ext -> Ext.trig_to_string ext

  let ability_limit_to_string = function
    | Once_per_round -> P.capitalize P.once_per_round

  let passive_ability_to_string (a : passive_ability_t) =
    let limit_prefix =
      match a.limit with
      | None -> ""
      | Some limit -> ability_limit_to_string limit ^ ", "
    in
    match a.condition with
    | None ->
        Printf.sprintf "%s - %s%s" (P.capitalize P.passive) limit_prefix
          (effect_to_string a.card_effect)
    | Some cond ->
        Printf.sprintf "%s - %s%s %s: %s" (P.capitalize P.passive) limit_prefix
          (P.capitalize P.if_) (condition_to_string cond)
          (effect_to_string a.card_effect)

  let activated_ability_to_string (a : activated_ability_t) =
    let cost = int_to_string a.cc_cost in
    let cost_prefix =
      if cost = "0" then ""
      else
        Printf.sprintf "%s %s %s, %s "
          (P.capitalize P.spend.imperative)
          cost P.cc P.then_
    in
    match a.condition with
    | None ->
        Printf.sprintf "%s (%s) - %s%s" P.cc_cost_label cost cost_prefix
          (effect_to_string a.card_effect)
    | Some cond ->
        Printf.sprintf "%s (%s) - %s %s: %s%s" P.cc_cost_label cost
          (P.capitalize P.if_) (condition_to_string cond) cost_prefix
          (effect_to_string a.card_effect)

  let triggered_ability_to_string (a : triggered_ability_t) =
    let trigger = trigger_to_string a.trigger in
    let limit_prefix =
      match a.limit with
      | None -> ""
      | Some limit -> ability_limit_to_string limit ^ ", "
    in
    let may_prefix =
      match a.optionality with Optional -> P.you_may ^ " " | Mandatory -> ""
    in
    let mood =
      match a.optionality with
      | Optional -> Infinitive
      | Mandatory -> Imperative
    in
    let effect_str = effect_to_string_with_mood mood a.card_effect in
    match a.condition with
    | None ->
        Printf.sprintf "%s - %s%s: %s%s" (P.capitalize P.passive) limit_prefix
          trigger may_prefix effect_str
    | Some cond ->
        Printf.sprintf "%s - %s%s, %s %s: %s%s" (P.capitalize P.passive)
          limit_prefix trigger P.if_ (condition_to_string cond) may_prefix
          effect_str

  let burnout_ability_to_string (a : burnout_ability_t) =
    Printf.sprintf "%s - %s: %s" (P.capitalize P.burnout)
      (P.capitalize P.when_cc_reduced_to_zero)
      (effect_to_string a.card_effect)

  let ability_to_string : ability_t -> string = function
    | Passive a -> passive_ability_to_string a
    | Activated a -> activated_ability_to_string a
    | Triggered a -> triggered_ability_to_string a
    | Burnout a -> burnout_ability_to_string a

  let threat_level_to_string = P.threat_level_name

  let containment_requirement_to_string (c : containment_requirement) =
    P.capitalize (condition_to_string c.check)

  let flavor_suffix = function None -> "" | Some text -> "\n\n---\n" ^ text

  let personnel_to_string (p : personnel_t) =
    let abilities = p.abilities |> List.map ability_to_string in
    let abilities_str =
      match abilities with
      | [] -> P.none_placeholder
      | _ -> String.concat "\n  - " ("" :: abilities)
    in
    Printf.sprintf "%s\n%s %s\n%s %s\n%s %s\n\n%s%s%s" p.name P.label_deck
      (division_to_string p.division)
      P.label_id (card_id_to_string p.id) P.label_starting_cc
      (int_to_string p.starting_cc)
      P.label_abilities abilities_str
      (flavor_suffix p.flavor_text)

  let procedure_to_string (p : procedure_t) =
    Printf.sprintf "%s\n%s %s\n%s %s\n\n%s\n  - %s%s" p.name P.label_deck
      (division_to_string p.division)
      P.label_id (card_id_to_string p.id) P.label_effect
      (effect_to_string p.card_effect)
      (flavor_suffix p.flavor_text)

  let event_to_string (e : event_t) =
    Printf.sprintf "%s\n%s %s\n%s %s\n\n%s\n  - %s%s" e.name P.label_deck
      (division_to_string e.division)
      P.label_id (card_id_to_string e.id) P.label_resolve_immediately
      (effect_to_string e.card_effect)
      (flavor_suffix e.flavor_text)

  let entity_to_string (e : entity_t) =
    Printf.sprintf
      "%s\n\
       %s %s\n\
       %s %s\n\
       %s %s\n\
       %s %s\n\n\
       %s\n\
      \  - %s\n\n\
       %s\n\
      \  - %s\n\n\
       %s\n\
      \  - %s%s"
      e.name P.label_deck
      (division_to_string e.division)
      P.label_id (card_id_to_string e.id) P.label_threat_level
      (threat_level_to_string e.threat_level)
      P.label_breach_timer
      (int_to_string e.breach_timer)
      P.label_end_phase_effect
      (effect_to_string e.end_phase_effect)
      P.label_breach_effect
      (effect_to_string e.breach_effect)
      P.label_containment
      (containment_requirement_to_string e.containment)
      (flavor_suffix e.flavor_text)

  let card_to_string : card_t -> string = function
    | Personnel p -> personnel_to_string p
    | Procedure p -> procedure_to_string p
    | Event e -> event_to_string e
    | Entity e -> entity_to_string e
end

module Core :
  S
    with type fx = No_ext.t
     and type trig = No_ext.t
     and type div = Core.division =
  MakeExt (Division_intf.No_extension) (English_phrases)

module Make (P : PHRASES) : TEXT_SERIALIZER = struct
  module M = MakeExt (Division_intf.No_extension) (P)

  let personnel = P.personnel
  let procedure = P.procedure
  let event = P.event
  let entity = P.entity
  let int_to_string = M.int_to_string
  let division_to_string = M.division_to_string
  let sector_to_string = M.sector_to_string
  let sector_state_to_string = M.sector_state_to_string
  let zone_to_string = M.zone_to_string
  let player_id_to_string = M.player_id_to_string
  let obj_id_to_string = M.obj_id_to_string
  let card_id_to_string = M.card_id_to_string
  let personnel_state_to_string = M.personnel_state_to_string
  let entity_state_to_string = M.entity_state_to_string
  let ctx_to_string = M.ctx_to_string
  let condition_to_string = M.condition_to_string
  let personnel_filter_to_string = M.personnel_filter_to_string
  let target_personnel_to_string = M.target_personnel_to_string
  let target_entity_to_string = M.target_entity_to_string
  let target_sector_to_string = M.target_sector_to_string
  let core_effect_to_string = M.core_effect_to_string
  let card_effect_to_string = M.effect_to_string
  let core_trigger_to_string = M.core_trigger_to_string
  let trigger_to_string = M.trigger_to_string
  let passive_ability_to_string = M.passive_ability_to_string
  let activated_ability_to_string = M.activated_ability_to_string
  let triggered_ability_to_string = M.triggered_ability_to_string
  let burnout_ability_to_string = M.burnout_ability_to_string
  let ability_to_string = M.ability_to_string
  let threat_level_to_string = M.threat_level_to_string
  let containment_requirement_to_string = M.containment_requirement_to_string
  let personnel_to_string = M.personnel_to_string
  let procedure_to_string = M.procedure_to_string
  let event_to_string = M.event_to_string
  let entity_to_string = M.entity_to_string
  let card_to_string = M.card_to_string
end

module Detailed_English : TEXT_SERIALIZER = Make (English_phrases)
module Pidgin_English : TEXT_SERIALIZER = Make (Pidgin_english_phrases)
module Rust : TEXT_SERIALIZER = Make (Rust_phrases)
module Haskell : TEXT_SERIALIZER = Make (Haskell_phrases)
module Ada : TEXT_SERIALIZER = Make (Ada_phrases)
module OCaml : TEXT_SERIALIZER = Make (Ocaml_phrases)
