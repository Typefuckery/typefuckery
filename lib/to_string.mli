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
  val division_name : Core.division -> string
  val sector_name : Core.sector -> string
  val sector_state_name : Core.sector_state -> string
  val threat_level_name : Cards.threat_level -> string
  val has_min_cc : min_cc:int -> string
  val filter_in_sector : Core.sector -> string
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
  val division_to_string : Core.division -> string
  val sector_to_string : Core.sector -> string
  val sector_state_to_string : Core.sector_state -> string
  val zone_to_string : Core.zone -> string
  val player_id_to_string : Core.Id.player -> string
  val obj_id_to_string : Core.Id.obj -> string
  val card_id_to_string : Core.Card_id.t -> string
  val personnel_state_to_string : Core.personnel_state -> string
  val entity_state_to_string : Core.entity_state -> string
  val ctx_to_string : Core.ctx -> string
  val condition_to_string : Targets.condition -> string
  val personnel_filter_to_string : Targets.personnel_filter -> string
  val target_personnel_to_string : Targets.target_personnel -> string
  val target_entity_to_string : Targets.target_entity -> string
  val target_sector_to_string : Targets.target_sector -> string
  val core_effect_to_string : Effects.core_effect -> string
  val card_effect_to_string : Effects.core -> string
  val core_trigger_to_string : Abilities.core_trigger -> string
  val trigger_to_string : Abilities.trigger_timing -> string
  val passive_ability_to_string : Abilities.core_passive_ability -> string
  val activated_ability_to_string : Abilities.core_activated_ability -> string
  val triggered_ability_to_string : Abilities.core_triggered_ability -> string
  val burnout_ability_to_string : Abilities.core_burnout_ability -> string
  val ability_to_string : Abilities.core_ability -> string
  val threat_level_to_string : Cards.threat_level -> string

  val containment_requirement_to_string :
    Cards.containment_requirement -> string

  val personnel_to_string : Cards.core_personnel -> string
  val procedure_to_string : Cards.core_procedure -> string
  val event_to_string : Cards.core_event -> string
  val entity_to_string : Cards.core_entity -> string
  val card_to_string : Cards.core_card -> string
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
  val sector_to_string : Core.sector -> string
  val sector_state_to_string : Core.sector_state -> string
  val zone_to_string : Core.zone -> string
  val player_id_to_string : Core.Id.player -> string
  val obj_id_to_string : Core.Id.obj -> string
  val card_id_to_string : Core.Card_id.t -> string
  val personnel_state_to_string : Core.personnel_state -> string
  val entity_state_to_string : Core.entity_state -> string
  val ctx_to_string : Core.ctx -> string
  val condition_to_string : Targets.condition -> string
  val personnel_filter_to_string : Targets.personnel_filter -> string
  val target_personnel_to_string : Targets.target_personnel -> string
  val target_entity_to_string : Targets.target_entity -> string
  val target_sector_to_string : Targets.target_sector -> string
  val core_effect_to_string : Effects.core_effect -> string
  val effect_to_string : effect_t -> string
  val core_trigger_to_string : Abilities.core_trigger -> string
  val trigger_to_string : trigger_t -> string
  val passive_ability_to_string : passive_ability_t -> string
  val activated_ability_to_string : activated_ability_t -> string
  val triggered_ability_to_string : triggered_ability_t -> string
  val burnout_ability_to_string : burnout_ability_t -> string
  val ability_to_string : ability_t -> string
  val threat_level_to_string : Cards.threat_level -> string

  val containment_requirement_to_string :
    Cards.containment_requirement -> string

  val personnel_to_string : personnel_t -> string
  val procedure_to_string : procedure_t -> string
  val event_to_string : event_t -> string
  val entity_to_string : entity_t -> string
  val card_to_string : card_t -> string
end

module MakeExt (Ext : Division_intf.EXTENSION) (_ : PHRASES) :
  S with type fx = Ext.fx and type trig = Ext.trig and type div = Ext.div

module Core :
  S
    with type fx = No_ext.t
     and type trig = No_ext.t
     and type div = Core.division

module Make (_ : PHRASES) : TEXT_SERIALIZER
module Detailed_English : TEXT_SERIALIZER
module Pidgin_English : TEXT_SERIALIZER
module Rust : TEXT_SERIALIZER
module Haskell : TEXT_SERIALIZER
module Ada : TEXT_SERIALIZER
module OCaml : TEXT_SERIALIZER
