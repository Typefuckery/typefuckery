open Core
open Targets

type ability_limit = Once_per_round
type optionality = Optional | Mandatory

type core_trigger =
  | End_of_round
  | End_phase
  | When_cc_would_reduce of {
      target : target_personnel;
      exclude_source : cc_change_source option;
    }
  | When_deployed
  | When_personnel_deployed_in_sector of sector
  | When_entity_effect of { in_sector : sector option }
  | When_sector_breached of sector

type 'trig_ext trigger = Core of core_trigger | Ext of 'trig_ext
type trigger_timing = No_ext.t trigger

type 'fx passive_ability = {
  id : string option;
  limit : ability_limit option;
  condition : condition option;
  card_effect : 'fx Effects.t;
}

type 'fx activated_ability = {
  id : string option;
  cc_cost : Int.non_negative Int.t;
  condition : condition option;
  card_effect : 'fx Effects.t;
}

type ('fx, 'trig_ext) triggered_ability = {
  id : string option;
  trigger : 'trig_ext trigger;
  limit : ability_limit option;
  optionality : optionality;
  condition : condition option;
  card_effect : 'fx Effects.t;
}

type 'fx burnout_ability = { id : string option; card_effect : 'fx Effects.t }

type ('fx, 'trig_ext) ability =
  | Passive of 'fx passive_ability
  | Activated of 'fx activated_ability
  | Triggered of ('fx, 'trig_ext) triggered_ability
  | Burnout of 'fx burnout_ability

type core_passive_ability = No_ext.t passive_ability
type core_activated_ability = No_ext.t activated_ability
type core_triggered_ability = (No_ext.t, No_ext.t) triggered_ability
type core_burnout_ability = No_ext.t burnout_ability
type core_ability = (No_ext.t, No_ext.t) ability
type passive = No_ext.t passive_ability
type activated = No_ext.t activated_ability
type triggered = (No_ext.t, No_ext.t) triggered_ability
type burnout = No_ext.t burnout_ability
