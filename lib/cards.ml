open Core
open Targets
open Abilities

type threat_level = Safe | Euclid | Keter | Titan
type containment_requirement = { check : condition }

type ('div, 'fx, 'trig_ext) personnel = {
  id : Card_id.t;
  name : string;
  division : 'div;
  lore : Lore.t option;
  flavor_text : string option;
  starting_cc : Int.non_negative Int.t;
  abilities : ('fx, 'trig_ext) ability list;
}

type ('div, 'fx) procedure = {
  id : Card_id.t;
  name : string;
  division : 'div;
  lore : Lore.t option;
  flavor_text : string option;
  card_effect : 'fx Effects.t;
}

type ('div, 'fx) event = {
  id : Card_id.t;
  name : string;
  division : 'div;
  lore : Lore.t option;
  flavor_text : string option;
  card_effect : 'fx Effects.t;
}

type ('div, 'fx) entity = {
  id : Card_id.t;
  name : string;
  division : 'div;
  lore : Lore.t option;
  flavor_text : string option;
  threat_level : threat_level;
  breach_timer : Int.positive Int.t;
  end_phase_effect : 'fx Effects.t;
  breach_effect : 'fx Effects.t;
  containment : containment_requirement;
}

type ('div, 'fx, 'trig_ext) card =
  | Personnel of ('div, 'fx, 'trig_ext) personnel
  | Procedure of ('div, 'fx) procedure
  | Event of ('div, 'fx) event
  | Entity of ('div, 'fx) entity

type core_personnel = (division, No_ext.t, No_ext.t) personnel
type core_procedure = (division, No_ext.t) procedure
type core_event = (division, No_ext.t) event
type core_entity = (division, No_ext.t) entity
type core_card = (division, No_ext.t, No_ext.t) card
