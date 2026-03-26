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

type 'div core_personnel =
  ('div Core.division_tag, No_ext.t, No_ext.t) personnel

type 'div core_procedure = ('div Core.division_tag, No_ext.t) procedure
type 'div core_event = ('div Core.division_tag, No_ext.t) event
type 'div core_entity = ('div Core.division_tag, No_ext.t) entity
type 'div core_card = ('div Core.division_tag, No_ext.t, No_ext.t) card

type any_core_personnel =
  | Any_core_personnel : 'div core_personnel -> any_core_personnel

type any_core_procedure =
  | Any_core_procedure : 'div core_procedure -> any_core_procedure

type any_core_event = Any_core_event : 'div core_event -> any_core_event
type any_core_entity = Any_core_entity : 'div core_entity -> any_core_entity
type any_core_card = Any_core_card : 'div core_card -> any_core_card

val pack_core_personnel : 'div core_personnel -> any_core_personnel
val pack_core_procedure : 'div core_procedure -> any_core_procedure
val pack_core_event : 'div core_event -> any_core_event
val pack_core_entity : 'div core_entity -> any_core_entity
val pack_core_card : 'div core_card -> any_core_card
val pack_core_cards : 'div core_card list -> any_core_card list

val erase_core_personnel :
  'div core_personnel -> (division, No_ext.t, No_ext.t) personnel

val erase_core_procedure : 'div core_procedure -> (division, No_ext.t) procedure
val erase_core_event : 'div core_event -> (division, No_ext.t) event
val erase_core_entity : 'div core_entity -> (division, No_ext.t) entity
