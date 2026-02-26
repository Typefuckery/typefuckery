open Core
open Targets

module type S = sig
  type fx
  type trig
  type div
  type effect_t = fx Effects.t
  type trigger_t = trig Abilities.trigger
  type ability_t = (fx, trig) Abilities.ability
  type card_t = (div, fx, trig) Cards.card

  val add_cc : target:target_personnel -> amount:Int.positive Int.t -> effect_t

  val remove_cc :
    target:target_personnel -> amount:Int.positive Int.t -> effect_t

  val move_cc :
    from:target_personnel ->
    to_:target_personnel ->
    amount:Int.positive Int.t ->
    effect_t

  val move_cc_between_pair :
    pair:target_personnel -> amount:Int.positive Int.t -> effect_t

  val add_breach_marker :
    target:target_entity -> amount:Int.positive Int.t -> effect_t

  val remove_breach_marker :
    target:target_entity -> amount:Int.positive Int.t -> effect_t

  val reset_breach_markers : target:target_entity -> effect_t
  val send_to_abyss : target_personnel -> effect_t

  val move_personnel :
    target:target_personnel -> to_sector:target_sector -> effect_t

  val flip_sector : target:target_sector -> state:sector_state -> effect_t
  val draw : player:target_player -> amount:Int.positive Int.t -> effect_t
  val discard : player:target_player -> amount:Int.positive Int.t -> effect_t
  val discard_hand : player:target_player -> effect_t

  val prevent_cc_loss :
    target:target_personnel -> amount:Int.positive Int.t -> effect_t

  val contain_entity : target:target_entity -> effect_t
  val log : string -> effect_t
  val composite : effect_t list -> effect_t
  val noop : effect_t
  val if_possible : effect_t -> effect_t

  val let_ : target_personnel -> (target_personnel -> effect_t) -> effect_t

  val let_entity :
    target_entity ->
    (target_entity -> target_sector -> target_personnel -> effect_t) ->
    effect_t

  val ext : fx -> effect_t

  val when_cc_would_reduce :
    ?exclude_source:cc_change_source -> target_personnel -> trigger_t

  val when_cc_would_reduce_not_from_spend : target_personnel -> trigger_t
  val trig_ext : trig -> trigger_t

  val delayed :
    window:timing_window -> scope:timing_scope -> then_do:effect_t -> effect_t

  val before_end_phase_step_1_this_round : effect_t -> effect_t

  val player_choice :
    player:target_player -> option_a:effect_t -> option_b:effect_t -> effect_t

  val make_containment_requirement :
    check:condition -> Cards.containment_requirement
end

module Make (Ext : Division_intf.EXTENSION) :
  S with type fx = Ext.fx and type trig = Ext.trig and type div = Ext.div

module Core :
  S with type fx = No_ext.t and type trig = No_ext.t and type div = division

include
  module type of Core
    with type fx := No_ext.t
     and type trig := No_ext.t
     and type div := division
