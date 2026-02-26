module Types = Core
open Core
open Targets
open Effects
open Cards

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

  val let_ :
    target_personnel -> (target_personnel -> effect_t) -> effect_t

  val let_entity :
    target_entity ->
    (target_entity -> target_sector -> target_personnel -> effect_t) ->
    effect_t

  val ext : fx -> effect_t

  val when_cc_would_reduce :
    ?exclude_source:Types.cc_change_source -> target_personnel -> trigger_t

  val when_cc_would_reduce_not_from_spend : target_personnel -> trigger_t
  val trig_ext : trig -> trigger_t

  val delayed :
    window:Types.timing_window ->
    scope:Types.timing_scope ->
    then_do:effect_t ->
    effect_t

  val before_end_phase_step_1_this_round : effect_t -> effect_t

  val player_choice :
    player:target_player -> option_a:effect_t -> option_b:effect_t -> effect_t

  val make_containment_requirement :
    check:condition -> Cards.containment_requirement
end

module Make (Ext : Division_intf.EXTENSION) :
  S with type fx = Ext.fx and type trig = Ext.trig and type div = Ext.div =
struct
  type fx = Ext.fx
  type trig = Ext.trig
  type div = Ext.div
  type effect_t = fx Effects.t
  type trigger_t = trig Abilities.trigger
  type ability_t = (fx, trig) Abilities.ability
  type card_t = (div, fx, trig) Cards.card

  let add_cc ~target ~amount = Core_effect (Add_CC { target; amount })
  let remove_cc ~target ~amount = Core_effect (Remove_CC { target; amount })
  let move_cc ~from ~to_ ~amount = Core_effect (Move_CC { from; to_; amount })

  let move_cc_between_pair ~pair ~amount =
    Core_effect (Move_CC_between_pair { pair; amount })

  let add_breach_marker ~target ~amount =
    Core_effect (Add_breach_marker { target; amount })

  let remove_breach_marker ~target ~amount =
    Core_effect (Remove_breach_marker { target; amount })

  let reset_breach_markers ~target =
    Core_effect (Reset_breach_markers { target })

  let send_to_abyss target = Core_effect (Send_to_abyss { target })

  let move_personnel ~target ~to_sector =
    Core_effect (Move_personnel { target; to_sector })

  let flip_sector ~target ~state =
    Core_effect (Flip_sector { target; to_state = state })

  let draw ~player ~amount = Core_effect (Draw { player; amount })
  let discard ~player ~amount = Core_effect (Discard { player; amount })
  let discard_hand ~player = Core_effect (Discard_hand { player })

  let prevent_cc_loss ~target ~amount =
    Core_effect (Prevent_CC_loss { target; amount })

  let contain_entity ~target = Core_effect (Contain_entity { target })
  let log msg = Log msg
  let composite effects = Composite effects
  let noop = Noop
  let if_possible eff = If_possible eff

  let let_ target f =
    let bound = Bound { index = 0; kind = Bound_that } in
    Let { name = "Personnel"; target; body = f bound }

  let let_entity target f =
    let bound_entity = Targets.Bound_entity { index = 0 } in
    let bound_sector = Targets.Bound_entity_sector { index = 0 } in
    let bound_personnel = Bound { index = 0; kind = In_bound_entity_sector } in
    Effects.Let_entity
      {
        name = "Entity";
        target;
        body = f bound_entity bound_sector bound_personnel;
      }

  let ext e = Ext e

  let when_cc_would_reduce ?exclude_source target =
    Abilities.Core (Abilities.When_cc_would_reduce { target; exclude_source })

  let when_cc_would_reduce_not_from_spend target =
    when_cc_would_reduce ~exclude_source:Spend_CC target

  let trig_ext t = Abilities.Ext t
  let delayed ~window ~scope ~then_do = Delayed { window; scope; then_do }

  let before_end_phase_step_1_this_round then_do =
    delayed ~window:End_phase ~scope:This_round ~then_do

  let player_choice ~player ~option_a ~option_b =
    Player_choice { player; option_a; option_b }

  let make_containment_requirement ~check = { check }
end

module Core = Make (Division_intf.No_extension)

include (
  Core :
    S
      with type fx := No_ext.t
       and type trig := No_ext.t
       and type div := division)
