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

  val let_ :
    string -> target_personnel -> (target_personnel -> effect_t) -> effect_t

  val let_entity :
    target_entity ->
    (target_entity -> target_sector -> target_personnel -> effect_t) ->
    effect_t

  val ext : fx -> effect_t

  module CC : sig
    val zero : Int.non_negative Int.t
    val one : Int.non_negative Int.t
    val two : Int.non_negative Int.t
    val three : Int.non_negative Int.t
    val four : Int.non_negative Int.t
    val five : Int.non_negative Int.t
    val six : Int.non_negative Int.t
    val seven : Int.non_negative Int.t
    val eight : Int.non_negative Int.t
    val nine : Int.non_negative Int.t
    val ten : Int.non_negative Int.t
    val eleven : Int.non_negative Int.t
    val twelve : Int.non_negative Int.t
    val thirteen : Int.non_negative Int.t
    val fourteen : Int.non_negative Int.t
    val fifteen : Int.non_negative Int.t
    val sixteen : Int.non_negative Int.t
    val seventeen : Int.non_negative Int.t
    val eighteen : Int.non_negative Int.t
    val nineteen : Int.non_negative Int.t
    val twenty : Int.non_negative Int.t
  end

  module Timer : sig
    val one : Int.positive Int.t
    val two : Int.positive Int.t
    val three : Int.positive Int.t
    val four : Int.positive Int.t
    val five : Int.positive Int.t
    val six : Int.positive Int.t
    val seven : Int.positive Int.t
    val eight : Int.positive Int.t
    val nine : Int.positive Int.t
    val ten : Int.positive Int.t
    val eleven : Int.positive Int.t
    val twelve : Int.positive Int.t
    val thirteen : Int.positive Int.t
    val fourteen : Int.positive Int.t
    val fifteen : Int.positive Int.t
    val sixteen : Int.positive Int.t
    val seventeen : Int.positive Int.t
    val eighteen : Int.positive Int.t
    val nineteen : Int.positive Int.t
    val twenty : Int.positive Int.t
  end

  module Amount : sig
    val one : Int.positive Int.t
    val two : Int.positive Int.t
    val three : Int.positive Int.t
    val four : Int.positive Int.t
    val five : Int.positive Int.t
    val six : Int.positive Int.t
    val seven : Int.positive Int.t
    val eight : Int.positive Int.t
    val nine : Int.positive Int.t
    val ten : Int.positive Int.t
  end

  val this_personnel : target_personnel
  val this_personnel_sector : target_personnel

  val choose_personnel :
    ?in_sector:sector ->
    ?filter:personnel_filter ->
    ?chooser:chooser option ->
    unit ->
    target_personnel

  val choose_personnel_each_player :
    ?in_sector:sector -> unit -> target_personnel

  val choose_personnel_pair :
    ?in_sector:sector ->
    ?filter:personnel_filter ->
    ?chooser:chooser option ->
    unit ->
    target_personnel

  val all_personnel_in_sector : sector -> target_personnel
  val all_personnel : target_personnel
  val all_personnel_in_this_sector : target_personnel

  val all_personnel_with :
    ?in_sector:sector -> ?filter:personnel_filter -> unit -> target_personnel

  val entity_in_this_sector : target_entity
  val entity_in_sector : sector -> target_entity
  val choose_entity : ?filter:entity_filter -> unit -> target_entity
  val all_entities : ?filter:entity_filter -> unit -> target_entity
  val this_sector : target_sector
  val specific_sector : sector -> target_sector

  val choose_sector :
    ?filter:sector_filter -> ?chooser:chooser option -> unit -> target_sector

  val you : target_player
  val another_player : target_player
  val any_player : target_player
  val each_player : target_player
  val always : condition
  val never : condition
  val and_ : condition -> condition -> condition
  val or_ : condition -> condition -> condition
  val not_ : condition -> condition
  val personnel_has_min_cc : int -> personnel_filter
  val personnel_in_sector : sector -> personnel_filter

  val personnel_filter_and :
    personnel_filter -> personnel_filter -> personnel_filter

  val in_same_sector_as_this_personnel : personnel_filter
  val exclude_this_personnel : personnel_filter
  val other_personnel : personnel_filter
  val personnel_in_play : personnel_filter
  val personnel_controlled_by_active_player : personnel_filter
  val sector_other_than_source : sector_filter
  val personnel_count_in_sector : sector -> int -> condition

  val personnel_with_min_cc :
    sector -> min_count:int -> min_cc_each:int -> condition

  val total_cc_in_sector : sector -> total:int -> condition
  val total_cc_in_this_sector : total:int -> condition
  val entity_is_uncontained : entity_filter
  val sector_is_breached : sector -> condition

  val make_containment_requirement :
    check:condition -> Cards.containment_requirement

  val when_cc_would_reduce :
    ?exclude_source:cc_change_source -> target_personnel -> trigger_t

  val when_cc_would_reduce_not_from_spend : target_personnel -> trigger_t
  val trig_ext : trig -> trigger_t

  val delayed :
    window:timing_window -> scope:timing_scope -> then_do:effect_t -> effect_t

  val before_end_phase_step_1_this_round : effect_t -> effect_t

  val player_choice :
    player:target_player -> option_a:effect_t -> option_b:effect_t -> effect_t
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
