open Core
open Targets
open Abilities
open Cards

module type S = sig
  type effect_t

  val ( +@ ) : target_personnel -> Int.positive Int.t -> effect_t
  val ( -@ ) : target_personnel -> Int.positive Int.t -> effect_t
  val ( &+ ) : effect_t -> effect_t -> effect_t
  val ( ==> ) : target_personnel -> target_personnel -> effect_t
  val ( --> ) : target_personnel -> target_sector -> effect_t
  val ( |+ ) : target_entity -> Int.positive Int.t -> effect_t
  val ( |- ) : target_entity -> Int.positive Int.t -> effect_t
  val reset_breach_markers : target_entity -> effect_t

  val transfer :
    Int.positive Int.t -> target_personnel -> target_personnel -> effect_t

  val me : target_personnel
  val my_sector : target_personnel

  val anyone :
    ?in_sector:sector -> ?filter:personnel_filter -> unit -> target_personnel

  val any_personnel :
    ?in_sector:sector -> ?filter:personnel_filter -> unit -> target_personnel

  val each_player_personnel : ?in_sector:sector -> unit -> target_personnel
  val controlled_by_you : personnel_filter
  val anyone_else : unit -> target_personnel
  val others_in_play : target_personnel
  val everyone : target_personnel
  val everyone_in : sector -> target_personnel
  val everyone_here : target_personnel
  val entity_here : target_entity
  val entity_in : sector -> target_entity
  val any_entity : ?filter:entity_filter -> unit -> target_entity
  val all_entities : ?filter:entity_filter -> unit -> target_entity
  val contain : target_entity -> effect_t
  val each_personnel_in_play : target_personnel
  val here : target_sector
  val to_sector : sector -> target_sector
  val any_sector : ?filter:sector_filter -> unit -> target_sector
  val other_than_source_sector : sector_filter
  val all_sectors : sector list
  val secure_all_sectors : effect_t
  val each_player : target_player
end

module type SYNTAX = sig
  type effect_t

  val ( let* ) : target_personnel -> (target_personnel -> effect_t) -> effect_t
end

module type ENTITY_SYNTAX = sig
  type effect_t

  val ( let+ ) :
    target_entity ->
    (target_entity -> target_sector -> target_personnel -> effect_t) ->
    effect_t
end

module Make (E : Engine.S) : S with type effect_t = E.effect_t
module Make_syntax (E : Engine.S) : SYNTAX with type effect_t = E.effect_t

module Make_entity_syntax (E : Engine.S) :
  ENTITY_SYNTAX with type effect_t = E.effect_t

module Conditions : sig
  val ( &&? ) : condition -> condition -> condition
  val ( ||? ) : condition -> condition -> condition
  val ( !? ) : condition -> condition
  val always : condition
  val never : condition
  val sector_breached : sector -> condition
  val personnel_count : sector -> int -> condition

  val personnel_with_cc :
    sector -> min_count:int -> min_cc_each:int -> condition

  val total_cc : sector -> total:int -> condition
  val total_cc_here : total:int -> condition
end

module Core_ : S with type effect_t = Engine.Core.effect_t
module Syntax : SYNTAX with type effect_t = Effects.core
module Entity_syntax : ENTITY_SYNTAX with type effect_t = Effects.core

val ( +@ ) : target_personnel -> Int.positive Int.t -> Effects.core
val ( -@ ) : target_personnel -> Int.positive Int.t -> Effects.core
val ( &+ ) : Effects.core -> Effects.core -> Effects.core
val ( ==> ) : target_personnel -> target_personnel -> Effects.core
val ( --> ) : target_personnel -> target_sector -> Effects.core
val ( |+ ) : target_entity -> Int.positive Int.t -> Effects.core
val ( |- ) : target_entity -> Int.positive Int.t -> Effects.core
val reset_breach_markers : target_entity -> Effects.core

val transfer :
  Int.positive Int.t -> target_personnel -> target_personnel -> Effects.core

val me : target_personnel
val my_sector : target_personnel

val anyone :
  ?in_sector:sector -> ?filter:personnel_filter -> unit -> target_personnel

val any_personnel :
  ?in_sector:sector -> ?filter:personnel_filter -> unit -> target_personnel

val each_player_personnel : ?in_sector:sector -> unit -> target_personnel
val controlled_by_you : personnel_filter
val anyone_else : unit -> target_personnel
val others_in_play : target_personnel
val everyone : target_personnel
val everyone_in : sector -> target_personnel
val everyone_here : target_personnel
val entity_here : target_entity
val entity_in : sector -> target_entity
val any_entity : ?filter:entity_filter -> unit -> target_entity
val all_entities : ?filter:entity_filter -> unit -> target_entity
val each_personnel_in_play : target_personnel
val here : target_sector
val to_sector : sector -> target_sector
val any_sector : ?filter:sector_filter -> unit -> target_sector
val other_than_source_sector : sector_filter
val all_sectors : sector list
val secure_all_sectors : Effects.core
val each_player : target_player
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

val once_per_round : ability_limit
val safe : threat_level
val euclid : threat_level
val keter : threat_level
val titan : threat_level
val seq : Effects.core list -> Effects.core
val draw : ?player:target_player -> Int.positive Int.t -> Effects.core
val discard : ?player:target_player -> Int.positive Int.t -> Effects.core
val discard_hand : ?player:target_player -> unit -> Effects.core
val shield : target_personnel -> Int.positive Int.t -> Effects.core
val secure : target_sector -> Effects.core
val breach_sector : target_sector -> Effects.core
val abyss : target_personnel -> Effects.core
val contain : target_entity -> Effects.core
val at_end_phase : Effects.core -> Effects.core
val at_end_round : Effects.core -> Effects.core

val any_personnel_pair :
  ?in_sector:sector -> ?filter:personnel_filter -> unit -> target_personnel

val transfer_between_pair :
  Int.positive Int.t -> target_personnel -> Effects.core

val if_possible : Effects.core -> Effects.core
val noop : Effects.core
val you : target_player
val another_player : target_player
val any_player : target_player

val player_chooses :
  player:target_player ->
  option_a:Effects.core ->
  option_b:Effects.core ->
  Effects.core

val each_player_chooses :
  option_a:Effects.core -> option_b:Effects.core -> Effects.core

val passive :
  ?limit:ability_limit -> ?when_:condition -> Effects.core -> core_ability

val activated :
  cost:Int.non_negative Int.t ->
  ?when_:condition ->
  Effects.core ->
  core_ability

val triggered :
  on:core_trigger ->
  ?limit:ability_limit ->
  ?optional:bool ->
  ?when_:condition ->
  Effects.core ->
  core_ability

val burnout : Effects.core -> core_ability

val on_deploy :
  ?optional:bool -> ?when_:condition -> Effects.core -> core_ability

val on_end_round :
  ?limit:ability_limit ->
  ?optional:bool ->
  ?when_:condition ->
  Effects.core ->
  core_ability

val on_end_phase :
  ?limit:ability_limit ->
  ?optional:bool ->
  ?when_:condition ->
  Effects.core ->
  core_ability

val on_breach :
  sector ->
  ?limit:ability_limit ->
  ?optional:bool ->
  ?when_:condition ->
  Effects.core ->
  core_ability

val on_entity_effect :
  ?in_sector:sector ->
  ?limit:ability_limit ->
  ?optional:bool ->
  ?when_:condition ->
  Effects.core ->
  core_ability

val on_deployed_in :
  sector ->
  ?limit:ability_limit ->
  ?optional:bool ->
  ?when_:condition ->
  Effects.core ->
  core_ability

val on_cc_loss :
  ?not_from_spend:bool ->
  target_personnel ->
  ?limit:ability_limit ->
  ?optional:bool ->
  ?when_:condition ->
  Effects.core ->
  core_ability

val personnel :
  string ->
  string ->
  division ->
  ?lore:Lore.t ->
  ?flavor_text:string ->
  cc:Int.non_negative Int.t ->
  core_ability list ->
  core_personnel

val procedure :
  string ->
  string ->
  division ->
  ?lore:Lore.t ->
  ?flavor_text:string ->
  Effects.core ->
  core_procedure

val event :
  string ->
  string ->
  division ->
  ?lore:Lore.t ->
  ?flavor_text:string ->
  Effects.core ->
  core_event

val entity :
  string ->
  string ->
  division ->
  ?lore:Lore.t ->
  ?flavor_text:string ->
  threat:threat_level ->
  timer:Int.positive Int.t ->
  on_end_phase:Effects.core ->
  on_breach:Effects.core ->
  contained:condition ->
  unit ->
  core_entity
