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

  val ( let* ) :
    Targets.target_personnel ->
    (Targets.target_personnel -> effect_t) ->
    effect_t
end

module type ENTITY_SYNTAX = sig
  type effect_t

  val ( let+ ) :
    Targets.target_entity ->
    (Targets.target_entity ->
    Targets.target_sector ->
    Targets.target_personnel ->
    effect_t) ->
    effect_t
end

module Make (E : Engine.S) : S with type effect_t = E.effect_t = struct
  type effect_t = E.effect_t

  let ( +@ ) target amount = E.add_cc ~target ~amount
  let ( -@ ) target amount = E.remove_cc ~target ~amount
  let ( &+ ) a b = E.composite [ a; b ]
  let ( ==> ) from to_ = E.move_cc ~from ~to_ ~amount:Int.Positive.one
  let ( --> ) target to_sector = E.move_personnel ~target ~to_sector
  let ( |+ ) target amount = E.add_breach_marker ~target ~amount
  let ( |- ) target amount = E.remove_breach_marker ~target ~amount
  let reset_breach_markers target = E.reset_breach_markers ~target
  let transfer amount from to_ = E.move_cc ~from ~to_ ~amount
  let me = Targets.this_personnel
  let my_sector = Targets.this_personnel_sector

  let anyone ?in_sector ?filter () =
    Targets.choose_personnel ?in_sector ?filter ()

  let each_player_personnel ?in_sector () =
    Targets.choose_personnel_each_player ?in_sector ()

  let controlled_by_you = Targets.personnel_controlled_by_active_player

  let anyone_else () =
    Targets.choose_personnel ~filter:Targets.other_personnel ()

  let others_in_play =
    Targets.all_personnel_with
      ~filter:
        (Targets.personnel_filter_and Targets.exclude_this_personnel
           Targets.personnel_in_play)
      ()

  let everyone = Targets.all_personnel
  let everyone_in sector = Targets.all_personnel_in_sector sector
  let everyone_here = Targets.all_personnel_in_this_sector

  let each_personnel_in_play =
    All_personnel { in_sector = None; filter = Some Targets.personnel_in_play }

  let entity_here = Targets.entity_in_this_sector
  let entity_in sector = Targets.entity_in_sector sector
  let any_entity ?filter () = Targets.choose_entity ?filter ()
  let all_entities ?filter () = Targets.all_entities ?filter ()
  let contain target = E.contain_entity ~target
  let here = Targets.this_sector
  let to_sector sector = Targets.specific_sector sector
  let any_sector ?filter () = Targets.choose_sector ?filter ()
  let other_than_source_sector = Targets.sector_other_than_source
  let all_sectors = [ Alpha; Beta; Gamma; Lambda ]

  let secure_all_sectors =
    E.composite
      (List.map
         (fun sector ->
           E.flip_sector ~target:(Targets.specific_sector sector) ~state:Secure)
         all_sectors)

  let each_player = Targets.each_player
end

module Make_syntax (E : Engine.S) : SYNTAX with type effect_t = E.effect_t =
struct
  type effect_t = E.effect_t

  let ( let* ) target f = E.let_ target f
end

module Make_entity_syntax (E : Engine.S) :
  ENTITY_SYNTAX with type effect_t = E.effect_t = struct
  type effect_t = E.effect_t

  let ( let+ ) target f = E.let_entity target f
end

module Conditions = struct
  let ( &&? ) = Condition.and_
  let ( ||? ) = Condition.or_
  let ( !? ) = Condition.not_
  let always = Condition.always
  let never = Condition.never
  let sector_breached = Condition.sector_is_breached
  let personnel_count = Condition.personnel_count_in_sector
  let personnel_with_cc = Condition.personnel_with_min_cc
  let total_cc = Condition.total_cc_in_sector
  let total_cc_here = Condition.total_cc_in_this_sector
end

module Core_ = Make (Engine.Core)
module Syntax = Make_syntax (Engine.Core)
module Entity_syntax = Make_entity_syntax (Engine.Core)
include Core_

let one = Int.Positive.one
let two = Int.Positive.two
let three = Int.Positive.three
let four = Int.Positive.four
let five = Int.Positive.five
let six = Int.Positive.six
let seven = Int.Positive.seven
let eight = Int.Positive.eight
let nine = Int.Positive.nine
let ten = Int.Positive.ten

module CC = Int
module Timer = Int.Positive

let once_per_round = Once_per_round
let safe = Safe
let euclid = Euclid
let keter = Keter
let titan = Titan
let seq effects = Engine.Core.composite effects
let draw ?(player = Targets.you) amount = Engine.Core.draw ~player ~amount
let discard ?(player = Targets.you) amount = Engine.Core.discard ~player ~amount
let discard_hand ?(player = Targets.you) () = Engine.Core.discard_hand ~player
let shield target amount = Engine.Core.prevent_cc_loss ~target ~amount
let secure target = Engine.Core.flip_sector ~target ~state:Secure
let breach_sector target = Engine.Core.flip_sector ~target ~state:Breached
let abyss target = Engine.Core.send_to_abyss target
let contain target = Engine.Core.contain_entity ~target

let at_end_phase eff =
  Engine.Core.delayed ~window:End_phase ~scope:This_round ~then_do:eff

let at_end_round eff =
  Engine.Core.delayed ~window:End_of_round ~scope:This_round ~then_do:eff

let any_personnel_pair ?in_sector ?filter () =
  Targets.choose_personnel_pair ?in_sector ?filter ()

let transfer_between_pair amount pair =
  Engine.Core.move_cc_between_pair ~pair ~amount

let if_possible eff = Engine.Core.if_possible eff
let noop = Engine.Core.noop
let you = Targets.you
let another_player = Targets.another_player
let any_player = Targets.any_player

let player_chooses ~player ~option_a ~option_b =
  Engine.Core.player_choice ~player ~option_a ~option_b

let each_player_chooses ~option_a ~option_b =
  Engine.Core.player_choice ~player:Each_player ~option_a ~option_b

let passive ?limit ?when_ eff : core_ability =
  Passive { id = None; limit; condition = when_; card_effect = eff }

let activated ~cost ?when_ eff : core_ability =
  Activated { id = None; cc_cost = cost; condition = when_; card_effect = eff }

let triggered ~on ?limit ?(optional = false) ?when_ eff : core_ability =
  let optionality = if optional then Optional else Mandatory in
  Triggered
    {
      id = None;
      trigger = Core on;
      limit;
      optionality;
      condition = when_;
      card_effect = eff;
    }

let burnout eff : core_ability = Burnout { id = None; card_effect = eff }

let on_deploy ?(optional = false) ?when_ eff : core_ability =
  triggered ~on:When_deployed ~optional ?when_ eff

let on_end_round ?limit ?(optional = false) ?when_ eff : core_ability =
  triggered ~on:End_of_round ?limit ~optional ?when_ eff

let on_end_phase ?limit ?(optional = false) ?when_ eff : core_ability =
  triggered ~on:End_phase ?limit ~optional ?when_ eff

let on_breach sector ?limit ?(optional = false) ?when_ eff : core_ability =
  triggered ~on:(When_sector_breached sector) ?limit ~optional ?when_ eff

let on_entity_effect ?in_sector ?limit ?(optional = false) ?when_ eff :
    core_ability =
  triggered ~on:(When_entity_effect { in_sector }) ?limit ~optional ?when_ eff

let on_deployed_in sector ?limit ?(optional = false) ?when_ eff : core_ability =
  triggered ~on:(When_personnel_deployed_in_sector sector) ?limit ~optional
    ?when_ eff

let on_cc_loss ?(not_from_spend = false) target ?limit ?(optional = false)
    ?when_ eff : core_ability =
  let exclude_source = if not_from_spend then Some Spend_CC else None in
  triggered
    ~on:(When_cc_would_reduce { target; exclude_source })
    ?limit ~optional ?when_ eff

let personnel id name division ?lore ?flavor_text ~cc abilities : core_personnel
    =
  {
    id = Card_id.of_string id;
    name;
    division;
    lore;
    flavor_text;
    starting_cc = cc;
    abilities;
  }

let procedure id name division ?lore ?flavor_text eff : core_procedure =
  {
    id = Card_id.of_string id;
    name;
    division;
    lore;
    flavor_text;
    card_effect = eff;
  }

let event id name division ?lore ?flavor_text eff : core_event =
  {
    id = Card_id.of_string id;
    name;
    division;
    lore;
    flavor_text;
    card_effect = eff;
  }

let entity id name division ?lore ?flavor_text ~threat ~timer ~on_end_phase
    ~on_breach ~contained () : core_entity =
  {
    id = Card_id.of_string id;
    name;
    division;
    lore;
    flavor_text;
    threat_level = threat;
    breach_timer = timer;
    end_phase_effect = on_end_phase;
    breach_effect = on_breach;
    containment = { check = contained };
  }
