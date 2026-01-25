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
    ?chooser:Types.chooser option ->
    unit ->
    target_personnel

  val choose_personnel_each_player :
    ?in_sector:sector -> unit -> target_personnel

  val choose_personnel_pair :
    ?in_sector:sector ->
    ?filter:personnel_filter ->
    ?chooser:Types.chooser option ->
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
    ?filter:sector_filter ->
    ?chooser:Types.chooser option ->
    unit ->
    target_sector

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
end

module Make_non_negative_constants (M : sig
  val make : int -> Int.non_negative Int.t
end) =
struct
  let zero = M.make 0
  let one = M.make 1
  let two = M.make 2
  let three = M.make 3
  let four = M.make 4
  let five = M.make 5
  let six = M.make 6
  let seven = M.make 7
  let eight = M.make 8
  let nine = M.make 9
  let ten = M.make 10
  let eleven = M.make 11
  let twelve = M.make 12
  let thirteen = M.make 13
  let fourteen = M.make 14
  let fifteen = M.make 15
  let sixteen = M.make 16
  let seventeen = M.make 17
  let eighteen = M.make 18
  let nineteen = M.make 19
  let twenty = M.make 20
end

module Make_positive_constants (M : sig
  val make : int -> Int.positive Int.t
end) =
struct
  let one = M.make 1
  let two = M.make 2
  let three = M.make 3
  let four = M.make 4
  let five = M.make 5
  let six = M.make 6
  let seven = M.make 7
  let eight = M.make 8
  let nine = M.make 9
  let ten = M.make 10
  let eleven = M.make 11
  let twelve = M.make 12
  let thirteen = M.make 13
  let fourteen = M.make 14
  let fifteen = M.make 15
  let sixteen = M.make 16
  let seventeen = M.make 17
  let eighteen = M.make 18
  let nineteen = M.make 19
  let twenty = M.make 20
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

  let let_ _name target f =
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

  module CC = Make_non_negative_constants (struct
    let make = Int.non_negative_exn
  end)

  module Timer = Make_positive_constants (struct
    let make = Int.positive_exn
  end)

  module Amount = Make_positive_constants (struct
    let make = Int.positive_exn
  end)

  let this_personnel = This_personnel
  let this_personnel_sector = This_personnel_sector

  let choose_personnel ?in_sector ?filter ?chooser () =
    Choose_personnel
      { in_sector; filter; chooser = Option.value chooser ~default:None }

  let choose_personnel_each_player ?in_sector () =
    Choose_personnel_each_player { in_sector }

  let choose_personnel_pair ?in_sector ?filter ?chooser () =
    Choose_personnel_pair
      { in_sector; filter; chooser = Option.value chooser ~default:None }

  let all_personnel_in_sector in_sector =
    All_personnel { in_sector = Some in_sector; filter = None }

  let all_personnel = All_personnel { in_sector = None; filter = None }

  let all_personnel_in_this_sector =
    Bound { index = 0; kind = Each_personnel_in_this_sector }

  let all_personnel_with ?in_sector ?filter () =
    All_personnel { in_sector; filter }

  let entity_in_this_sector = Entity_in_this_sector
  let entity_in_sector sector = Entity_in_sector sector
  let choose_entity ?filter () = Choose_entity { filter }
  let all_entities ?filter () = All_entities { filter }
  let this_sector = This_sector
  let specific_sector sector = Specific sector

  let choose_sector ?filter ?chooser () =
    Choose_sector { filter; chooser = Option.value chooser ~default:None }

  let you : target_player = You
  let another_player : target_player = Another_player
  let any_player : target_player = Any_player
  let each_player : target_player = Each_player
  let always : condition = { check = (fun _ -> true); kind = Core_cond Always }
  let never : condition = { check = (fun _ -> false); kind = Core_cond Never }

  let and_ (c1 : condition) (c2 : condition) : condition =
    {
      check = (fun ctx -> c1.check ctx && c2.check ctx);
      kind = And (c1.kind, c2.kind);
    }

  let or_ (c1 : condition) (c2 : condition) : condition =
    {
      check = (fun ctx -> c1.check ctx || c2.check ctx);
      kind = Or (c1.kind, c2.kind);
    }

  let not_ (c : condition) : condition =
    { check = (fun ctx -> not (c.check ctx)); kind = Not c.kind }

  let source_personnel_sector (ctx : ctx) : sector option =
    Option.bind ctx.source_obj_id (fun obj_id ->
        Option.map (fun p -> p.sector) (ObjMap.find_opt obj_id ctx.personnel))

  module Filters = struct
    let personnel_has_min_cc min_cc : personnel_filter =
      {
        check =
          (fun ctx obj_id ->
            match ObjMap.find_opt obj_id ctx.personnel with
            | None -> false
            | Some p -> p.current_cc >= min_cc);
        kind = Has_min_cc min_cc;
        tag = Generic;
      }

    let personnel_in_sector sector : personnel_filter =
      {
        check =
          (fun ctx obj_id ->
            match ObjMap.find_opt obj_id ctx.personnel with
            | None -> false
            | Some p -> p.sector = sector);
        kind = In_sector sector;
        tag = Generic;
      }

    let personnel_filter_and (f1 : personnel_filter) (f2 : personnel_filter) :
        personnel_filter =
      {
        check = (fun ctx obj_id -> f1.check ctx obj_id && f2.check ctx obj_id);
        kind = Filter_and (f1.kind, f2.kind);
        tag = Generic;
      }

    let in_same_sector_as_this_personnel : personnel_filter =
      {
        check =
          (fun ctx obj_id ->
            match source_personnel_sector ctx with
            | None -> true
            | Some source_sector -> (
                match ObjMap.find_opt obj_id ctx.personnel with
                | None -> false
                | Some p -> p.sector = source_sector));
        kind = In_same_sector_as_this;
        tag = Generic;
      }

    let exclude_this_personnel : personnel_filter =
      {
        check =
          (fun ctx obj_id ->
            match ctx.source_obj_id with
            | None -> true
            | Some source_id -> obj_id <> source_id);
        kind = Exclude_this;
        tag = Other;
      }

    let other_personnel : personnel_filter =
      {
        check =
          (fun ctx obj_id ->
            match source_personnel_sector ctx with
            | None -> true
            | Some source_sector -> (
                match ObjMap.find_opt obj_id ctx.personnel with
                | None -> false
                | Some p ->
                    p.sector = source_sector && ctx.source_obj_id <> Some obj_id
                ));
        kind = Other_in_sector;
        tag = Other;
      }

    let personnel_in_play : personnel_filter =
      {
        check =
          (fun ctx obj_id ->
            match ObjMap.find_opt obj_id ctx.personnel with
            | None -> false
            | Some p -> (
                match p.zone with
                | Battlefield _ -> true
                | Hand | Abyss | Stack | Deck -> false));
        kind = Filter_in_play;
        tag = In_play;
      }

    let personnel_controlled_by_active_player : personnel_filter =
      {
        check =
          (fun ctx obj_id ->
            match ObjMap.find_opt obj_id ctx.personnel with
            | None -> false
            | Some p -> p.controller = ctx.active_player);
        kind = Controlled_by_active_player;
        tag = Generic;
      }

    let sector_other_than_source : sector_filter =
      {
        check =
          (fun ctx sector ->
            match source_personnel_sector ctx with
            | None -> true
            | Some source_sector -> sector <> source_sector);
        kind = Other_than_source_sector;
      }

    let personnel_count_in_sector sector min_count : condition =
      {
        check =
          (fun ctx ->
            let count =
              ObjMap.fold
                (fun _ p acc -> if p.sector = sector then acc + 1 else acc)
                ctx.personnel 0
            in
            count >= min_count);
        kind = Core_cond (Personnel_count { sector; min_count });
      }

    let personnel_with_min_cc sector ~min_count ~min_cc_each : condition =
      {
        check =
          (fun ctx ->
            let matching =
              ObjMap.fold
                (fun _ p acc ->
                  if p.sector = sector && p.current_cc >= min_cc_each then
                    acc + 1
                  else acc)
                ctx.personnel 0
            in
            matching >= min_count);
        kind =
          Core_cond
            (Personnel_with_min_cc { sector; min_count; min_cc = min_cc_each });
      }

    let total_cc_in_sector sector ~total : condition =
      {
        check =
          (fun ctx ->
            let sum =
              ObjMap.fold
                (fun _ p acc ->
                  if p.sector = sector then acc + p.current_cc else acc)
                ctx.personnel 0
            in
            sum = total);
        kind = Core_cond (Total_cc_in_sector { sector; total });
      }

    let total_cc_in_this_sector ~total : condition =
      {
        check =
          (fun ctx ->
            match ctx.source_entity_sector with
            | None -> false
            | Some sector ->
                let sum =
                  ObjMap.fold
                    (fun _ p acc ->
                      if p.sector = sector then acc + p.current_cc else acc)
                    ctx.personnel 0
                in
                sum = total);
        kind = Core_cond (Total_cc_in_this_sector { total });
      }

    let entity_is_uncontained : entity_filter =
      {
        check = (fun _ctx _sector entity_state -> not entity_state.contained);
        kind = Uncontained;
      }

    let sector_is_breached sector : condition =
      {
        check =
          (fun ctx ->
            match SectorMap.find_opt sector ctx.sectors with
            | Some Breached -> true
            | _ -> false);
        kind = Core_cond (Sector_breached sector);
      }
  end

  let personnel_has_min_cc = Filters.personnel_has_min_cc
  let personnel_in_sector = Filters.personnel_in_sector
  let personnel_filter_and = Filters.personnel_filter_and

  let in_same_sector_as_this_personnel =
    Filters.in_same_sector_as_this_personnel

  let exclude_this_personnel = Filters.exclude_this_personnel
  let other_personnel = Filters.other_personnel
  let personnel_in_play = Filters.personnel_in_play

  let personnel_controlled_by_active_player =
    Filters.personnel_controlled_by_active_player

  let sector_other_than_source = Filters.sector_other_than_source
  let personnel_count_in_sector = Filters.personnel_count_in_sector
  let personnel_with_min_cc = Filters.personnel_with_min_cc
  let total_cc_in_sector = Filters.total_cc_in_sector
  let total_cc_in_this_sector = Filters.total_cc_in_this_sector
  let entity_is_uncontained = Filters.entity_is_uncontained
  let sector_is_breached = Filters.sector_is_breached

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
