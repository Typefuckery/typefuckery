open Core

type condition = Condition.core
type personnel_filter_tag = Other | In_play | Generic

type personnel_filter_kind =
  | Has_min_cc of int
  | In_sector of sector
  | Filter_and of personnel_filter_kind * personnel_filter_kind
  | In_same_sector_as_this
  | Exclude_this
  | Other_in_sector
  | Filter_in_play
  | Controlled_by_active_player

type personnel_filter = {
  check : ctx -> Id.obj -> bool;
  kind : personnel_filter_kind;
  tag : personnel_filter_tag;
}

type entity_filter_kind = Uncontained

type entity_filter = {
  check : ctx -> sector -> entity_state -> bool;
  kind : entity_filter_kind;
}

type sector_filter_kind = Sector_filter_placeholder | Other_than_source_sector

type sector_filter = {
  check : ctx -> sector -> bool;
  kind : sector_filter_kind;
}

type bound_kind =
  | Bound_that
  | Each_personnel_in_this_sector
  | In_bound_entity_sector

type target_player = You | Another_player | Any_player | Each_player

type target_personnel =
  | This_personnel
  | This_personnel_sector
  | Choose_personnel_each_player of { in_sector : sector option }
  | Choose_personnel of {
      in_sector : sector option;
      filter : personnel_filter option;
      chooser : chooser option;
    }
  | Choose_personnel_pair of {
      in_sector : sector option;
      filter : personnel_filter option;
      chooser : chooser option;
    }
  | All_personnel of {
      in_sector : sector option;
      filter : personnel_filter option;
    }
  | Bound of { index : int; kind : bound_kind }

type target_entity =
  | Entity_in_this_sector
  | Entity_in_sector of sector
  | Choose_entity of { filter : entity_filter option }
  | All_entities of { filter : entity_filter option }
  | Bound_entity of { index : int }

type target_sector =
  | This_sector
  | Specific of sector
  | Choose_sector of { filter : sector_filter option; chooser : chooser option }
  | Bound_entity_sector of { index : int }

let this_personnel = This_personnel
let this_personnel_sector = This_personnel_sector

let choose_personnel ?in_sector ?filter ?chooser () =
  Choose_personnel { in_sector; filter; chooser }

let choose_personnel_each_player ?in_sector () =
  Choose_personnel_each_player { in_sector }

let choose_personnel_pair ?in_sector ?filter ?chooser () =
  Choose_personnel_pair { in_sector; filter; chooser }

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

let choose_sector ?filter ?chooser () = Choose_sector { filter; chooser }

let you : target_player = You
let another_player : target_player = Another_player
let any_player : target_player = Any_player
let each_player : target_player = Each_player

let source_personnel_sector (ctx : ctx) : sector option =
  Option.bind ctx.source_obj_id (fun obj_id ->
      Option.map (fun p -> p.sector) (ObjMap.find_opt obj_id ctx.personnel))

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
                p.sector = source_sector && ctx.source_obj_id <> Some obj_id));
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

let entity_is_uncontained : entity_filter =
  {
    check = (fun _ctx _sector entity_state -> not entity_state.contained);
    kind = Uncontained;
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

let personnel_filter_kind (f : personnel_filter) = f.kind
let personnel_filter_tag (f : personnel_filter) = f.tag
let personnel_filter_check (f : personnel_filter) = f.check
let entity_filter_kind (f : entity_filter) = f.kind
let entity_filter_check (f : entity_filter) = f.check
let sector_filter_kind (f : sector_filter) = f.kind
let sector_filter_check (f : sector_filter) = f.check


