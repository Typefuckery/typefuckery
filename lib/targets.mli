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

type entity_filter_kind = Uncontained
type sector_filter_kind = Sector_filter_placeholder | Other_than_source_sector
type personnel_filter
type entity_filter
type sector_filter

val personnel_filter_kind : personnel_filter -> personnel_filter_kind
val personnel_filter_tag : personnel_filter -> personnel_filter_tag
val personnel_filter_check : personnel_filter -> ctx -> Id.obj -> bool
val entity_filter_kind : entity_filter -> entity_filter_kind
val entity_filter_check : entity_filter -> ctx -> sector -> entity_state -> bool
val sector_filter_kind : sector_filter -> sector_filter_kind
val sector_filter_check : sector_filter -> ctx -> sector -> bool

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
      chooser : Core.chooser option;
    }
  | Choose_personnel_pair of {
      in_sector : sector option;
      filter : personnel_filter option;
      chooser : Core.chooser option;
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
  | Choose_sector of {
      filter : sector_filter option;
      chooser : Core.chooser option;
    }
  | Bound_entity_sector of { index : int }

val this_personnel : target_personnel
val this_personnel_sector : target_personnel

val choose_personnel :
  ?in_sector:Core.sector ->
  ?filter:personnel_filter ->
  ?chooser:Core.chooser ->
  unit ->
  target_personnel

val choose_personnel_each_player :
  ?in_sector:Core.sector -> unit -> target_personnel

val choose_personnel_pair :
  ?in_sector:Core.sector ->
  ?filter:personnel_filter ->
  ?chooser:Core.chooser ->
  unit ->
  target_personnel

val all_personnel_in_sector : Core.sector -> target_personnel
val all_personnel : target_personnel
val all_personnel_in_this_sector : target_personnel

val all_personnel_with :
  ?in_sector:Core.sector -> ?filter:personnel_filter -> unit -> target_personnel

val entity_in_this_sector : target_entity
val entity_in_sector : Core.sector -> target_entity
val choose_entity : ?filter:entity_filter -> unit -> target_entity
val all_entities : ?filter:entity_filter -> unit -> target_entity
val this_sector : target_sector
val specific_sector : Core.sector -> target_sector

val choose_sector :
  ?filter:sector_filter -> ?chooser:Core.chooser -> unit -> target_sector

val you : target_player
val another_player : target_player
val any_player : target_player
val each_player : target_player
val personnel_has_min_cc : int -> personnel_filter
val personnel_in_sector : Core.sector -> personnel_filter

val personnel_filter_and :
  personnel_filter -> personnel_filter -> personnel_filter

val in_same_sector_as_this_personnel : personnel_filter
val exclude_this_personnel : personnel_filter
val other_personnel : personnel_filter
val personnel_in_play : personnel_filter
val personnel_controlled_by_active_player : personnel_filter
val entity_is_uncontained : entity_filter
val sector_other_than_source : sector_filter
