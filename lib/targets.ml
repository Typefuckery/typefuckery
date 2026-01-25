open Core

type core_condition_kind =
  | Always
  | Never
  | Sector_breached of sector
  | Personnel_count of { sector : sector; min_count : int }
  | Personnel_with_min_cc of { sector : sector; min_count : int; min_cc : int }
  | Total_cc_in_sector of { sector : sector; total : int }
  | Total_cc_in_this_sector of { total : int }

type 'ext condition_kind =
  | Core_cond of core_condition_kind
  | Ext of 'ext
  | Not of 'ext condition_kind
  | And of 'ext condition_kind * 'ext condition_kind
  | Or of 'ext condition_kind * 'ext condition_kind

type 'ext cond = { check : ctx -> bool; kind : 'ext condition_kind }
type condition = No_ext.t cond
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
