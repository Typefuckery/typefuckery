open Core

type core_kind =
  | Always
  | Never
  | Sector_breached of sector
  | Personnel_count of { sector : sector; min_count : int }
  | Personnel_with_min_cc of { sector : sector; min_count : int; min_cc : int }
  | Total_cc_in_sector of { sector : sector; total : int }
  | Total_cc_in_this_sector of { total : int }

type 'ext kind =
  | Core_cond of core_kind
  | Ext of 'ext
  | Not of 'ext kind
  | And of 'ext kind * 'ext kind
  | Or of 'ext kind * 'ext kind

type 'ext t = { kind : 'ext kind }
type core = No_ext.t t

val eval : 'ext t -> ctx -> bool
val always : core
val never : core
val and_ : core -> core -> core
val or_ : core -> core -> core
val not_ : core -> core
val sector_is_breached : sector -> core
val personnel_count_in_sector : sector -> int -> core
val personnel_with_min_cc : sector -> min_count:int -> min_cc_each:int -> core
val total_cc_in_sector : sector -> total:int -> core
val total_cc_in_this_sector : total:int -> core
val kind : 'ext t -> 'ext kind
