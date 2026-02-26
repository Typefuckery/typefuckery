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

let eval (cond : 'ext t) (ctx : ctx) : bool =
  let rec eval_kind : 'ext kind -> bool = function
    | Core_cond Always -> true
    | Core_cond Never -> false
    | Core_cond (Sector_breached sector) -> (
        match SectorMap.find_opt sector ctx.sectors with
        | Some Breached -> true
        | _ -> false)
    | Core_cond (Personnel_count { sector; min_count }) ->
        let count =
          ObjMap.fold
            (fun _ p acc -> if p.sector = sector then acc + 1 else acc)
            ctx.personnel 0
        in
        count >= min_count
    | Core_cond (Personnel_with_min_cc { sector; min_count; min_cc }) ->
        let matching =
          ObjMap.fold
            (fun _ p acc ->
              if p.sector = sector && p.current_cc >= min_cc then acc + 1
              else acc)
            ctx.personnel 0
        in
        matching >= min_count
    | Core_cond (Total_cc_in_sector { sector; total }) ->
        let sum =
          ObjMap.fold
            (fun _ p acc ->
              if p.sector = sector then acc + p.current_cc else acc)
            ctx.personnel 0
        in
        sum = total
    | Core_cond (Total_cc_in_this_sector { total }) -> (
        match ctx.source_entity_sector with
        | None -> false
        | Some sector ->
            let sum =
              ObjMap.fold
                (fun _ p acc ->
                  if p.sector = sector then acc + p.current_cc else acc)
                ctx.personnel 0
            in
            sum = total)
    | Ext _ ->
        failwith "Condition.eval: extension conditions require custom evaluator"
    | Not k -> not (eval_kind k)
    | And (k1, k2) -> eval_kind k1 && eval_kind k2
    | Or (k1, k2) -> eval_kind k1 || eval_kind k2
  in
  eval_kind cond.kind

let always : core = { kind = Core_cond Always }
let never : core = { kind = Core_cond Never }
let and_ (c1 : core) (c2 : core) : core = { kind = And (c1.kind, c2.kind) }
let or_ (c1 : core) (c2 : core) : core = { kind = Or (c1.kind, c2.kind) }
let not_ (c : core) : core = { kind = Not c.kind }

let sector_is_breached sector : core =
  { kind = Core_cond (Sector_breached sector) }

let personnel_count_in_sector sector min_count : core =
  { kind = Core_cond (Personnel_count { sector; min_count }) }

let personnel_with_min_cc sector ~min_count ~min_cc_each : core =
  {
    kind =
      Core_cond
        (Personnel_with_min_cc { sector; min_count; min_cc = min_cc_each });
  }

let total_cc_in_sector sector ~total : core =
  { kind = Core_cond (Total_cc_in_sector { sector; total }) }

let total_cc_in_this_sector ~total : core =
  { kind = Core_cond (Total_cc_in_this_sector { total }) }

let kind (c : 'ext t) : 'ext kind = c.kind
