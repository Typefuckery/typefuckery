module Id = struct
  type player = int
  type obj = int
end

module Card_id = struct
  type t = string

  let of_string s =
    match String.split_on_char ':' s with
    | [ set; slug ] when set <> "" && slug <> "" -> s
    | _ ->
        invalid_arg
          (Printf.sprintf
             "Card_id.of_string: invalid format %S, expected 'set:slug'" s)

  let to_string t = t
  let compare = String.compare
  let equal = String.equal
end

type division = Ada | Haskell | OCaml | Rust | Institute
type sector = Alpha | Beta | Lambda | Gamma
type sector_state = Secure | Breached
type zone = Hand | Battlefield of sector | Abyss | Stack | Deck
type cc_change_source = Spend_CC | Card_Effect | Entity_Effect | Breach
type timing_window = End_phase | End_of_round
type timing_scope = This_round | Future_round
type chooser = Active_player | Starting_player | Controller

let sector_rank = function Alpha -> 0 | Beta -> 1 | Lambda -> 2 | Gamma -> 3
let compare_sector s1 s2 = Stdlib.Int.compare (sector_rank s1) (sector_rank s2)

module SectorMap = Map.Make (struct
  type t = sector

  let compare = compare_sector
end)

module ObjMap = Map.Make (struct
  type t = Id.obj

  let compare = Stdlib.compare
end)

type ctx = {
  sectors : sector_state SectorMap.t;
  personnel : personnel_state ObjMap.t;
  entities : entity_state SectorMap.t;
  breach_markers : int SectorMap.t;
  custom_state : (string * string) list;
  source_obj_id : Id.obj option;
  source_entity_sector : sector option;
  active_player : Id.player;
}

and personnel_state = {
  id : Id.obj;
  controller : Id.player;
  card_id : Card_id.t;
  sector : sector;
  current_cc : int;
  activated_this_round : bool;
  zone : zone;
}

and entity_state = {
  entity_card_id : Card_id.t;
  entity_sector : sector;
  contained : bool;
  breach_markers_remaining : int;
}
