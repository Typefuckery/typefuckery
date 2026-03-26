module Id : sig
  type player = int
  type obj = int
end

module Card_id : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

type division = Ada | Haskell | OCaml | Rust | Institute
type ada
type haskell
type ocaml_div
type rust
type institute

type _ division_tag =
  | Ada_div : ada division_tag
  | Haskell_div : haskell division_tag
  | OCaml_div : ocaml_div division_tag
  | Rust_div : rust division_tag
  | Institute_div : institute division_tag

val division_of_tag : 'div division_tag -> division
val division_prefix : 'div division_tag -> string
val card_id_of_slug : 'div division_tag -> string -> Card_id.t
val set_id_of_division : 'div division_tag -> string

type sector = Alpha | Beta | Lambda | Gamma
type sector_state = Secure | Breached
type zone = Hand | Battlefield of sector | Abyss | Stack | Deck
type cc_change_source = Spend_CC | Card_Effect | Entity_Effect | Breach
type timing_window = End_phase | End_of_round
type timing_scope = This_round | Future_round
type chooser = Active_player | Starting_player | Controller

val sector_rank : sector -> int
val compare_sector : sector -> sector -> int

module SectorMap : Map.S with type key = sector
module ObjMap : Map.S with type key = Id.obj

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
