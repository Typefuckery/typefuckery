module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type set_id = string
type set_name = string
type card_type = Personnel | Procedure | Event | Entity

type registered_card = {
  card_id : Core.Card_id.t;
  name : string;
  card_type : card_type;
  set_id : set_id;
  set_name : set_name;
  rendered_text : string;
  card_data : Cards.core_card option;
}

type set_metadata = { id : set_id; name : set_name }
type set = { metadata : set_metadata; cards : registered_card list }

type error =
  [ `Set_id_already_registered of set_id
  | `Set_name_already_registered of set_name * set_id
  | `Duplicate_card_id_within_set of set_id * Core.Card_id.t * int
  | `Card_id_already_registered of Core.Card_id.t * set_id * set_id ]

type t = {
  sets_by_id : set StringMap.t;
  set_ids_by_name : set_id StringMap.t;
  cards_by_id : registered_card StringMap.t;
}

let empty : t =
  {
    sets_by_id = StringMap.empty;
    set_ids_by_name = StringMap.empty;
    cards_by_id = StringMap.empty;
  }

let card_id : type div fx trig. (div, fx, trig) Cards.card -> Core.Card_id.t =
  function
  | Cards.Personnel p -> p.id
  | Cards.Procedure p -> p.id
  | Cards.Event e -> e.id
  | Cards.Entity e -> e.id

let card_name : type div fx trig. (div, fx, trig) Cards.card -> string =
  function
  | Cards.Personnel p -> p.name
  | Cards.Procedure p -> p.name
  | Cards.Event e -> e.name
  | Cards.Entity e -> e.name

let card_type_of : type div fx trig. (div, fx, trig) Cards.card -> card_type =
  function
  | Cards.Personnel _ -> Personnel
  | Cards.Procedure _ -> Procedure
  | Cards.Event _ -> Event
  | Cards.Entity _ -> Entity

let list_sets (registry : t) : set_metadata list =
  registry.sets_by_id |> StringMap.bindings
  |> List.map (fun (_, s) -> s.metadata)

let find_set_by_id (registry : t) (set_id : set_id) : set option =
  StringMap.find_opt set_id registry.sets_by_id

let find_set_by_name (registry : t) (set_name : set_name) : set option =
  match StringMap.find_opt set_name registry.set_ids_by_name with
  | None -> None
  | Some set_id -> find_set_by_id registry set_id

let list_cards (registry : t) : registered_card list =
  registry.cards_by_id |> StringMap.bindings
  |> List.map (fun (_, entry) -> entry)

let find_card (registry : t) (id : Core.Card_id.t) : registered_card option =
  StringMap.find_opt (Core.Card_id.to_string id) registry.cards_by_id

let render_card (entry : registered_card)
    (ser : (module To_string.TEXT_SERIALIZER)) : string option =
  match entry.card_data with
  | None -> None
  | Some card ->
      let module S = (val ser : To_string.TEXT_SERIALIZER) in
      Some (S.card_to_string card)

let ( let* ) = Result.bind

let validate_set_id_unique (registry : t) (set_id : set_id) :
    (unit, error) result =
  if StringMap.mem set_id registry.sets_by_id then
    Error (`Set_id_already_registered set_id)
  else Ok ()

let validate_set_name_unique (registry : t) (set_name : set_name) :
    (unit, error) result =
  match StringMap.find_opt set_name registry.set_ids_by_name with
  | Some existing_set_id ->
      Error (`Set_name_already_registered (set_name, existing_set_id))
  | None -> Ok ()

let validate_no_intra_set_duplicates (set_id : set_id)
    (cards : registered_card list) : (unit, error) result =
  let rec check seen_ids idx = function
    | [] -> Ok ()
    | card :: rest ->
        let id_str = Core.Card_id.to_string card.card_id in
        if StringSet.mem id_str seen_ids then
          Error (`Duplicate_card_id_within_set (set_id, card.card_id, idx))
        else check (StringSet.add id_str seen_ids) (idx + 1) rest
  in
  check StringSet.empty 0 cards

let validate_no_cross_set_conflicts (registry : t) (set_id : set_id)
    (cards : registered_card list) : (unit, error) result =
  let rec check = function
    | [] -> Ok ()
    | card :: rest -> (
        let id_str = Core.Card_id.to_string card.card_id in
        match StringMap.find_opt id_str registry.cards_by_id with
        | None -> check rest
        | Some existing ->
            Error
              (`Card_id_already_registered
                 (card.card_id, existing.set_id, set_id)))
  in
  check cards

let prerender_cards : type div fx trig.
    set_id:set_id ->
    set_name:set_name ->
    cards:(div, fx, trig) Cards.card list ->
    renderer:((div, fx, trig) Cards.card -> string) ->
    registered_card list =
 fun ~set_id ~set_name ~cards ~renderer ->
  let prerender_card card =
    {
      card_id = card_id card;
      name = card_name card;
      card_type = card_type_of card;
      set_id;
      set_name;
      rendered_text = renderer card;
      card_data = None;
    }
  in
  List.map prerender_card cards

let prerender_core_cards ~(set_id : set_id) ~(set_name : set_name)
    ~(cards : Cards.core_card list) : registered_card list =
  let prerender_card card =
    {
      card_id = card_id card;
      name = card_name card;
      card_type = card_type_of card;
      set_id;
      set_name;
      rendered_text = To_string.Core.card_to_string card;
      card_data = Some card;
    }
  in
  List.map prerender_card cards

let add_cards_to_registry cards_by_id (cards : registered_card list) =
  List.fold_left
    (fun map card ->
      let id_str = Core.Card_id.to_string card.card_id in
      StringMap.add id_str card map)
    cards_by_id cards

let register_division : type div fx trig.
    t ->
    id:set_id ->
    name:set_name ->
    cards:(div, fx, trig) Cards.card list ->
    renderer:((div, fx, trig) Cards.card -> string) ->
    (t, error) result =
 fun registry ~id ~name ~cards ~renderer ->
  let* () = validate_set_id_unique registry id in
  let* () = validate_set_name_unique registry name in
  let rendered_cards =
    prerender_cards ~set_id:id ~set_name:name ~cards ~renderer
  in
  let* () = validate_no_intra_set_duplicates id rendered_cards in
  let* () = validate_no_cross_set_conflicts registry id rendered_cards in
  let set = { metadata = { id; name }; cards = rendered_cards } in
  let sets_by_id = StringMap.add id set registry.sets_by_id in
  let set_ids_by_name = StringMap.add name id registry.set_ids_by_name in
  let cards_by_id = add_cards_to_registry registry.cards_by_id rendered_cards in
  Ok { sets_by_id; set_ids_by_name; cards_by_id }

let register_core_division (registry : t) ~(id : set_id) ~(name : set_name)
    ~(cards : Cards.core_card list) : (t, error) result =
  let* () = validate_set_id_unique registry id in
  let* () = validate_set_name_unique registry name in
  let rendered_cards = prerender_core_cards ~set_id:id ~set_name:name ~cards in
  let* () = validate_no_intra_set_duplicates id rendered_cards in
  let* () = validate_no_cross_set_conflicts registry id rendered_cards in
  let set = { metadata = { id; name }; cards = rendered_cards } in
  let sets_by_id = StringMap.add id set registry.sets_by_id in
  let set_ids_by_name = StringMap.add name id registry.set_ids_by_name in
  let cards_by_id = add_cards_to_registry registry.cards_by_id rendered_cards in
  Ok { sets_by_id; set_ids_by_name; cards_by_id }

module Global = struct
  let global : t ref = ref empty
  let current () = !global
  let set registry = global := registry
  let clear () = global := empty

  let register_division ~id ~name ~cards ~renderer =
    match register_division (current ()) ~id ~name ~cards ~renderer with
    | Ok registry ->
        set registry;
        Ok ()
    | Error e -> Error e

  let register_core_division ~id ~name ~cards =
    match register_core_division (current ()) ~id ~name ~cards with
    | Ok registry ->
        set registry;
        Ok ()
    | Error e -> Error e

  let list_sets () = list_sets (current ())
  let find_set_by_id set_id = find_set_by_id (current ()) set_id
  let find_set_by_name set_name = find_set_by_name (current ()) set_name
  let list_cards () = list_cards (current ())
  let find_card card_id = find_card (current ()) card_id
end
