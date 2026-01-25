open Core
open Abilities
open Cards

type json =
  | Null
  | Bool of bool
  | Int of int
  | String of string
  | Array of json list
  | Object of (string * json) list

module type S = sig
  val json_to_string : json -> string
  val card_to_json : Cards.core_card -> json
  val personnel_to_json : Cards.core_personnel -> json
  val procedure_to_json : Cards.core_procedure -> json
  val event_to_json : Cards.core_event -> json
  val entity_to_json : Cards.core_entity -> json
end

module Make (TS : To_string.TEXT_SERIALIZER) : S = struct
  let escape_string s =
    let buf = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        match c with
        | '"' -> Buffer.add_string buf "\\\""
        | '\\' -> Buffer.add_string buf "\\\\"
        | '\n' -> Buffer.add_string buf "\\n"
        | '\r' -> Buffer.add_string buf "\\r"
        | '\t' -> Buffer.add_string buf "\\t"
        | c -> Buffer.add_char buf c)
      s;
    Buffer.contents buf

  let rec json_to_string = function
    | Null -> "null"
    | Bool b -> if b then "true" else "false"
    | Int n -> string_of_int n
    | String s -> Printf.sprintf "\"%s\"" (escape_string s)
    | Array items ->
        let items_str =
          items |> List.map json_to_string |> String.concat ", "
        in
        Printf.sprintf "[%s]" items_str
    | Object pairs ->
        let pairs_str =
          pairs
          |> List.map (fun (k, v) ->
              Printf.sprintf "\"%s\": %s" k (json_to_string v))
          |> String.concat ", "
        in
        Printf.sprintf "{%s}" pairs_str

  let division_std (d : division) : string =
    match d with
    | Ada -> "ada"
    | Haskell -> "haskell"
    | OCaml -> "ocaml"
    | Rust -> "rust"
    | Institute -> "institute"

  let threat_level_std (t : threat_level) : string =
    match t with
    | Safe -> "safe"
    | Euclid -> "euclid"
    | Keter -> "keter"
    | Titan -> "titan"

  let option_to_json f = function None -> Null | Some v -> f v

  let lore_to_json lore =
    option_to_json (fun l -> String (Lore.to_markdown l)) lore

  let flavor_to_json flavor = option_to_json (fun s -> String s) flavor

  let ability_to_json (a : core_ability) : json =
    Object [ ("text", String (TS.ability_to_string a)) ]

  let personnel_to_json (p : core_personnel) : json =
    Object
      [
        ("card_type_std", String "personnel");
        ("card_type", String TS.personnel);
        ("id", String (Card_id.to_string p.id));
        ("name", String p.name);
        ("division_std", String (division_std p.division));
        ("division", String (TS.division_to_string p.division));
        ("lore_md", lore_to_json p.lore);
        ("flavor_text", flavor_to_json p.flavor_text);
        ("starting_cc", Int (Int.to_int p.starting_cc));
        ("abilities", Array (List.map ability_to_json p.abilities));
      ]

  let procedure_to_json (p : core_procedure) : json =
    Object
      [
        ("card_type_std", String "procedure");
        ("card_type", String TS.procedure);
        ("id", String (Card_id.to_string p.id));
        ("name", String p.name);
        ("division_std", String (division_std p.division));
        ("division", String (TS.division_to_string p.division));
        ("lore_md", lore_to_json p.lore);
        ("flavor_text", flavor_to_json p.flavor_text);
        ("text", String (TS.card_effect_to_string p.card_effect));
      ]

  let event_to_json (e : core_event) : json =
    Object
      [
        ("card_type_std", String "event");
        ("card_type", String TS.event);
        ("id", String (Card_id.to_string e.id));
        ("name", String e.name);
        ("division_std", String (division_std e.division));
        ("division", String (TS.division_to_string e.division));
        ("lore_md", lore_to_json e.lore);
        ("flavor_text", flavor_to_json e.flavor_text);
        ("text", String (TS.card_effect_to_string e.card_effect));
      ]

  let entity_to_json (e : core_entity) : json =
    Object
      [
        ("card_type_std", String "entity");
        ("card_type", String TS.entity);
        ("id", String (Card_id.to_string e.id));
        ("name", String e.name);
        ("division_std", String (division_std e.division));
        ("division", String (TS.division_to_string e.division));
        ("lore_md", lore_to_json e.lore);
        ("flavor_text", flavor_to_json e.flavor_text);
        ("threat_level_std", String (threat_level_std e.threat_level));
        ("threat_level", String (TS.threat_level_to_string e.threat_level));
        ("breach_timer", Int (Int.to_int e.breach_timer));
        ( "end_phase_effect_text",
          String (TS.card_effect_to_string e.end_phase_effect) );
        ("breach_effect_text", String (TS.card_effect_to_string e.breach_effect));
        ( "containment_text",
          String (TS.containment_requirement_to_string e.containment) );
      ]

  let card_to_json : core_card -> json = function
    | Personnel p -> personnel_to_json p
    | Procedure p -> procedure_to_json p
    | Event e -> event_to_json e
    | Entity e -> entity_to_json e
end

module Core = Make (To_string.Detailed_English)
include Core
