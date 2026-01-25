module R = Typefuckery.Registry
module D = Typefuckery.Divisions
module L = Typefuckery.Language_registry
module Core = Typefuckery.Core
module To_json = Typefuckery.To_json
open Typefuckery.Core

type output_format = Text | Json

let serializer_of_lang lang =
  match L.Global.find_language lang with
  | Ok ser -> ser
  | Error (`Language_not_found _) ->
      Printf.printf "Unknown language: %s\n" lang;
      Printf.printf "Available languages: %s\n"
        (String.concat ", " (L.Global.list_languages ()));
      exit 1
  | Error (`Language_already_registered _) ->
      failwith "serializer_of_lang: unexpected error"

let print_help () =
  print_endline "typefuckery - Card game system CLI";
  print_endline "";
  print_endline "Usage:";
  print_endline "  typefuckery list-sets";
  print_endline "    List all registered card sets";
  print_endline "";
  print_endline "  typefuckery list-cards [--set SET_ID] [--type TYPE]";
  print_endline "    List cards with optional filters";
  print_endline "    --set: Filter by set ID (e.g., rust, ada, institute)";
  print_endline
    "    --type: Filter by card type (personnel, procedure, event, entity)";
  print_endline "";
  print_endline "  typefuckery show CARD_ID [--format FORMAT] [--lang LANG]";
  print_endline "    Display full card text for a specific card";
  print_endline "    --format: Output format (text, json). Default: text";
  print_endline "    --lang: Language for text output. Default: english";
  print_endline "";
  print_endline
    "  typefuckery export [--set SET_ID] [--type TYPE] [--format FORMAT] \
     [--lang LANG]";
  print_endline "    Export multiple cards with optional filters";
  print_endline "    --set: Filter by set ID";
  print_endline
    "    --type: Filter by card type (personnel, procedure, event, entity)";
  print_endline "    --format: Output format (text, json). Default: text";
  print_endline "    --lang: Language for text output. Default: english";
  print_endline "";
  print_endline "  typefuckery list-languages";
  print_endline "    List all available languages for text rendering";
  print_endline "";
  print_endline "  typefuckery help";
  print_endline "    Show this help message"

let list_sets () =
  let sets = R.Global.list_sets () in
  if List.length sets = 0 then print_endline "No sets registered."
  else (
    Printf.printf "Registered sets (%d):\n" (List.length sets);
    List.iter
      (fun metadata ->
        let set_opt = R.Global.find_set_by_id metadata.R.id in
        let card_count =
          match set_opt with Some s -> List.length s.R.cards | None -> 0
        in
        Printf.printf "  %s - %s (%d cards)\n" metadata.R.id metadata.R.name
          card_count)
      sets)

let list_languages () =
  let langs = L.Global.list_languages () in
  Printf.printf "Available languages (%d):\n" (List.length langs);
  List.iter (fun name -> Printf.printf "  %s\n" name) langs

let card_type_of_string s =
  match String.lowercase_ascii s with
  | "personnel" -> Some R.Personnel
  | "procedure" -> Some R.Procedure
  | "event" -> Some R.Event
  | "entity" -> Some R.Entity
  | _ -> None

let card_type_to_string = function
  | R.Personnel -> "Personnel"
  | R.Procedure -> "Procedure"
  | R.Event -> "Event"
  | R.Entity -> "Entity"

let list_cards ?set_id ?card_type () =
  let all_cards = R.Global.list_cards () in
  let filtered =
    all_cards
    |> List.filter (fun entry ->
        match set_id with Some id -> entry.R.set_id = id | None -> true)
    |> List.filter (fun entry ->
        match card_type with Some ct -> entry.R.card_type = ct | None -> true)
  in
  if List.length filtered = 0 then
    print_endline "No cards found matching the filters."
  else (
    Printf.printf "Cards (%d):\n" (List.length filtered);
    List.iter
      (fun (entry : R.registered_card) ->
        Printf.printf "  [%s] %s (%s) - %s\n" entry.set_id
          (Card_id.to_string entry.card_id)
          (card_type_to_string entry.card_type)
          entry.name)
      filtered)

let render_entry_text (entry : R.registered_card) serializer =
  match R.render_card entry serializer with
  | Some text -> text
  | None -> entry.R.rendered_text

let render_entry_json (entry : R.registered_card) =
  match entry.card_data with
  | Some card -> Some (To_json.json_to_string (To_json.card_to_json card))
  | None -> None

let show_card card_id_str ~format ~lang =
  let search_id = Card_id.of_string card_id_str in
  match R.Global.find_card search_id with
  | Some entry -> (
      match format with
      | Json -> (
          match render_entry_json entry with
          | Some json -> print_endline json
          | None ->
              Printf.printf
                "JSON export not available for non-core division card: %s\n"
                card_id_str;
              exit 1)
      | Text ->
          let serializer = serializer_of_lang lang in
          print_endline (render_entry_text entry serializer))
  | None ->
      Printf.printf "Card not found: %s\n" card_id_str;
      exit 1

let export_cards ?set_id ?card_type ~format ~lang () =
  let all_cards = R.Global.list_cards () in
  let filtered =
    all_cards
    |> List.filter (fun entry ->
        match set_id with Some id -> entry.R.set_id = id | None -> true)
    |> List.filter (fun entry ->
        match card_type with Some ct -> entry.R.card_type = ct | None -> true)
  in
  if List.length filtered = 0 then
    print_endline "No cards found matching the filters."
  else
    match format with
    | Text ->
        let serializer = serializer_of_lang lang in
        let texts =
          List.map (fun e -> render_entry_text e serializer) filtered
        in
        print_endline (String.concat "\n---\n" texts)
    | Json ->
        List.iter
          (fun entry ->
            match render_entry_json entry with
            | Some json -> print_endline json
            | None ->
                Printf.eprintf "Warning: skipping non-core card %s (no JSON)\n"
                  (Card_id.to_string entry.R.card_id))
          filtered

let parse_list_cards_args args =
  let rec go args set_id card_type =
    match args with
    | [] -> (set_id, card_type)
    | "--set" :: id :: rest -> go rest (Some id) card_type
    | "--type" :: t :: rest -> (
        match card_type_of_string t with
        | Some ct -> go rest set_id (Some ct)
        | None ->
            Printf.printf "Unknown card type: %s\n" t;
            exit 1)
    | _ ->
        Printf.printf "Unknown option or missing argument\n";
        exit 1
  in
  go args None None

let parse_show_args args =
  let rec go args format lang =
    match args with
    | [] -> (format, lang)
    | "--format" :: "text" :: rest -> go rest Text lang
    | "--format" :: "json" :: rest -> go rest Json lang
    | "--format" :: f :: _ ->
        Printf.printf "Unknown format: %s (expected text or json)\n" f;
        exit 1
    | "--lang" :: l :: rest -> go rest format l
    | _ ->
        Printf.printf "Unknown option or missing argument\n";
        exit 1
  in
  go args Text "english"

let parse_export_args args =
  let rec go args set_id card_type format lang =
    match args with
    | [] -> (set_id, card_type, format, lang)
    | "--set" :: id :: rest -> go rest (Some id) card_type format lang
    | "--type" :: t :: rest -> (
        match card_type_of_string t with
        | Some ct -> go rest set_id (Some ct) format lang
        | None ->
            Printf.printf "Unknown card type: %s\n" t;
            exit 1)
    | "--format" :: "text" :: rest -> go rest set_id card_type Text lang
    | "--format" :: "json" :: rest -> go rest set_id card_type Json lang
    | "--format" :: f :: _ ->
        Printf.printf "Unknown format: %s (expected text or json)\n" f;
        exit 1
    | "--lang" :: l :: rest -> go rest set_id card_type format l
    | _ ->
        Printf.printf "Unknown option or missing argument\n";
        exit 1
  in
  go args None None Text "english"

let () =
  (match D.register () with
  | Ok () -> ()
  | Error (errs : (string * R.error) list) ->
      List.iter
        (fun (division, err) ->
          let err_msg =
            match err with
            | `Set_id_already_registered id ->
                Printf.sprintf "Set_id_already_registered: %s" id
            | `Set_name_already_registered (name, existing) ->
                Printf.sprintf "Set_name_already_registered: %s (existing %s)"
                  name existing
            | `Duplicate_card_id_within_set (set_id, card_id, idx) ->
                Printf.sprintf "Duplicate_card_id_within_set: %s/%s at index %d"
                  set_id
                  (Core.Card_id.to_string card_id)
                  idx
            | `Card_id_already_registered (card_id, existing, new_set) ->
                Printf.sprintf
                  "Card_id_already_registered: %s already in %s (new %s)"
                  (Core.Card_id.to_string card_id)
                  existing new_set
          in
          Printf.printf "Division %s failed to register: %s\n" division err_msg)
        errs;
      exit 1);
  L.init ();
  let args = List.tl (Array.to_list Sys.argv) in
  match args with
  | [] | "help" :: _ -> print_help ()
  | "list-sets" :: _ -> list_sets ()
  | "list-languages" :: _ -> list_languages ()
  | "list-cards" :: rest ->
      let set_id, card_type = parse_list_cards_args rest in
      list_cards ?set_id ?card_type ()
  | "show" :: card_id :: rest ->
      let format, lang = parse_show_args rest in
      show_card card_id ~format ~lang
  | "export" :: rest ->
      let set_id, card_type, format, lang = parse_export_args rest in
      export_cards ?set_id ?card_type ~format ~lang ()
  | cmd :: _ ->
      Printf.printf "Unknown command: %s\n" cmd;
      print_help ();
      exit 1
