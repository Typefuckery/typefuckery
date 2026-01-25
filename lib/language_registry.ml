module StringMap = Map.Make (String)

type serializer = (module To_string.TEXT_SERIALIZER)

type error =
  [ `Language_already_registered of string | `Language_not_found of string ]

type t = { languages : serializer StringMap.t }

let empty : t = { languages = StringMap.empty }

let register_language (registry : t) ~(name : string) (ser : serializer) :
    (t, error) result =
  if StringMap.mem name registry.languages then
    Error (`Language_already_registered name)
  else Ok { languages = StringMap.add name ser registry.languages }

let find_language (registry : t) (name : string) : (serializer, error) result =
  match StringMap.find_opt name registry.languages with
  | Some ser -> Ok ser
  | None -> Error (`Language_not_found name)

let list_languages (registry : t) : string list =
  registry.languages |> StringMap.bindings |> List.map fst

module Global = struct
  let global : t ref = ref empty
  let current () = !global
  let set registry = global := registry
  let clear () = global := empty

  let register_language ~name ser =
    match register_language (current ()) ~name ser with
    | Ok registry ->
        set registry;
        Ok ()
    | Error e -> Error e

  let find_language name = find_language (current ()) name
  let list_languages () = list_languages (current ())
end

let init () =
  let builtins =
    [
      ( "english",
        (module To_string.Detailed_English : To_string.TEXT_SERIALIZER) );
      ("pidgin", (module To_string.Pidgin_English : To_string.TEXT_SERIALIZER));
      ("rust", (module To_string.Rust : To_string.TEXT_SERIALIZER));
      ("haskell", (module To_string.Haskell : To_string.TEXT_SERIALIZER));
      ("ada", (module To_string.Ada : To_string.TEXT_SERIALIZER));
      ("ocaml", (module To_string.OCaml : To_string.TEXT_SERIALIZER));
    ]
  in
  List.iter
    (fun (name, ser) ->
      match Global.register_language ~name ser with
      | Ok () -> ()
      | Error (`Language_already_registered _) ->
          failwith
            ("Language_registry.init: builtin language already registered: "
           ^ name)
      | Error (`Language_not_found _) ->
          failwith "Language_registry.init: unexpected error")
    builtins
