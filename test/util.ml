let assert_true cond label =
  if not cond then failwith ("Assertion failed: " ^ label)

module Json = struct
  type t =
    | Null
    | Bool of bool
    | Int of int
    | String of string
    | Array of t list
    | Object of (string * t) list

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

  let rec to_string = function
    | Null -> "null"
    | Bool b -> if b then "true" else "false"
    | Int n -> string_of_int n
    | String s -> Printf.sprintf "\"%s\"" (escape_string s)
    | Array items ->
        let items_str = items |> List.map to_string |> String.concat ", " in
        Printf.sprintf "[%s]" items_str
    | Object pairs ->
        let pairs_str =
          pairs
          |> List.map (fun (k, v) ->
              Printf.sprintf "\"%s\": %s" k (to_string v))
          |> String.concat ", "
        in
        Printf.sprintf "{%s}" pairs_str
end

let contains_substring ~haystack ~needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  if needle_len = 0 then true
  else
    let rec go i =
      if i + needle_len > haystack_len then false
      else if String.sub haystack i needle_len = needle then true
      else go (i + 1)
    in
    go 0

let assert_contains haystack needle label =
  if not (contains_substring ~haystack ~needle) then
    failwith (Printf.sprintf "%s: %S not found in %S" label needle haystack)

let load_file ~ext filename =
  let path = "test/golden/" ^ filename ^ ext in
  try
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    content
  with Sys_error msg ->
    failwith (Printf.sprintf "Failed to load golden file %s: %s" path msg)

let write_file ~ext filename content =
  let path = "test/golden/" ^ filename ^ ext in
  try
    let oc = open_out path in
    output_string oc content;
    close_out oc
  with Sys_error msg ->
    failwith (Printf.sprintf "Failed to write golden file %s: %s" path msg)

let load_golden_file filename = load_file ~ext:".txt" filename
let write_golden_file filename content = write_file ~ext:".txt" filename content
let load_json_golden_file filename = load_file ~ext:".json" filename

let write_json_golden_file filename content =
  write_file ~ext:".json" filename content

let update_goldens () =
  match Sys.getenv_opt "UPDATE_GOLDENS" with
  | Some "1" | Some "true" | Some "yes" -> true
  | _ -> false

let run_golden_tests ~load ~write ~render ~label items =
  if update_goldens () then
    List.iter
      (fun (filename, item) ->
        let actual = render item in
        write filename actual)
      items
  else
    List.iter
      (fun (filename, item) ->
        let expected = load filename in
        let actual = render item in
        assert_true (actual = expected)
          (Printf.sprintf
             "%s golden test match for %s\nExpected:\n%s\nActual:\n%s" label
             filename expected actual))
      items

let run_text_golden_tests ~render items =
  run_golden_tests ~load:load_golden_file ~write:write_golden_file ~render
    ~label:"Text" items

let run_json_golden_tests ~render items =
  run_golden_tests ~load:load_json_golden_file ~write:write_json_golden_file
    ~render ~label:"JSON" items
