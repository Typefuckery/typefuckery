type t = Inline_md of string | Doc_uri of string

let inline_md md = Inline_md md
let doc_uri uri = Doc_uri uri

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let to_markdown = function
  | Inline_md md -> md
  | Doc_uri uri -> (
      let prefix = "lore/" in
      let normalized =
        if
          String.length uri >= String.length prefix
          && String.sub uri 0 (String.length prefix) = prefix
        then uri
        else prefix ^ uri
      in
      try read_file normalized
      with Sys_error msg ->
        invalid_arg (Printf.sprintf "Lore.to_markdown: %s" msg))
