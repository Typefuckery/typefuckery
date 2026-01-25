type t = Inline_md of string | Doc_uri of string

val inline_md : string -> t
val doc_uri : string -> t
val to_markdown : t -> string
