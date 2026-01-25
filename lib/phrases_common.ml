type verb_forms = {
  infinitive : string;
  imperative : string;
  third_person : string;
}

let capitalize s =
  if String.length s = 0 then s
  else String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c else c) s
