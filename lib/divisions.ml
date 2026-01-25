module Ada = Ada
module Haskell = Haskell
module Ocaml = Ocaml
module Rust = Rust
module Institute = Institute

let register () : (unit, (Registry.set_id * Registry.error) list) result =
  let registrations =
    [
      (Ada.id, Ada.register);
      (Haskell.id, Haskell.register);
      (Ocaml.id, Ocaml.register);
      (Rust.id, Rust.register);
      (Institute.id, Institute.register);
    ]
  in
  let rec go acc = function
    | [] -> if acc = [] then Ok () else Error (List.rev acc)
    | (name, register) :: rest -> (
        match register () with
        | Ok () -> go acc rest
        | Error e -> go ((name, e) :: acc) rest)
  in
  go [] registrations
