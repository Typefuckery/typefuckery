module D = Typefuckery.Divisions
module R = Typefuckery.Registry
module Core = Typefuckery.Core

let assert_true = Util.assert_true

let assert_some opt label =
  match opt with Some v -> v | None -> failwith ("Expected Some for " ^ label)

let test_all_divisions_register () =
  R.Global.clear ();

  match D.register () with
  | Ok () -> ()
  | Error errs ->
      let msg =
        errs
        |> List.map (fun (division, e) ->
            match e with
            | `Set_id_already_registered id ->
                Printf.sprintf "%s: Set_id_already_registered %s" division id
            | `Set_name_already_registered (name, existing) ->
                Printf.sprintf
                  "%s: Set_name_already_registered %s (existing %s)" division
                  name existing
            | `Duplicate_card_id_within_set (set_id, _, idx) ->
                Printf.sprintf "%s: Duplicate_card_id_within_set %s at %d"
                  division set_id idx
            | `Card_id_already_registered (cid, set1, set2) ->
                Printf.sprintf "%s: Card_id_already_registered %s (%s vs %s)"
                  division
                  (Core.Card_id.to_string cid)
                  set1 set2)
        |> String.concat "; "
      in
      failwith ("Divisions registration failed: " ^ msg)

let test_rust_division_exists () =
  let set = assert_some (R.Global.find_set_by_id "rust") "rust set" in
  assert_true (set.metadata.name = "Rust Division") "Rust set has correct name"

let test_ada_division_exists () =
  let set = assert_some (R.Global.find_set_by_id "ada") "ada set" in
  assert_true (set.metadata.name = "Ada Division") "Ada set has correct name"

let test_haskell_division_exists () =
  let set = assert_some (R.Global.find_set_by_id "haskell") "haskell set" in
  assert_true
    (set.metadata.name = "Haskell Division")
    "Haskell set has correct name"

let test_ocaml_division_exists () =
  let set = assert_some (R.Global.find_set_by_id "ocaml") "ocaml set" in
  assert_true
    (set.metadata.name = "OCaml Division")
    "OCaml set has correct name"

let test_institute_division_exists () =
  let set = assert_some (R.Global.find_set_by_id "institute") "institute set" in
  assert_true (set.metadata.name = "Institute") "Institute set has correct name"

let test_five_divisions_total () =
  let sets = R.Global.list_sets () in
  assert_true (List.length sets = 5) "Exactly 5 divisions registered"

let () =
  test_all_divisions_register ();

  test_rust_division_exists ();

  test_ada_division_exists ();

  test_haskell_division_exists ();

  test_ocaml_division_exists ();

  test_institute_division_exists ();

  test_five_divisions_total ()
