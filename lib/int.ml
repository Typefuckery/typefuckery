type non_negative
type positive
type _ t = Non_neg : int -> non_negative t | Pos : int -> positive t

let non_negative_exn n =
  if n < 0 then
    invalid_arg
      (Printf.sprintf "Int.non_negative_exn: value must be >= 0, got %d" n)
  else Non_neg n

let positive_exn n =
  if n < 1 then
    invalid_arg
      (Printf.sprintf "Int.positive_exn: value must be >= 1, got %d" n)
  else Pos n

let mk_non_neg n = non_negative_exn n
let mk_pos n = positive_exn n
let zero : non_negative t = mk_non_neg 0
let one : non_negative t = mk_non_neg 1
let two : non_negative t = mk_non_neg 2
let three : non_negative t = mk_non_neg 3
let four : non_negative t = mk_non_neg 4
let five : non_negative t = mk_non_neg 5
let six : non_negative t = mk_non_neg 6
let seven : non_negative t = mk_non_neg 7
let eight : non_negative t = mk_non_neg 8
let nine : non_negative t = mk_non_neg 9
let ten : non_negative t = mk_non_neg 10
let eleven : non_negative t = mk_non_neg 11
let twelve : non_negative t = mk_non_neg 12
let thirteen : non_negative t = mk_non_neg 13
let fourteen : non_negative t = mk_non_neg 14
let fifteen : non_negative t = mk_non_neg 15
let sixteen : non_negative t = mk_non_neg 16
let seventeen : non_negative t = mk_non_neg 17
let eighteen : non_negative t = mk_non_neg 18
let nineteen : non_negative t = mk_non_neg 19
let twenty : non_negative t = mk_non_neg 20

module Positive = struct
  let one : positive t = mk_pos 1
  let two : positive t = mk_pos 2
  let three : positive t = mk_pos 3
  let four : positive t = mk_pos 4
  let five : positive t = mk_pos 5
  let six : positive t = mk_pos 6
  let seven : positive t = mk_pos 7
  let eight : positive t = mk_pos 8
  let nine : positive t = mk_pos 9
  let ten : positive t = mk_pos 10
  let eleven : positive t = mk_pos 11
  let twelve : positive t = mk_pos 12
  let thirteen : positive t = mk_pos 13
  let fourteen : positive t = mk_pos 14
  let fifteen : positive t = mk_pos 15
  let sixteen : positive t = mk_pos 16
  let seventeen : positive t = mk_pos 17
  let eighteen : positive t = mk_pos 18
  let nineteen : positive t = mk_pos 19
  let twenty : positive t = mk_pos 20
end

let to_non_negative (Pos n : positive t) : non_negative t = Non_neg n
let to_int (type a) (n : a t) : int = match n with Non_neg v -> v | Pos v -> v

let ( + ) (type a) (a : a t) (b : a t) : a t =
  match (a, b) with
  | Non_neg x, Non_neg y -> Non_neg (x + y)
  | Pos x, Pos y -> Pos (x + y)

let ( - ) (type a) (a : a t) (b : a t) : a t option =
  match (a, b) with
  | Non_neg x, Non_neg y -> if x >= y then Some (Non_neg (x - y)) else None
  | Pos x, Pos y ->
      let diff = x - y in
      if diff > 0 then Some (Pos diff) else None
