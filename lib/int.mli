type non_negative
type positive
type 'a t

val zero : non_negative t
val one : non_negative t
val two : non_negative t
val three : non_negative t
val four : non_negative t
val five : non_negative t
val six : non_negative t
val seven : non_negative t
val eight : non_negative t
val nine : non_negative t
val ten : non_negative t
val eleven : non_negative t
val twelve : non_negative t
val thirteen : non_negative t
val fourteen : non_negative t
val fifteen : non_negative t
val sixteen : non_negative t
val seventeen : non_negative t
val eighteen : non_negative t
val nineteen : non_negative t
val twenty : non_negative t

module Positive : sig
  val one : positive t
  val two : positive t
  val three : positive t
  val four : positive t
  val five : positive t
  val six : positive t
  val seven : positive t
  val eight : positive t
  val nine : positive t
  val ten : positive t
  val eleven : positive t
  val twelve : positive t
  val thirteen : positive t
  val fourteen : positive t
  val fifteen : positive t
  val sixteen : positive t
  val seventeen : positive t
  val eighteen : positive t
  val nineteen : positive t
  val twenty : positive t
end

val non_negative_exn : int -> non_negative t
val positive_exn : int -> positive t
val to_non_negative : positive t -> non_negative t
val to_int : _ t -> int
val ( + ) : 'a t -> 'a t -> 'a t
val ( - ) : 'a t -> 'a t -> 'a t option
