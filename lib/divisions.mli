module Ada : sig
  val id : Registry.set_id
  val name : Registry.set_name
  val cards : Cards.core_card list
  val register : unit -> (unit, Registry.error) result
end

module Haskell : sig
  val id : Registry.set_id
  val name : Registry.set_name
  val cards : Cards.core_card list
  val register : unit -> (unit, Registry.error) result
end

module Ocaml : sig
  val id : Registry.set_id
  val name : Registry.set_name
  val cards : Cards.core_card list
  val register : unit -> (unit, Registry.error) result
end

module Rust : sig
  val id : Registry.set_id
  val name : Registry.set_name
  val cards : Cards.core_card list
  val register : unit -> (unit, Registry.error) result
end

module Institute : sig
  val id : Registry.set_id
  val name : Registry.set_name
  val cards : Cards.core_card list
  val register : unit -> (unit, Registry.error) result
end

val register : unit -> (unit, (Registry.set_id * Registry.error) list) result
