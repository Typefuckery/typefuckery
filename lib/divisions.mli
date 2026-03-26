module Ada : sig
  val id : Registry.set_id
  val name : Registry.set_name
  val cards : Core.ada Cards.core_card list
  val register : unit -> (unit, Registry.error) result
end

module Haskell : sig
  val id : Registry.set_id
  val name : Registry.set_name
  val cards : Core.haskell Cards.core_card list
  val register : unit -> (unit, Registry.error) result
end

module Ocaml : sig
  val id : Registry.set_id
  val name : Registry.set_name
  val cards : Core.ocaml_div Cards.core_card list
  val register : unit -> (unit, Registry.error) result
end

module Rust : sig
  val id : Registry.set_id
  val name : Registry.set_name
  val cards : Core.rust Cards.core_card list
  val register : unit -> (unit, Registry.error) result
end

module Institute : sig
  val id : Registry.set_id
  val name : Registry.set_name
  val cards : Core.institute Cards.core_card list
  val register : unit -> (unit, Registry.error) result
end

val register : unit -> (unit, (Registry.set_id * Registry.error) list) result
