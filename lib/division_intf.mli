module type EXTENSION = sig
  type fx
  type trig
  type div

  val fx_to_string : fx -> string
  val trig_to_string : trig -> string
  val div_to_string : div -> string
end

module No_extension :
  EXTENSION
    with type fx = No_ext.t
     and type trig = No_ext.t
     and type div = Core.division

module type DIVISION = sig
  val id : string
  val name : string
  val is_core : bool
  val lore : Lore.t option

  module Ext : EXTENSION

  type effect_t = Ext.fx Effects.t
  type trigger_t = Ext.trig Abilities.trigger
  type ability_t = (Ext.fx, Ext.trig) Abilities.ability
  type card_t = (Ext.div, Ext.fx, Ext.trig) Cards.card

  val cards : card_t list
end
