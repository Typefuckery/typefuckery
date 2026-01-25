open Core
open Targets

type core_effect =
  | Add_CC of { target : target_personnel; amount : Int.positive Int.t }
  | Remove_CC of { target : target_personnel; amount : Int.positive Int.t }
  | Move_CC of {
      from : target_personnel;
      to_ : target_personnel;
      amount : Int.positive Int.t;
    }
  | Move_CC_between_pair of {
      pair : target_personnel;
      amount : Int.positive Int.t;
    }
  | Deploy_personnel of {
      card : Card_id.t;
      to_sector : sector;
      bonus_cc : Int.non_negative Int.t;
    }
  | Move_personnel of { target : target_personnel; to_sector : target_sector }
  | Send_to_abyss of { target : target_personnel }
  | Add_breach_marker of { target : target_entity; amount : Int.positive Int.t }
  | Remove_breach_marker of {
      target : target_entity;
      amount : Int.positive Int.t;
    }
  | Reset_breach_markers of { target : target_entity }
  | Flip_sector of { target : target_sector; to_state : sector_state }
  | Draw of { player : target_player; amount : Int.positive Int.t }
  | Discard of { player : target_player; amount : Int.positive Int.t }
  | Discard_hand of { player : target_player }
  | Prevent_CC_loss of {
      target : target_personnel;
      amount : Int.positive Int.t;
    }
  | Contain_entity of { target : target_entity }

type 'ext t =
  | Core_effect of core_effect
  | Ext of 'ext
  | If_possible of 'ext t
  | Let of { name : string; target : Targets.target_personnel; body : 'ext t }
  | Let_entity of {
      name : string;
      target : Targets.target_entity;
      body : 'ext t;
    }
  | Delayed of {
      window : Core.timing_window;
      scope : Core.timing_scope;
      then_do : 'ext t;
    }
  | Composite of 'ext t list
  | Player_choice of {
      player : Targets.target_player;
      option_a : 'ext t;
      option_b : 'ext t;
    }
  | Log of string
  | Noop

type core = No_ext.t t
