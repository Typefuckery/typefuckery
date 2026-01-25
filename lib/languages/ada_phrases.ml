open Phrases_common
include English_phrases

let this = "Self"
let the = "The_"
let a = "A_"
let all = "All_"
let each = "for Item of"
let another = "Other_"
let chosen = "Selected"
let personnel = "Personnel_Record"
let personnel_possessive = "Personnel_Record.all"
let procedure = "Procedure_Card"
let event = "Event_Card"
let entity = "Entity_Record"
let entities = "Entity_Array"
let sector = "Sector_Type"
let card = "Card_Access"
let cards = "Card_Array"
let player = "Player_Record"
let cc = "CC : Natural"
let breach_marker = "Breach_Marker_Type"
let breach_markers = "Breach_Marker_Array"
let abyss = "Abyss_Zone"
let hand = "Hand_Zone"
let battlefield = "Battlefield_Zone"
let stack = "Stack_Zone"
let deck = "Deck_Zone"
let from = "from"
let to_ = "to"
let in_ = "in"
let on = "onto"
let for_ = "for"
let up_to = "in range 1 .."
let then_ = ";"
let and_ = "and then"
let or_ = "or else"
let if_ = "if"
let when_ = "when"
let at = "at"
let that = "which"

let add =
  {
    infinitive = "Append";
    imperative = "Append (Container,";
    third_person = "Append (Container,";
  }

let remove =
  {
    infinitive = "Delete";
    imperative = "Delete (Container,";
    third_person = "Delete (Container,";
  }

let move =
  {
    infinitive = "Move";
    imperative = "Move (Source => From, Target =>";
    third_person = "Move (Source => From, Target =>";
  }

let draw =
  {
    infinitive = "Draw";
    imperative = "Draw (Player => Self, Count =>";
    third_person = "Draw (Player => Self, Count =>";
  }

let discard =
  {
    infinitive = "Discard";
    imperative = "Discard (Card =>";
    third_person = "Discard (Card =>";
  }

let deploy =
  {
    infinitive = "Deploy";
    imperative = "Deploy (Personnel =>";
    third_person = "Deploy (Personnel =>";
  }

let send =
  {
    infinitive = "Send";
    imperative = "Send (Destination => Abyss,";
    third_person = "Send (Destination => Abyss,";
  }

let flip =
  {
    infinitive = "Flip";
    imperative = "Flip (Card =>";
    third_person = "Flip (Card =>";
  }

let prevent =
  {
    infinitive = "Prevent";
    imperative = "Prevent (Effect =>";
    third_person = "Prevent (Effect =>";
  }

let choose =
  {
    infinitive = "Select";
    imperative = "Select (From => Valid_Targets,";
    third_person = "Select (From => Valid_Targets,";
  }

let spend =
  {
    infinitive = "Spend";
    imperative = "Spend (Amount =>";
    third_person = "Spend (Amount =>";
  }

let reset =
  { infinitive = "Reset"; imperative = "Reset"; third_person = "Resets" }

let breach_markers_to_start = "to_starting_breach_markers"
let you = "Active_Player"
let you_may = "if May_Activate then"
let another_target_player = "Target_Player : Player_Access"
let a_target_player = "Target : Player_Access"
let each_player = "for Player of Players loop"
let their_hand = "Player.Hand"
let no_effect = "null; -- No effect"
let if_you_cant_do_nothing = "exception when Constraint_Error => null;"
let loss = "raise Game_Loss"
let passive = "pragma Passive;"
let burnout = "pragma Finalize (Self);"
let cc_cost_label = "-- Cost : Natural :="
let at_end_of_round = "procedure End_Of_Round_Trigger is"
let at_end_of_phase = "procedure End_Of_Phase_Trigger is"
let when_deployed = "procedure On_Deploy_Trigger is"
let when_cc_reduced_to_zero = "procedure On_CC_Zero_Trigger is"
let once_per_round = "pragma Restriction (Max_Calls => 1, Per => Round);"
let before_end_phase = "procedure Before_End_Phase_Trigger is"
let before_end_phase_step_2 = "procedure Before_End_Phase_Step_2_Trigger is"
let at_start_of_end_phase = "procedure Start_Of_End_Phase_Trigger is"
let at_end_of_round_timing = "procedure End_Of_Round_Timing_Trigger is"
let this_round = "Current_Round"
let future_rounds = "Subsequent_Rounds"
let not_from_spending_cc = "-- Pre => Source /= Spend_CC"
let not_from_card_effect = "-- Pre => Source /= Card_Effect"
let not_from_entity_effect = "-- Pre => Source /= Entity_Effect"
let not_from_breach = "-- Pre => Source /= Breach"
let chosen_by_active_player = "with Chooser => Active_Player"
let chosen_by_starting_player = "with Chooser => Starting_Player"
let chosen_by_controller = "with Chooser => Controller"
let label_deck = "-- Division : Division_Type :="
let label_id = "-- Id : Card_Id :="
let label_starting_cc = "-- Starting_CC : Natural :="
let label_abilities = "-- Abilities : Ability_Array :="
let label_effect = "-- Effect : Effect_Type :="
let label_resolve_immediately = "-- pragma Immediate;"
let label_threat_level = "-- Threat_Level : Threat_Type :="
let label_breach_timer = "-- Breach_Timer : Natural :="

let label_end_phase_effect =
  "-- End_Phase_Effect : Effect_Type := -- while not Contained"

let label_breach_effect = "-- Breach_Effect : Effect_Type := -- on breach"
let label_containment = "-- Containment_Requirement : Requirement_Type :="
let none_placeholder = "null"
let bonus = "Bonus_Value"
let have = "Has"
let an_entity_possessive = "Entity_Ref.all"
let a_card_effect_would_reduce = "Effect in Reduce_Effect'Class"
let is_deployed = "'Deployed"
let is_breached = "'Breached"
let is_not_breached = "not 'Breached"
let always_contained = "pragma Always_Contained;"
let cannot_be_contained = "pragma Never_Contained;"
let not_ = "not"
let other_than = "/="
let with_ = "with"
let effect_resolves = "Resolve (Effect);"
let one_to_another noun = Printf.sprintf "Swap (%s_A, %s_B)" noun noun

let one_of_to_the_other noun =
  Printf.sprintf "Transfer (From => %s_Source, To => %s_Target)" noun noun

let or_more n = Printf.sprintf "in range %d .. Natural'Last" n

let has_min_cc ~min_cc =
  Printf.sprintf "(for all P of Container => P.CC >= %d)" min_cc

let filter_in_sector sec =
  Printf.sprintf "(for all P of Container => P.Sector = %s)" (sector_name sec)

let filter_and left right = Printf.sprintf "(%s) and then (%s)" left right

let in_same_sector_as_this_personnel =
  "(for all P of Container => P.Sector = Self.Sector)"

let other_personnel_description = "(for all P of Container => P /= Self)"

let other_personnel_in_sector =
  "(for all P of Self.Sector.Personnel => P /= Self)"

let filter_in_play = "(for all C of Container => C.Zone = Play)"
let filter_uncontained = "(for all E of Container => not E.Is_Contained)"

let controlled_by_active_player =
  "(for all P of Container => P.Controller = Active_Player)"

let controlled_by_chooser = "(for all P of Container => P.Controller = Chooser)"
let bound_that = "procedure Apply (Target : in out Personnel_Record)"

let bound_each_personnel_in_this_sector =
  "for P of Self.Sector.Personnel_List loop"

let that_entity = "Target_Entity"
let that_sector = "Target_Entity.Sector"
let chooses_one = "Select_One"
let either = "case Choice is"

let division_name = function
  | Core.Ada -> "Division_Ada"
  | Core.Haskell -> "Division_Haskell"
  | Core.OCaml -> "Division_OCaml"
  | Core.Rust -> "Division_Rust"
  | Core.Institute -> "Division_Institute"

let sector_name = function
  | Core.Alpha -> "Sector_Alpha"
  | Core.Beta -> "Sector_Beta"
  | Core.Lambda -> "Sector_Lambda"
  | Core.Gamma -> "Sector_Gamma"

let sector_state_name = function
  | Core.Secure -> "State_Secure"
  | Core.Breached -> "State_Breached"

let threat_level_name = function
  | Cards.Safe -> "Threat_Safe"
  | Cards.Euclid -> "Threat_Euclid"
  | Cards.Keter -> "Threat_Keter"
  | Cards.Titan -> "Threat_Titan"
