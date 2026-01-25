open Cards
open Card_dsl

let argus_daemon =
  personnel "institute:argus" "ARGUS, the Daemon" Institute ~cc:CC.seven
    [
      on_deploy (others_in_play +@ two);
      activated ~cost:CC.two (transfer one others_in_play me);
      activated ~cost:CC.three (anyone () --> here);
      burnout (secure here);
    ]

let pyroclasm =
  event "institute:pyroclasm" "Pyroclasm" Institute (everyone -@ two)

let game_reset =
  event "institute:game_reset" "Game Reset" Institute
    (seq
       [
         secure_all_sectors;
         reset_breach_markers (all_entities ());
         abyss each_personnel_in_play;
         draw ~player:each_player three;
       ])

let recursive_mirror =
  entity "institute:recursive_mirror" "Recursive Mirror" Institute
    ~flavor_text:
      "It reflects not your image, but your definition. And definitions that \
       reference themselves have no base case."
    ~threat:euclid ~timer:Timer.eight ~on_end_phase:(everyone_here -@ one)
    ~on_breach:(each_personnel_in_play -@ two)
    ~contained:Conditions.(total_cc_here ~total:8)
    ()

include Division_helper.Make_core_division (struct
  let id = "institute"
  let name = "Institute"
  let lore = None

  let cards =
    [
      Personnel argus_daemon;
      Event pyroclasm;
      Event game_reset;
      Entity recursive_mirror;
    ]
end)
