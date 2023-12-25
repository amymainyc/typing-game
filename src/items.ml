(*Because of a decision to structure our game state module more like a java
  class, we do not have a type t and our variables are all bound to the original
  instance of the class that was created. We did not realize that this would be
  incompatible with functors (which return a new, separate instance of the game
  state and thus do not have the variables at the same value). However, at the
  point where we realized this issue, it would not have been worth the time and
  effort to do a complete overhaul of the system*)
open State

type name =
  | Apple
  | Banana
  | BrokenClock
  | EdibleClock
  | Chaos
  | ForgottenAltar
  | BloodyAltar
  | Obfuscinator
  | Jetpack
  | ReverseJetpack
  | BigFryingPan
  | WordEviscerInator
  | BlackCatTrinket
  | RegressionStone
  | WizardsWand
  | CrystalOfClarity

type rarity =
  | Common
  | Rare
  | Epic
  | Undiscovered

type item =
  name * rarity * string * string * string list * (unit -> string -> unit)

let apple_effect () (mode : string) =
  match mode with
  | "Easy" ->
      EasyGameMutable._health :=
        min (EasyGameMutable.health () + 5) (EasyGameMutable.max_health ())
  | "Normal" ->
      NormalGameMutable._health :=
        min (NormalGameMutable.health () + 5) (NormalGameMutable.max_health ())
  | "Hard" ->
      HardGameMutable._health :=
        min (HardGameMutable.health () + 5) (HardGameMutable.max_health ())
  | "Extreme" ->
      ExtremeGameMutable._health :=
        min
          (ExtremeGameMutable.health () + 5)
          (ExtremeGameMutable.max_health ())
  | "Sudden Death" ->
      SuddenDeathMutable._health :=
        min
          (SuddenDeathMutable.health () + 5)
          (SuddenDeathMutable.max_health ())
  | "Chaos" ->
      ChaosGameMutable._health :=
        min (ChaosGameMutable.health () + 5) (ChaosGameMutable.max_health ())
  | _ -> failwith "Invalid game mode"

let apple : item =
  (Apple, Common, "Banapple", "Tastes like a banana.", [ "+5 HP" ], apple_effect)

let banana_effect () (mode : string) = apple_effect () mode

let banana : item =
  ( Banana,
    Common,
    "Appanana",
    "Tastes like an apple.",
    [ "+5 HP" ],
    banana_effect )

let broken_clock_effect () (mode : string) =
  match mode with
  | "Easy" -> EasyGameMutable._time := EasyGameMutable.time () + 10
  | "Normal" -> NormalGameMutable._time := NormalGameMutable.time () + 10
  | "Hard" -> HardGameMutable._time := HardGameMutable.time () + 10
  | "Extreme" -> ExtremeGameMutable._time := ExtremeGameMutable.time () + 10
  | "Sudden Death" ->
      SuddenDeathMutable._time := SuddenDeathMutable.time () + 10
  | "Chaos" -> ChaosGameMutable._time := ChaosGameMutable.time () + 10
  | _ -> failwith "Invalid game mode"

let broken_clock : item =
  ( BrokenClock,
    Rare,
    "A Broken Clock",
    "It's right twice a day, even though it's ten seconds off.",
    [ "+10 Seconds" ],
    broken_clock_effect )

let edible_clock_effect () (mode : string) =
  match mode with
  | "Easy" ->
      EasyGameMutable._time := EasyGameMutable.time () + 15;
      EasyGameMutable._health :=
        min (EasyGameMutable.health () + 10) (EasyGameMutable.max_health ())
  | "Normal" ->
      NormalGameMutable._time := NormalGameMutable.time () + 15;
      NormalGameMutable._health :=
        min (NormalGameMutable.health () + 10) (NormalGameMutable.max_health ())
  | "Hard" ->
      HardGameMutable._time := HardGameMutable.time () + 15;
      HardGameMutable._health :=
        min (HardGameMutable.health () + 10) (HardGameMutable.max_health ())
  | "Extreme" ->
      ExtremeGameMutable._time := ExtremeGameMutable.time () + 15;
      ExtremeGameMutable._health :=
        min
          (ExtremeGameMutable.health () + 10)
          (ExtremeGameMutable.max_health ())
  | "Sudden Death" ->
      SuddenDeathMutable._time := SuddenDeathMutable.time () + 15;
      SuddenDeathMutable._health :=
        min
          (SuddenDeathMutable.health () + 10)
          (SuddenDeathMutable.max_health ())
  | "Chaos" ->
      ChaosGameMutable._time := ChaosGameMutable.time () + 15;
      ChaosGameMutable._health :=
        min (ChaosGameMutable.health () + 10) (ChaosGameMutable.max_health ())
  | _ -> failwith "Invalid game mode"

let edible_clock : item =
  ( EdibleClock,
    Epic,
    "Let Them Eat Clock",
    "It ... kinda looks like cake??",
    [ "+10 HP"; "+15 Seconds" ],
    edible_clock_effect )

let chaos_effect () (mode : string) =
  match mode with
  | "Easy" ->
      if Random.int 2 = 0 then (
        EasyGameMutable._health := 1;
        EasyGameMutable._time := 60)
      else (
        EasyGameMutable._health := EasyGameMutable.max_health ();
        EasyGameMutable._time := 90)
  | "Normal" ->
      if Random.int 2 = 0 then (
        NormalGameMutable._health := 1;
        NormalGameMutable._time := 60)
      else (
        NormalGameMutable._health := NormalGameMutable.max_health ();
        NormalGameMutable._time := 90)
  | "Hard" ->
      if Random.int 2 = 0 then (
        HardGameMutable._health := 1;
        HardGameMutable._time := 60)
      else (
        HardGameMutable._health := HardGameMutable.max_health ();
        HardGameMutable._time := 90)
  | "Extreme" ->
      if Random.int 2 = 0 then (
        ExtremeGameMutable._health := 1;
        ExtremeGameMutable._time := 60)
      else (
        ExtremeGameMutable._health := ExtremeGameMutable.max_health ();
        ExtremeGameMutable._time := 90)
  | "Sudden Death" ->
      if Random.int 2 = 0 then (
        SuddenDeathMutable._health := 1;
        SuddenDeathMutable._time := 60)
      else (
        SuddenDeathMutable._health := SuddenDeathMutable.max_health ();
        SuddenDeathMutable._time := 90)
  | "Chaos" ->
      if Random.int 2 = 0 then (
        ChaosGameMutable._health := 1;
        ChaosGameMutable._time := 60)
      else (
        ChaosGameMutable._health := ChaosGameMutable.max_health ();
        ChaosGameMutable._time := 90)
  | _ -> failwith "Invalid game mode"

let chaos : item =
  (Chaos, Undiscovered, "???", "Don't.", [ "???" ], chaos_effect)

let forgotton_altar_effect () mode =
  match mode with
  | "Easy" ->
      EasyGameMutable._time := EasyGameMutable.time () / 2;
      EasyGameMutable._health :=
        min (EasyGameMutable.health () * 2) (EasyGameMutable.max_health ())
  | "Normal" ->
      NormalGameMutable._time := NormalGameMutable.time () / 2;
      NormalGameMutable._health :=
        min (NormalGameMutable.health () * 2) (NormalGameMutable.max_health ())
  | "Hard" ->
      HardGameMutable._time := HardGameMutable.time () / 2;
      HardGameMutable._health :=
        min (HardGameMutable.health () * 2) (HardGameMutable.max_health ())
  | "Extreme" ->
      ExtremeGameMutable._time := ExtremeGameMutable.time () / 2;
      ExtremeGameMutable._health :=
        min
          (ExtremeGameMutable.health () * 2)
          (ExtremeGameMutable.max_health ())
  | "Sudden Death" ->
      SuddenDeathMutable._time := SuddenDeathMutable.time () / 2;
      SuddenDeathMutable._health :=
        min
          (SuddenDeathMutable.health () * 2)
          (SuddenDeathMutable.max_health ())
  | "Chaos" ->
      ChaosGameMutable._time := ChaosGameMutable.time () / 2;
      ChaosGameMutable._health :=
        min (ChaosGameMutable.health () * 2) (ChaosGameMutable.max_health ())
  | _ -> failwith "Invalid game mode"

let forgotton_altar : item =
  ( ForgottenAltar,
    Rare,
    "The TIME Altar",
    "This altar is showing clear wear and tear due to the passage of time. It \
     seems to want something from you...",
    [ "+? HP"; "-? Seconds" ],
    forgotton_altar_effect )

let bloody_altar_effect () mode =
  match mode with
  | "Easy" ->
      EasyGameMutable._time := EasyGameMutable.time () * 2;
      EasyGameMutable._health :=
        max
          (min (EasyGameMutable.health () / 2) (EasyGameMutable.max_health ()))
          1
  | "Normal" ->
      NormalGameMutable._time := NormalGameMutable.time () * 2;
      NormalGameMutable._health :=
        max
          (min
             (NormalGameMutable.health () / 2)
             (NormalGameMutable.max_health ()))
          1
  | "Hard" ->
      HardGameMutable._time := HardGameMutable.time () * 2;
      HardGameMutable._health :=
        max
          (min (HardGameMutable.health () / 2) (HardGameMutable.max_health ()))
          1
  | "Extreme" ->
      ExtremeGameMutable._time := ExtremeGameMutable.time () * 2;
      ExtremeGameMutable._health :=
        max
          (min
             (ExtremeGameMutable.health () / 2)
             (ExtremeGameMutable.max_health ()))
          1
  | "Sudden Death" ->
      SuddenDeathMutable._time := SuddenDeathMutable.time () * 2;
      SuddenDeathMutable._health :=
        max
          (min
             (SuddenDeathMutable.health () / 2)
             (SuddenDeathMutable.max_health ()))
          1
  | "Chaos" ->
      ChaosGameMutable._time := ChaosGameMutable.time () * 2;
      ChaosGameMutable._health :=
        max
          (min
             (ChaosGameMutable.health () / 2)
             (ChaosGameMutable.max_health ()))
          1
  | _ -> failwith "Invalid game mode"

let bloody_altar : item =
  ( BloodyAltar,
    Rare,
    "The BLOOD Altar",
    "This altar is overflowing with blood, but its pulsing for more. It seems \
     to want something from you...",
    [ "-? HP"; "+? Seconds" ],
    bloody_altar_effect )

let obfuscinator_effect () mode =
  match mode with
  | "Easy" ->
      let tmp = EasyGameMutable.time () in
      EasyGameMutable._time := EasyGameMutable.health ();
      EasyGameMutable._health := min tmp 100
  | "Normal" ->
      let tmp = NormalGameMutable.time () in
      NormalGameMutable._time := NormalGameMutable.health ();
      NormalGameMutable._health := min tmp 100
  | "Hard" ->
      let tmp = HardGameMutable.time () in
      HardGameMutable._time := HardGameMutable.health ();
      HardGameMutable._health := min tmp 100
  | "Extreme" ->
      let tmp = ExtremeGameMutable.time () in
      ExtremeGameMutable._time := ExtremeGameMutable.health ();
      ExtremeGameMutable._health := min tmp 50
  | "Sudden Death" ->
      let tmp = SuddenDeathMutable.time () in
      SuddenDeathMutable._time := SuddenDeathMutable.health ();
      SuddenDeathMutable._health := min tmp 100
  | "Chaos" ->
      let tmp = ChaosGameMutable.time () in
      ChaosGameMutable._time := ChaosGameMutable.health ();
      ChaosGameMutable._health := min tmp 150
  | _ -> failwith "Invalid game mode"

let obfuscinator : item =
  ( Obfuscinator,
    Epic,
    "The Obfuscinator!!!",
    "Doofenshmirtz's latest invention! Allows you to make a deal with Time in \
     exchange for your soul.",
    [ "HP <==> Seconds" ],
    obfuscinator_effect )

let jetpack_effect () mode =
  match mode with
  | "Easy" ->
      EasyGameMutable._health := max (EasyGameMutable.health () - 10) 0;
      EasyGameMutable.adjust_level ()
  | "Normal" ->
      NormalGameMutable._health := max (NormalGameMutable.health () - 10) 0;
      NormalGameMutable.adjust_level ()
  | "Hard" ->
      HardGameMutable._health := max (HardGameMutable.health () - 10) 0;
      HardGameMutable.adjust_level ()
  | "Extreme" ->
      ExtremeGameMutable._health := max (ExtremeGameMutable.health () - 10) 0;
      ExtremeGameMutable.adjust_level ()
  | "Sudden Death" ->
      SuddenDeathMutable._health := max (SuddenDeathMutable.health () - 10) 0;
      SuddenDeathMutable.adjust_level ()
  | "Chaos" ->
      ChaosGameMutable._health := max (ChaosGameMutable.health () - 10) 0;
      ChaosGameMutable.adjust_level ()
  | _ -> failwith "Invalid game mode"

let jetpack : item =
  ( Jetpack,
    Rare,
    "Joyride",
    "You don't have your joyriding license with you, you can't use this!",
    [ "-10 HP"; "+1 Level" ],
    jetpack_effect )

let reverse_jetpack_effect () mode =
  match mode with
  | "Easy" ->
      EasyGameMutable._health := max (EasyGameMutable.health () - 10) 0;
      EasyGameMutable.decrement_level ()
  | "Normal" ->
      NormalGameMutable._health := max (NormalGameMutable.health () - 10) 0;
      NormalGameMutable.decrement_level ()
  | "Hard" ->
      HardGameMutable._health := max (HardGameMutable.health () - 10) 0;
      HardGameMutable.decrement_level ()
  | "Extreme" ->
      ExtremeGameMutable._health := max (ExtremeGameMutable.health () - 10) 0;
      ExtremeGameMutable.decrement_level ()
  | "Sudden Death" ->
      SuddenDeathMutable._health := max (SuddenDeathMutable.health () - 10) 0;
      SuddenDeathMutable.decrement_level ()
  | "Chaos" ->
      ChaosGameMutable._health := max (ChaosGameMutable.health () - 10) 0;
      ChaosGameMutable.decrement_level ()
  | _ -> failwith "Invalid game mode"

let reverse_jetpack : item =
  ( ReverseJetpack,
    Epic,
    "Suspicious Joyride",
    "You don't have your joyriding license with you, but you can still use \
     this!",
    [ "-10 HP"; "+1 Level..." ],
    jetpack_effect )

let big_frying_pan_effect () mode =
  match mode with
  | "Easy" ->
      EasyGameMutable._max_health := max (EasyGameMutable.max_health () - 5) 1;
      EasyGameMutable._health :=
        min (EasyGameMutable.max_health ()) (EasyGameMutable.health ())
  | "Normal" ->
      NormalGameMutable._max_health :=
        max (NormalGameMutable.max_health () - 5) 1;
      NormalGameMutable._health :=
        min (NormalGameMutable.max_health ()) (NormalGameMutable.health ())
  | "Hard" ->
      HardGameMutable._max_health := max (HardGameMutable.max_health () - 5) 1;
      HardGameMutable._health :=
        min (HardGameMutable.max_health ()) (HardGameMutable.health ())
  | "Extreme" ->
      ExtremeGameMutable._max_health :=
        max (ExtremeGameMutable.max_health () - 5) 1;
      ExtremeGameMutable._health :=
        min (ExtremeGameMutable.max_health ()) (ExtremeGameMutable.health ())
  | "Sudden Death" ->
      SuddenDeathMutable._max_health :=
        max (SuddenDeathMutable.max_health () - 5) 1;
      SuddenDeathMutable._health :=
        min (SuddenDeathMutable.max_health ()) (SuddenDeathMutable.health ())
  | "Chaos" ->
      ChaosGameMutable._max_health := max (ChaosGameMutable.max_health () - 5) 1;
      ChaosGameMutable._health :=
        min (ChaosGameMutable.max_health ()) (ChaosGameMutable.health ())
  | _ -> failwith "Invalid game mode"

let big_frying_pan : item =
  ( BigFryingPan,
    Rare,
    "Beeg Frying Pan",
    "It's ... it's big! And it looks angry!",
    [ "+5 Max HP (?)" ],
    big_frying_pan_effect )

let word_eviscer_inator_effect () mode =
  match mode with
  | "Easy" ->
      EasyGameMutable._num_words := max (EasyGameMutable.num_words () - 4) 10
  | "Normal" ->
      NormalGameMutable._num_words :=
        max (NormalGameMutable.num_words () - 4) 10
  | "Hard" ->
      HardGameMutable._num_words := max (HardGameMutable.num_words () - 4) 10
  | "Extreme" ->
      ExtremeGameMutable._num_words :=
        max (ExtremeGameMutable.num_words () - 4) 10
  | "Sudden Death" ->
      SuddenDeathMutable._num_words :=
        max (SuddenDeathMutable.num_words () - 4) 10
  | "Chaos" ->
      ChaosGameMutable._num_words := max (ChaosGameMutable.num_words () - 4) 10
  | _ -> failwith "Invalid game mode"

let word_eviscer_inator : item =
  ( WordEviscerInator,
    Epic,
    "Hungry Hungry Word Hippo",
    "It's hungry for knowledge, and words are the easiest way to that!",
    [ "-4 Number of Words" ],
    word_eviscer_inator_effect )

let black_cat_trinket_effect () mode =
  match mode with
  | "Easy" ->
      let new_health =
        max 1 (EasyGameMutable.max_health () - EasyGameMutable.health ())
      in
      EasyGameMutable._max_health := new_health;
      EasyGameMutable._health := new_health
  | "Normal" ->
      let new_health =
        max 1 (NormalGameMutable.max_health () - NormalGameMutable.health ())
      in
      NormalGameMutable._max_health := new_health;
      NormalGameMutable._health := new_health
  | "Hard" ->
      let new_health =
        max 1 (HardGameMutable.max_health () - HardGameMutable.health ())
      in
      HardGameMutable._max_health := new_health;
      HardGameMutable._health := new_health
  | "Extreme" ->
      let new_health =
        max 1 (ExtremeGameMutable.max_health () - ExtremeGameMutable.health ())
      in
      ExtremeGameMutable._max_health := new_health;
      ExtremeGameMutable._health := new_health
  | "Sudden Death" ->
      let new_health =
        max 1 (SuddenDeathMutable.max_health () - SuddenDeathMutable.health ())
      in
      SuddenDeathMutable._max_health := new_health;
      SuddenDeathMutable._health := new_health
  | "Chaos" ->
      let new_health =
        max 1 (ChaosGameMutable.max_health () - ChaosGameMutable.health ())
      in
      ChaosGameMutable._max_health := new_health;
      ChaosGameMutable._health := new_health
  | _ -> failwith "Invalid game mode"

let black_cat_trinket : item =
  ( BlackCatTrinket,
    Undiscovered,
    "A Cat Trinket",
    "An unfortunate encounter.",
    [ "??? h*a*th" ],
    black_cat_trinket_effect )

let regression_stone_effect () (mode : string) =
  match mode with
  | "Easy" ->
      let tmp = EasyGameMutable.cur_level () in
      EasyGameMutable._cur_level := EasyGameMutable.cur_level () / 2;
      let i = ref 1 in
      while !i < tmp - EasyGameMutable.cur_level () do
        EasyGameMutable.decrement_level ();
        i := !i + 1
      done
  | "Normal" ->
      let tmp = NormalGameMutable.cur_level () in
      NormalGameMutable._cur_level := NormalGameMutable.cur_level () / 2;
      let i = ref 1 in
      while !i < tmp - NormalGameMutable.cur_level () do
        NormalGameMutable.decrement_level ();
        i := !i + 1
      done
  | "Hard" ->
      let tmp = HardGameMutable.cur_level () in
      HardGameMutable._cur_level := HardGameMutable.cur_level () / 2;
      let i = ref 1 in
      while !i < tmp - HardGameMutable.cur_level () do
        HardGameMutable.decrement_level ();
        i := !i + 1
      done
  | "Extreme" ->
      let tmp = ExtremeGameMutable.cur_level () in
      ExtremeGameMutable._cur_level := ExtremeGameMutable.cur_level () / 2;
      let i = ref 1 in
      while !i < tmp - ExtremeGameMutable.cur_level () do
        ExtremeGameMutable.decrement_level ();
        i := !i + 1
      done
  | "Sudden Death" ->
      let tmp = SuddenDeathMutable.cur_level () in
      SuddenDeathMutable._cur_level := SuddenDeathMutable.cur_level () / 2;
      let i = ref 1 in
      while !i < tmp - SuddenDeathMutable.cur_level () do
        SuddenDeathMutable.decrement_level ();
        i := !i + 1
      done
  | "Chaos" ->
      let tmp = ChaosGameMutable.cur_level () in
      ChaosGameMutable._cur_level := ChaosGameMutable.cur_level () / 2;
      let i = ref 1 in
      while !i < tmp - ChaosGameMutable.cur_level () do
        ChaosGameMutable.decrement_level ();
        i := !i + 1
      done
  | _ -> failwith "Invalid game mode"

let regression_stone : item =
  ( RegressionStone,
    Epic,
    "Glowing Blue Rock",
    "As you peer into it, you start seeing glimpses of the past...",
    [ "-??? Levels" ],
    regression_stone_effect )

let wizards_wand_effect () mode =
  Random.self_init ();
  let rand_int = (Random.int 20) + 1 in
  match mode with
  | "Easy" ->
      EasyGameMutable._health :=
        min
          (EasyGameMutable.health () + rand_int)
          (EasyGameMutable.max_health ())
  | "Normal" ->
      NormalGameMutable._health :=
        min
          (NormalGameMutable.health () + rand_int)
          (NormalGameMutable.max_health ())
  | "Hard" ->
      HardGameMutable._health :=
        min
          (HardGameMutable.health () + rand_int)
          (HardGameMutable.max_health ())
  | "Extreme" ->
      ExtremeGameMutable._health :=
        min
          (ExtremeGameMutable.health () + rand_int)
          (ExtremeGameMutable.max_health ())
  | "Sudden Death" ->
      SuddenDeathMutable._health :=
        min
          (SuddenDeathMutable.health () + rand_int)
          (SuddenDeathMutable.max_health ())
  | "Chaos" ->
      ChaosGameMutable._health :=
        min
          (ChaosGameMutable.health () + rand_int)
          (ChaosGameMutable.max_health ())
  | _ -> failwith "Invalid game mode"

let wizards_wand : item =
  ( WizardsWand,
    Epic,
    "Wizard's Wand",
    "Shall I cast a spell?...",
    [ "+??? HP" ],
    wizards_wand_effect )

let crystal_of_clarity_effect () mode =
  match mode with
  | "Easy" -> EasyGameMutable._time := max (EasyGameMutable.time () - 5) 1
  | "Normal" ->
      NormalGameMutable._health := max (NormalGameMutable.time () - 5) 1
  | "Hard" -> HardGameMutable._health := max (HardGameMutable.time () - 5) 1
  | "Extreme" ->
      ExtremeGameMutable._health := max (ExtremeGameMutable.time () - 5) 1
  | "Sudden Death" ->
      SuddenDeathMutable._health := max (SuddenDeathMutable.time () - 5) 1
  | "Chaos" -> ChaosGameMutable._health := max (ChaosGameMutable.time () - 5) 1
  | _ -> failwith "Invalid game mode"

let crystal_of_clarity : item =
  ( CrystalOfClarity,
    Rare,
    "Crystal of Clarity",
    "See the future?",
    [ "??? Time" ],
    wizards_wand_effect )

module type ItemBag = sig
  type items

  val obtain_item : unit -> item
end

module ArrayItemBag : ItemBag = struct
  type items = {
    common : item array;
    rare : item array;
    epic : item array;
    undiscovered : item array;
  }

  let itemlist =
    {
      common = [| apple; banana |];
      rare =
        [|
          broken_clock;
          forgotton_altar;
          bloody_altar;
          jetpack;
          big_frying_pan;
          crystal_of_clarity;
        |];
      epic =
        [|
          edible_clock;
          obfuscinator;
          reverse_jetpack;
          word_eviscer_inator;
          regression_stone;
          wizards_wand;
        |];
      undiscovered = [| chaos; black_cat_trinket |];
    }

  let obtain_item () =
    Random.self_init ();
    let i = itemlist in
    let chosen_number = Random.int 100 in
    if chosen_number < 50 then
      let lst = i.common in
      Array.get lst (Random.int (Array.length lst))
    else if chosen_number < 80 then
      let lst = i.rare in
      Array.get lst (Random.int (Array.length lst))
    else if chosen_number < 95 then
      let lst = i.epic in
      Array.get lst (Random.int (Array.length lst))
    else
      let lst = i.undiscovered in
      Array.get lst (Random.int (Array.length lst))
end

let name_to_string (item : item) =
  let _, _, n, _, _, _ = item in
  n

let effect_to_string (item : item) =
  let i, _, _, _, _, _ = item in
  match i with
  | Apple -> [ "You gained 5 health."; "It still tastes like banana." ]
  | Banana -> [ "You grained 5 health."; "It still tastes like apple." ]
  | BrokenClock -> [ "You gained 10 seconds."; "This was not a tasty clock." ]
  | EdibleClock ->
      [
        "You gained 10 health.";
        "You gained 15 seconds.";
        "This was a tasty clock.";
      ]
  | Chaos -> [ "You were warned..." ]
  | ForgottenAltar ->
      [
        "You gained ??? health.";
        "You lost !!! time.";
        "The concrete material was rather savory.";
      ]
  | BloodyAltar ->
      [
        "You lost ??? health.";
        "You gained !!! time.";
        "It wasn't blood: it was pomegranate juice. ";
      ]
  | Obfuscinator ->
      [
        "Your health became your time and your time became your health???";
        "The Time entity wasn't very tasty...";
      ]
  | Jetpack ->
      [
        "You crashed through the roof, losing 10 heatlth.";
        "But, you skipped a level!!!";
      ]
  | ReverseJetpack ->
      [
        "You put the jetpack on upside down, losing 10 heatlth.";
        "You also went down a level!!!";
      ]
  | BigFryingPan -> [ "Haha! You lost 5 max health! Hahah!!" ]
  | WordEviscerInator ->
      [
        "The hippo thanks you for the feast.";
        "You have 4 less words to deal with in the next round.";
      ]
  | BlackCatTrinket -> [ "You were warned..." ]
  | RegressionStone -> [ "Shwoop da woop! You were sent back in time." ]
  | WizardsWand -> [ "You cast a spell!... on yourself? You feel healthier." ]
  | CrystalOfClarity ->
      [
        "You look into the future...";
        "You lost 5 time.";
        "That was a waste of time.";
      ]

let flavor_to_string (item : item) =
  let _, _, _, f, _, _ = item in
  f

let rarity_to_string (item : item) =
  let _, r, _, _, _, _ = item in
  match r with
  | Common -> "Common"
  | Rare -> "Rare"
  | Epic -> "Epic"
  | Undiscovered -> "Undiscovered"

let stats_to_string (item : item) =
  let _, _, _, _, s, _ = item in
  s

let apply_item (item : item) (mode : string) =
  let _, _, _, _, _, f = item in
  f () mode
