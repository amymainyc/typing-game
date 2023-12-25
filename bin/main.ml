(*Because of a decision to structure our game state module more like a java
  class, we do not have a type t and our variables are all bound to the original
  instance of the class that was created. We did not realize that this would be
  incompatible with functors (which return a new, separate instance of the game
  state and thus do not have the variables at the same value). However, at the
  point where we realized this issue, it would not have been worth the time and
  effort to do a complete overhaul of the system*)
open TypeGame

type round_state = {
  pos : int * int;
  mutable str : string;
  fin : bool;
  mutable typed : int;
  mutable right : int;
  mutable wrong : int;
  mutable words : string list;
  mutable differences : (string * string) list;
}

(**Given a string representing the game mode, **)
let game_get_time gm =
  if gm = "Easy" then State.EasyGameMutable.time ()
  else if gm = "Normal" then State.NormalGameMutable.time ()
  else if gm = "Hard" then State.HardGameMutable.time ()
  else if gm = "Extreme" then State.ExtremeGameMutable.time ()
  else if gm = "Sudden Death" then State.SuddenDeathMutable.time ()
  else if gm = "Chaos" then State.ChaosGameMutable.time ()
  else failwith "cringe"

let game_get_num_words gm =
  if gm = "Easy" then State.EasyGameMutable.num_words ()
  else if gm = "Normal" then State.NormalGameMutable.num_words ()
  else if gm = "Hard" then State.HardGameMutable.num_words ()
  else if gm = "Extreme" then State.ExtremeGameMutable.num_words ()
  else if gm = "Sudden Death" then State.SuddenDeathMutable.num_words ()
  else if gm = "Chaos" then State.ChaosGameMutable.num_words ()
  else failwith "cringe"

let game_get_difficulty gm =
  if gm = "Easy" then State.EasyGameMutable.difficulty ()
  else if gm = "Normal" then State.NormalGameMutable.difficulty ()
  else if gm = "Hard" then State.HardGameMutable.difficulty ()
  else if gm = "Extreme" then State.ExtremeGameMutable.difficulty ()
  else if gm = "Sudden Death" then State.SuddenDeathMutable.difficulty ()
  else if gm = "Chaos" then State.ChaosGameMutable.difficulty ()
  else failwith "cringe"

let game_get_health gm =
  if gm = "Easy" then State.EasyGameMutable.health ()
  else if gm = "Normal" then State.NormalGameMutable.health ()
  else if gm = "Hard" then State.HardGameMutable.health ()
  else if gm = "Extreme" then State.ExtremeGameMutable.health ()
  else if gm = "Sudden Death" then State.SuddenDeathMutable.health ()
  else if gm = "Chaos" then State.ChaosGameMutable.health ()
  else failwith "cringe"

let game_get_max_health gm =
  if gm = "Easy" then State.EasyGameMutable.max_health ()
  else if gm = "Normal" then State.NormalGameMutable.max_health ()
  else if gm = "Hard" then State.HardGameMutable.max_health ()
  else if gm = "Extreme" then State.ExtremeGameMutable.max_health ()
  else if gm = "Sudden Death" then State.SuddenDeathMutable.max_health ()
  else if gm = "Chaos" then State.ChaosGameMutable.max_health ()
  else failwith "cringe"

let game_get_cur_level gm =
  if gm = "Easy" then State.EasyGameMutable.cur_level ()
  else if gm = "Normal" then State.NormalGameMutable.cur_level ()
  else if gm = "Hard" then State.HardGameMutable.cur_level ()
  else if gm = "Extreme" then State.ExtremeGameMutable.cur_level ()
  else if gm = "Sudden Death" then State.SuddenDeathMutable.cur_level ()
  else if gm = "Chaos" then State.ChaosGameMutable.cur_level ()
  else failwith "cringe"

let game_get_score gm =
  if gm = "Easy" then State.EasyGameMutable.score ()
  else if gm = "Normal" then State.NormalGameMutable.score ()
  else if gm = "Hard" then State.HardGameMutable.score ()
  else if gm = "Extreme" then State.ExtremeGameMutable.score ()
  else if gm = "Sudden Death" then State.SuddenDeathMutable.score ()
  else if gm = "Chaos" then State.ChaosGameMutable.score ()
  else failwith "cringe"

let game_get_num_items gm =
  if gm = "Easy" then State.EasyGameMutable.num_items ()
  else if gm = "Normal" then State.NormalGameMutable.num_items ()
  else if gm = "Hard" then State.HardGameMutable.num_items ()
  else if gm = "Extreme" then State.ExtremeGameMutable.num_items ()
  else if gm = "Sudden Death" then State.SuddenDeathMutable.num_items ()
  else if gm = "Chaos" then State.ChaosGameMutable.num_items ()
  else failwith "cringe"

let game_health_lost gm time wrong remaining =
  if gm = "Easy" then State.EasyGameMutable.health_lost time wrong remaining
  else if gm = "Normal" then
    State.NormalGameMutable.health_lost time wrong remaining
  else if gm = "Hard" then
    State.HardGameMutable.health_lost time wrong remaining
  else if gm = "Extreme" then
    State.ExtremeGameMutable.health_lost time wrong remaining
  else if gm = "Sudden Death" then
    State.SuddenDeathMutable.health_lost time wrong remaining
  else if gm = "Chaos" then
    State.ChaosGameMutable.health_lost time wrong remaining
  else failwith "cringe"

let game_adjust_level gm =
  if gm = "Easy" then State.EasyGameMutable.adjust_level ()
  else if gm = "Normal" then State.NormalGameMutable.adjust_level ()
  else if gm = "Hard" then State.HardGameMutable.adjust_level ()
  else if gm = "Extreme" then State.ExtremeGameMutable.adjust_level ()
  else if gm = "Sudden Death" then State.SuddenDeathMutable.adjust_level ()
  else if gm = "Chaos" then State.ChaosGameMutable.adjust_level ()
  else failwith "cringe"

let game_add_score gm right =
  if gm = "Easy" then State.EasyGameMutable.add_score right
  else if gm = "Normal" then State.NormalGameMutable.add_score right
  else if gm = "Hard" then State.HardGameMutable.add_score right
  else if gm = "Extreme" then State.ExtremeGameMutable.add_score right
  else if gm = "Sudden Death" then State.SuddenDeathMutable.add_score right
  else if gm = "Chaos" then State.ChaosGameMutable.add_score right
  else failwith "cringe"

let game_initialize gm =
  if gm = "Easy" then State.EasyGameMutable.initialize ()
  else if gm = "Normal" then State.NormalGameMutable.initialize ()
  else if gm = "Hard" then State.HardGameMutable.initialize ()
  else if gm = "Extreme" then State.ExtremeGameMutable.initialize ()
  else if gm = "Sudden Death" then State.SuddenDeathMutable.initialize ()
  else if gm = "Chaos" then State.ChaosGameMutable.initialize ()
  else failwith "cringe"

(*for keeping spacing relatively consistent*)
let new_line = function
  | x, y -> if x < 800 then () else Graphics.moveto 100 (y - 25)

(*things happening at each tick of a round*)
let round_tick start_time time_allotted (state : round_state) =
  let cur_time = Unix.gettimeofday () in
  let time_passed =
    int_of_float (Float.round ((cur_time -. start_time) *. 100.0))
  in
  let frac_sec = time_passed mod 100 in
  let string_frac_sec =
    if frac_sec < 10 then "0" ^ string_of_int frac_sec
    else string_of_int frac_sec
  in
  let sec = time_passed / 100 mod 60 in
  let string_sec =
    if sec >= 10 then string_of_int sec else "0" ^ string_of_int sec
  in
  let min = time_passed / 6000 in
  let time_string =
    string_of_int min ^ ":" ^ string_sec ^ "." ^ string_frac_sec
  in
  Graphics.moveto 100 900;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 100 900 100 30;
  Graphics.set_color Graphics.black;
  Graphics.draw_string time_string;
  if time_passed >= time_allotted then (
    Graphics.moveto 100 100;
    Graphics.draw_string "You've run out of time. Press <ENTER> to continue.";
    let cont = ref false in
    while !cont = false do
      if Graphics.key_pressed () then
        if Graphics.read_key () = Char.chr 13 then cont := true
    done;
    { state with fin = true })
  else
    let x_pos = fst state.pos in
    let y_pos = snd state.pos in
    match Graphics.key_pressed () with
    | true ->
        Graphics.moveto x_pos y_pos;
        let key = Graphics.read_key () in
        if key = Char.chr 8 then
          if String.length state.str = 0 then state
          else (
            Graphics.set_color Graphics.white;
            Graphics.fill_rect (x_pos - 6) y_pos 6 13;
            Graphics.moveto (x_pos - 6) y_pos;
            {
              state with
              pos = (x_pos - 6, y_pos);
              str = String.sub state.str 0 (String.length state.str - 1);
            })
        else if key = ' ' || key = Char.chr 13 then (
          Graphics.draw_char key;
          state.typed <- state.typed + 1;
          (match state.words with
          | [] -> state.wrong <- state.wrong + 1
          | h :: t ->
              if h = state.str then (
                state.right <- state.right + 1;
                state.words <- t)
              else (
                state.wrong <- state.wrong + 1;
                Graphics.set_color Graphics.red;
                Graphics.fill_rect x_pos y_pos 6 13;
                Graphics.set_color Graphics.white;
                state.words <- t;
                state.differences <- (h, state.str) :: state.differences));
          state.str <- "";
          new_line (x_pos, y_pos);
          if key = ' ' then { state with pos = Graphics.current_point () }
          else { state with fin = true })
        else if Char.code key >= 97 && Char.code key <= 122 then (
          Graphics.draw_char key;
          {
            state with
            pos = Graphics.current_point ();
            str = state.str ^ String.make 1 key;
          })
        else state
    | false -> state

let item_select gm =
  let rec item_gen n acc =
    if n = 0 then acc
    else item_gen (n - 1) (Items.ArrayItemBag.obtain_item () :: acc)
  in
  let item_list = item_gen (game_get_num_items gm) [] in
  let rec draw_list_spaced stat_list =
    match stat_list with
    | [] -> ()
    | h :: t ->
        Graphics.draw_string h;
        new_line (1000, Graphics.current_y ());
        draw_list_spaced t
  in
  let rec print_items n lst =
    match lst with
    | [] -> ()
    | h :: t ->
        Graphics.draw_string
          ("Item <" ^ string_of_int n ^ ">: " ^ Items.name_to_string h);
        new_line (1000, Graphics.current_y ());
        Graphics.draw_string ("[" ^ Items.rarity_to_string h ^ "]");
        new_line (1000, Graphics.current_y ());
        Graphics.draw_string (Items.flavor_to_string h);
        new_line (1000, Graphics.current_y ());
        draw_list_spaced (Items.stats_to_string h);
        new_line (1000, Graphics.current_y ());
        print_items (n + 1) t
  in
  Graphics.moveto 100 900;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 1000 1000;
  Graphics.set_color Graphics.black;
  Graphics.draw_string "Choose an item:";
  new_line (1000, Graphics.current_y ());
  new_line (1000, Graphics.current_y ());
  print_items 1 item_list;
  let item_chosen = ref false in
  while not !item_chosen do
    if Graphics.key_pressed () then
      let key = Graphics.read_key () in
      let ascii_key = Char.code key in
      if ascii_key >= 48 && ascii_key <= 57 then (
        let item_num_string = String.make 1 key in
        let item_num = int_of_string item_num_string in
        if item_num > 0 && item_num <= game_get_num_items gm then (
          let chosen_item = List.nth item_list (item_num - 1) in
          Graphics.draw_string
            ("You've chosen item " ^ item_num_string ^ ": "
            ^ Items.name_to_string chosen_item
            ^ "!");
          new_line (1000, Graphics.current_y ());
          draw_list_spaced (Items.effect_to_string chosen_item);
          Items.apply_item chosen_item gm;
          new_line (1000, Graphics.current_y ());
          item_chosen := true;
          Graphics.draw_string "<PRESS ANY KEY TO CONTINUE>";
          ignore (Graphics.wait_next_event [ Key_pressed ]);
          Graphics.set_color Graphics.white;
          Graphics.fill_rect 0 0 1000 1000;
          Graphics.set_color Graphics.black))
      else (
        Graphics.draw_string "Please choose a valid item number.";
        new_line (1000, Graphics.current_y ()))
    else ()
  done;
  ()

let round gm =
  Graphics.moveto 100 900;
  let word_list =
    Words.generate_sequence
      (Words.word_bag_t (game_get_difficulty gm))
      (game_get_num_words gm)
  in
  let words_given = List.length word_list in
  let rec print_words = function
    | [] -> ()
    | h :: t ->
        Graphics.draw_string (h ^ " ");
        new_line (Graphics.current_point ());
        print_words t
  in
  let start_time = Unix.gettimeofday () in
  let finished = ref false in
  Graphics.set_color Graphics.black;
  Graphics.moveto 100 900;
  Graphics.draw_string
    ("-" ^ gm ^ " LEVEL " ^ string_of_int (game_get_cur_level gm) ^ "-");
  new_line (1000, Graphics.current_y ());
  new_line (1000, Graphics.current_y ());
  Graphics.draw_string "This round you will have:";
  new_line (1000, Graphics.current_y ());
  Graphics.draw_string
    (string_of_int (game_get_num_words gm)
    ^ " words in "
    ^ string_of_int (game_get_time gm)
    ^ " seconds.");
  Graphics.moveto 100 100;
  Graphics.draw_string "Are you ready? <PRESS ANY KEY TO START ROUND>";
  ignore (Graphics.wait_next_event [ Key_pressed ]);
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 1000 1000;
  Graphics.set_color Graphics.black;
  Graphics.moveto 100 875;
  Graphics.draw_string
    ("[Current Health: " ^ string_of_int (game_get_health gm) ^ "] ");
  Graphics.draw_string
    ("[Current Score: " ^ string_of_int (game_get_score gm) ^ "] ");
  Graphics.draw_string
    ("[Total Seconds Allowed This Level: "
    ^ string_of_int (game_get_time gm)
    ^ "] ");
  Graphics.moveto 100 825;
  print_words word_list;
  new_line (1000, Graphics.current_y ());
  new_line (1000, Graphics.current_y ());
  let rs_tick =
    ref
      {
        pos = Graphics.current_point ();
        str = "";
        fin = false;
        typed = 0;
        right = 0;
        wrong = 0;
        words = word_list;
        differences = [ ("", "") ];
      }
  in
  while !finished = false do
    Unix.sleepf 0.001;
    rs_tick := round_tick start_time (game_get_time gm * 100) !rs_tick;
    finished := !rs_tick.fin
  done;
  let cur_time = Unix.gettimeofday () in
  let time_passed = int_of_float (Float.round (cur_time -. start_time)) in
  let time_given = game_get_time gm in
  Unix.sleepf 1.0;
  let words_left = Int.abs words_given - !rs_tick.typed in
  let hp =
    game_health_lost gm (time_given - time_passed) !rs_tick.wrong words_left
  in
  item_select gm;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 1000 1000;
  Graphics.set_color Graphics.black;
  Graphics.moveto 100 900;
  Graphics.draw_string
    (gm ^ " Level " ^ string_of_int (game_get_cur_level gm) ^ " Results :");
  new_line (1000, Graphics.current_y ());
  new_line (1000, Graphics.current_y ());
  let accuracy = !rs_tick.right * 100 / words_given in
  Graphics.draw_string
    ("Your accuracy was " ^ string_of_int accuracy ^ "% with "
    ^ string_of_int !rs_tick.right
    ^ " words typed correctly and "
    ^ string_of_int !rs_tick.wrong
    ^ " words typed incorrectly out of " ^ string_of_int words_given
    ^ " words given.");
  let health_lost_string =
    if fst hp = 0 then " (no health lost)"
    else " (-" ^ string_of_int (fst hp) ^ " health)"
  in
  Graphics.draw_string health_lost_string;
  let cur_health = game_get_health gm in
  new_line (1000, Graphics.current_y ());
  Graphics.draw_string
    ("You also used up " ^ string_of_int time_passed ^ " seconds out of "
    ^ string_of_int (game_get_time gm)
    ^ " seconds given.");
  let health_gained_string =
    if snd hp = 0 then " (no health gained)"
    else " (+ " ^ string_of_int (snd hp) ^ " health)"
  in
  Graphics.draw_string health_gained_string;
  new_line (1000, Graphics.current_y ());
  Graphics.draw_string "With item effects:";
  new_line (1000, Graphics.current_y ());
  Graphics.draw_string
    ("You are now at "
    ^ string_of_int (game_get_health gm)
    ^ "/"
    ^ string_of_int (game_get_max_health gm)
    ^ " health");
  new_line (1000, Graphics.current_y ());
  Graphics.draw_string "Here's what you got wrong: ";
  new_line (1000, Graphics.current_y ());
  let rec diffs_to_strings (diffs : (string * string) list) (acc : string list)
      =
    match diffs with
    | [] -> acc
    | h :: t -> (
        match h with
        | original, given ->
            let stringified = "|" ^ original ^ "-" ^ given ^ "|" in
            diffs_to_strings t (stringified :: acc))
  in
  let stringed_diffs = diffs_to_strings (List.tl !rs_tick.differences) [] in
  print_words stringed_diffs;
  let pos = Graphics.current_point () in
  new_line (1000, snd pos);
  Graphics.draw_string "Here's what you didn't get around to:";
  let pos = Graphics.current_point () in
  new_line (1000, snd pos);
  print_words !rs_tick.words;
  let pos = Graphics.current_point () in
  new_line (1000, snd pos);
  if cur_health > 0 then (
    let score_gain = game_add_score gm !rs_tick.right in
    Graphics.draw_string ("You gained " ^ string_of_int score_gain ^ " points!");
    new_line (1000, Graphics.current_y ());
    Graphics.draw_string
      ("Your current score is " ^ string_of_int (game_get_score gm) ^ ".");
    true)
  else (
    Graphics.draw_string "You died! Better luck next time.";
    new_line (1000, Graphics.current_y ());
    Graphics.draw_string
      ("Your final score was " ^ string_of_int (game_get_score gm));
    false)

let () =
  (*Game initialization*)
  Graphics.open_graph " 1000x1000+0+0";
  Graphics.moveto 100 900;
  Graphics.set_color Graphics.black;
  Graphics.draw_string "Rules : ";
  new_line (1000, Graphics.current_y ());
  new_line (1000, Graphics.current_y ());
  Graphics.draw_string "1. You lose health for getting words wrong";
  new_line (1000, Graphics.current_y ());
  Graphics.draw_string
    "2. Once you press space, we consider that a new word! You can't go back \
     to fix your mistakes :)";
  new_line (1000, Graphics.current_y ());
  Graphics.draw_string
    "3. If you don't finish, then any words left over will also cost you some \
     health :)";
  new_line (1000, Graphics.current_y ());
  Graphics.draw_string
    "4. Press <ENTER> if you finish early. If you have time left over, you \
     might regain some health :)";
  Graphics.moveto 100 100;
  Graphics.draw_string "<PRESS ANY KEY TO CONTINUE>";
  ignore (Graphics.wait_next_event [ Key_pressed ]);
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 1000 1000;
  Graphics.set_color Graphics.black;
  Graphics.moveto 100 900;
  Graphics.draw_string "CHOOSE A GAME MODE:";
  new_line (1000, Graphics.current_y ());
  new_line (1000, Graphics.current_y ());
  Graphics.set_color Graphics.green;
  Graphics.draw_string "1 : Easy - More time, Less Hurt";
  new_line (1000, Graphics.current_y ());
  Graphics.set_color Graphics.black;
  Graphics.draw_string "2 : Normal";
  new_line (1000, Graphics.current_y ());
  Graphics.draw_string "3 : Hard - Less time, More hurt";
  new_line (1000, Graphics.current_y ());
  Graphics.draw_string "4 : Extreme - Even Less time, Even more hurt";
  new_line (1000, Graphics.current_y ());
  Graphics.set_color Graphics.red;
  Graphics.draw_string "5 : Sudden Death - One mistakes and you die";
  new_line (1000, Graphics.current_y ());
  Graphics.set_color Graphics.magenta;
  Graphics.draw_string "6 : Chaos - Heheheha";
  new_line (1000, Graphics.current_y ());
  Graphics.set_color Graphics.black;
  let gm =
    let gm_chosen = ref "" in
    while !gm_chosen = "" do
      if Graphics.key_pressed () then
        let key_char = Graphics.read_key () in
        let ascii_key = Char.code key_char in
        if ascii_key >= 48 && ascii_key <= 57 then
          let gm_num_string = String.make 1 key_char in
          let gm_num = int_of_string gm_num_string in
          if gm_num = 1 then gm_chosen := "Easy"
          else if gm_num = 2 then gm_chosen := "Normal"
          else if gm_num = 3 then gm_chosen := "Hard"
          else if gm_num = 4 then gm_chosen := "Extreme"
          else if gm_num = 5 then gm_chosen := "Sudden Death"
          else if gm_num = 6 then gm_chosen := "Chaos"
    done;
    Graphics.set_color Graphics.white;
    Graphics.fill_rect 0 0 1000 1000;
    Graphics.set_color Graphics.black;
    !gm_chosen
  in
  game_initialize gm;
  let playing = ref true in
  (*Game loop*)
  while !playing do
    playing := round gm;
    Graphics.moveto 100 100;
    Graphics.draw_string "<PRESS ANY KEY TO CONTINUE>";
    ignore (Graphics.wait_next_event [ Key_pressed ]);
    game_adjust_level gm;
    Graphics.set_color Graphics.white;
    Graphics.fill_rect 0 0 1000 1000
  done
