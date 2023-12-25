(* Test Plan - WHAT AND HOW DID WE TEST USING OUNIT: For out testing, we first
   tested our 'bags'. This includes our word bags which hold a random set of
   words that the user has to type and the items bags, which hold the items the
   user has.

   We also test that given a state, the associated items are functioning as
   expected. Each state has different health and time levels which we take into
   account.

   Finally, we test all the items themselves.

   For our test methodology, we used a combinating of Black Box Testing and
   Glass Box Testing. For Black Box testing, we test edge cases for the stats
   (for example what happens if our health hits 0), typical inputs, and we pair
   different initializers with the code for different items (producer consumer
   pairs). For Glass Box Testing, I also made sure to test all possible states,
   and the items associated with each state.

   WHAT PARTS WERE OUNIT TESTED VS MANUALLY TESTED: Everything stated above was
   tested by OUnit. However, there were some things that had to be manually
   tested. For example, the UI interface was all manually tested to make sure
   the user could actually type into the game and we could then check the
   "score" of what the user typed. We also had some states (for example the
   chaos state) where we had randomized stats (eg a randomized health). Because
   they were randomized we could not create OUnit tests for them and so kept
   track of scores and stats when we were in that mode or used those items
   manually. Finally, we had to randomly sample words from a word bag, which was
   also manually tested.

   WHY OUR TESTING PLAN WORKS: We used a combination of Black Box testing, Glass
   Box Testing (also stated above) and manual testing to test the critical
   aspects of our system which are the word bags, the items, associated with
   specific game modes and a functioning user interface. Testing those things
   ensures that when a user runs the game, no bugs like health being below 0, or
   level being negative occur. *)

open OUnit2
module WB = TypeGame.Words.WordBag
module ItemTester = TypeGame.Items
module IBag = ItemTester.ArrayItemBag
module StateTester = TypeGame.State
module GamHard = StateTester.HardGameMutable
module GamEasy = StateTester.EasyGameMutable
module GamExtreme = StateTester.ExtremeGameMutable
module GamSudden = StateTester.SuddenDeathMutable
module GamNormal = StateTester.NormalGameMutable

(********************************************************************
  word tests
 ********************************************************************)

(* [cmp_bag_like_lists lst1 lst2] compares two lists to see whether they are
   equivalent bag-like lists. That means checking that they they contain the
   same elements with the same number of repetitions, though not necessarily in
   the same order. *)
let cmp_bag_like_lists lst1 lst2 =
  let sort1 = List.sort compare lst1 in
  let sort2 = List.sort compare lst2 in
  sort1 = sort2

(* [cmp_bags b1 b2] compares two bags to see whether they are equivalent. *)
let cmp_bags b1 b2 =
  let lst1 = WB.to_list b1 in
  let lst2 = WB.to_list b2 in
  cmp_bag_like_lists lst1 lst2

let empty1 = WB.of_list []
let empty2 = WB.of_list []
let b1 = WB.of_list [ "1" ]
let b2 = WB.of_list [ "1"; "2"; "3" ]
let b3 = WB.of_list [ "3"; "1"; "2" ]
let b6 = WB.of_list [ "3"; "1"; "2"; "4"; "cat"; "dog" ]
let b7 = WB.of_list [ "3"; "1"; "4"; "cat"; "dog"; "2" ]

let of_list_tests =
  [
    ("of_list empty" >:: fun _ -> assert_equal ~cmp:cmp_bags empty1 empty2);
    ("of_list 1 str" >:: fun _ -> assert_equal ~cmp:cmp_bags b1 b1);
    ("of_list 3 str" >:: fun _ -> assert_equal ~cmp:cmp_bags b2 b3);
    ("of_list long" >:: fun _ -> assert_equal ~cmp:cmp_bags b6 b7);
  ]

let to_list_tests =
  [
    ( "to_list empty" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists [] (WB.to_list empty1) );
    ( "to_list 1 str" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists [ "1" ] (WB.to_list b1) );
    ( "to_list 3 str" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists [ "1"; "2"; "3" ] (WB.to_list b2) );
    ( "to_list long" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists
        [ "3"; "1"; "2"; "4"; "cat"; "dog" ]
        (WB.to_list b6) );
  ]

let join_tests =
  [
    ( "join empty" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists []
        (WB.join empty1 empty2 |> WB.to_list) );
    ( "join empty+1" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists [ "1" ]
        (WB.join empty1 b1 |> WB.to_list) );
    ( "join 1+1" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists [ "1"; "1" ]
        (WB.join b1 b1 |> WB.to_list) );
    ( "join 3+1" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists [ "1"; "1"; "2"; "3" ]
        (WB.join b1 b2 |> WB.to_list) );
    ( "join 3+long" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists
        [ "3"; "1"; "2"; "4"; "cat"; "dog"; "1"; "2"; "3" ]
        (WB.join b2 b6 |> WB.to_list) );
  ]

(* manually tested sample becuase it randomly pics words from a bag *)

(********************************************************************
  item tests
 ********************************************************************)
let apple_tests =
  [
    ( "apple_effect" >:: fun _ ->
      GamHard.initialize ();
      GamHard._health := 90;
      ItemTester.apple_effect () "Hard";
      assert_equal (GamHard.health ()) 95 );
    ( "apple_effect max health" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.apple_effect () "Hard";
      assert_equal (GamHard.health ()) 100 );
  ]

let banana_tests =
  [
    ( "banana_effect" >:: fun _ ->
      GamHard.initialize ();
      GamHard._health := 90;
      ItemTester.banana_effect () "Hard";
      assert_equal (GamHard.health ()) 95 );
    ( "banana_effect max health" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.banana_effect () "Hard";
      assert_equal (GamHard.health ()) 100 );
  ]

let broken_clock_tests =
  [
    ( "broken_clock_effect" >:: fun _ ->
      GamHard.initialize ();
      GamHard._time := 50;
      ItemTester.broken_clock_effect () "Hard";
      assert_equal (GamHard.time ()) 60 );
  ]

let edible_clock_tests =
  [
    ( "edible_clock_effect" >:: fun _ ->
      GamHard.initialize ();
      GamHard._health := 50;
      let initial_time = GamHard.time () in
      ItemTester.edible_clock_effect () "Hard";
      assert_equal (GamHard.health ()) 60;
      assert_equal (GamHard.time ()) (initial_time + 15) );
  ]

let forgotten_altar_tests =
  [
    ( "initialize_forgotten_Easy" >:: fun _ ->
      GamEasy.initialize ();
      ItemTester.forgotton_altar_effect () "Easy";
      assert_equal (GamEasy.health ()) 100;
      assert_equal (GamEasy.time ()) 60 );
    ( "initialize_forgotten_Normal" >:: fun _ ->
      GamNormal.initialize ();
      ItemTester.forgotton_altar_effect () "Normal";
      assert_equal (GamNormal.health ()) 100;
      assert_equal (GamNormal.time ()) 60 );
    ( "initialize_forgotten_Hard" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.forgotton_altar_effect () "Hard";
      assert_equal (GamHard.health ()) 100;
      assert_equal (GamHard.time ()) 45 );
    ( "initialize_forgotten_Extreme" >:: fun _ ->
      GamExtreme.initialize ();
      ItemTester.forgotton_altar_effect () "Extreme";
      assert_equal (GamExtreme.health ()) 50;
      assert_equal (GamExtreme.time ()) 30 );
    ( "initialize_forgotten_Sudden" >:: fun _ ->
      GamSudden.initialize ();
      ItemTester.forgotton_altar_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 100;
      assert_equal (GamSudden.time ()) 45 );
  ]

let bloody_altar_tests =
  [
    ( "initialize_bloodAltar" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.bloody_altar_effect () "Hard";
      assert_equal (GamHard.health ()) 50 );
    ( "bloody_altar_effect" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.bloody_altar_effect () "Hard";
      assert_equal (GamHard.health ()) 50;
      assert_equal (GamHard.time ()) 180 );
    ( "bloody_altar_effect 1 hp" >:: fun _ ->
      GamHard._health := 1;
      ItemTester.bloody_altar_effect () "Hard";
      assert_equal (GamHard.health ()) 1;
      assert_equal (GamHard.time ()) 360 );
  ]

let obfuscinator_tests =
  [
    ( "initialize_obfuscinator_Easy" >:: fun _ ->
      GamEasy.initialize ();
      ItemTester.obfuscinator_effect () "Easy";
      assert_equal (GamEasy.health ()) 100;
      assert_equal (GamEasy.time ()) 100 );
    ( "initialize_obfuscinator_Normal" >:: fun _ ->
      GamNormal.initialize ();
      ItemTester.obfuscinator_effect () "Normal";
      assert_equal (GamNormal.health ()) 100;
      assert_equal (GamNormal.time ()) 100 );
    ( "initialize_obfuscinator_Hard" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.obfuscinator_effect () "Hard";
      assert_equal (GamHard.health ()) 90;
      assert_equal (GamHard.time ()) 100 );
    ( "initialize_obfuscinator_Extreme" >:: fun _ ->
      GamExtreme.initialize ();
      ItemTester.obfuscinator_effect () "Extreme";
      assert_equal (GamExtreme.health ()) 50;
      assert_equal (GamExtreme.time ()) 50 );
    ( "initialize_obfuscinator_Sudden" >:: fun _ ->
      GamSudden.initialize ();
      ItemTester.obfuscinator_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 90;
      assert_equal (GamSudden.time ()) 100 );
    ( "initialize_obfuscinator_lowHealth" >:: fun _ ->
      GamEasy.initialize ();
      ItemTester.bloody_altar_effect () "Easy";
      assert_equal (GamEasy.health ()) 50;
      ItemTester.bloody_altar_effect () "Easy";
      assert_equal (GamEasy.health ()) 25;
      ItemTester.bloody_altar_effect () "Easy";
      assert_equal (GamEasy.health ()) 12;
      ItemTester.bloody_altar_effect () "Easy";
      assert_equal (GamEasy.health ()) 6;
      ItemTester.bloody_altar_effect () "Easy";
      assert_equal (GamEasy.health ()) 3;
      ItemTester.bloody_altar_effect () "Easy";
      assert_equal (GamEasy.health ()) 1;
      ItemTester.obfuscinator_effect () "Easy";
      assert_equal (GamEasy.health ()) 100;
      assert_equal (GamEasy.time ()) 1;
      ItemTester.obfuscinator_effect () "Easy";
      assert_equal (GamEasy.health ()) 1;
      assert_equal (GamEasy.time ()) 100;
      ItemTester.jetpack_effect () "Easy";
      assert_equal (GamEasy.health ()) 0;
      ItemTester.obfuscinator_effect () "Easy";
      assert_equal (GamEasy.health ()) 100;
      assert_equal (GamEasy.time ()) 0 );
  ]

let jet_pack_tests =
  [
    ( "initialize_jetpack" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.jetpack_effect () "Hard";
      assert_equal (GamHard.health ()) 90 );
    ( "jetpack_effect" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.jetpack_effect () "Hard";
      assert_equal (GamHard.health ()) 90;
      assert_equal (GamHard.cur_level ()) 1 );
    ( "reverse_jetpack_effect" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.jetpack_effect () "Hard";
      ItemTester.reverse_jetpack_effect () "Hard";
      assert_equal (GamHard.health ()) 80;
      assert_equal (GamHard.cur_level ()) 0 );
  ]

let fryingpan_tests =
  [
    ( "initialize_fryingpan_Easy" >:: fun _ ->
      GamEasy.initialize ();
      ItemTester.big_frying_pan_effect () "Easy";
      assert_equal (GamEasy.health ()) 95;
      assert_equal (GamEasy.max_health ()) 95 );
    ( "initialize_fryingpan_Normal" >:: fun _ ->
      GamNormal.initialize ();
      ItemTester.big_frying_pan_effect () "Normal";
      assert_equal (GamNormal.health ()) 95;
      assert_equal (GamNormal.max_health ()) 95 );
    ( "initialize_fryingpan_Hard" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.big_frying_pan_effect () "Hard";
      assert_equal (GamHard.health ()) 95;
      assert_equal (GamHard.max_health ()) 95 );
    ( "initialize_fryingpan_Extreme" >:: fun _ ->
      GamExtreme.initialize ();
      ItemTester.big_frying_pan_effect () "Extreme";
      assert_equal (GamExtreme.health ()) 45;
      assert_equal (GamExtreme.max_health ()) 45 );
    ( "initialize_fryingpan_Sudden" >:: fun _ ->
      GamSudden.initialize ();
      ItemTester.big_frying_pan_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 95;
      assert_equal (GamSudden.max_health ()) 95 );
  ]

let word_eviscer_inator_tests =
  [
    ( "initialize_eviscer_Easy" >:: fun _ ->
      GamEasy.initialize ();
      ItemTester.word_eviscer_inator_effect () "Easy";
      assert_equal (GamEasy.num_words ()) 46 );
    ( "initialize_eviscer_Normal" >:: fun _ ->
      GamNormal.initialize ();
      ItemTester.word_eviscer_inator_effect () "Normal";
      assert_equal (GamNormal.num_words ()) 46 );
    ( "initialize_eviscer_Hard" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.word_eviscer_inator_effect () "Hard";
      assert_equal (GamHard.num_words ()) 56 );
    ( "initialize_eviscer_Extreme" >:: fun _ ->
      GamExtreme.initialize ();
      ItemTester.word_eviscer_inator_effect () "Extreme";
      assert_equal (GamExtreme.num_words ()) 56 );
    ( "initialize_eviscer_Sudden" >:: fun _ ->
      GamSudden.initialize ();
      ItemTester.word_eviscer_inator_effect () "Sudden Death";
      assert_equal (GamSudden.num_words ()) 56 );
  ]

let black_cat_tests =
  [
    ( "initialize_black_cat_Easy" >:: fun _ ->
      GamEasy.initialize ();
      ItemTester.black_cat_trinket_effect () "Easy";
      assert_equal (GamEasy.health ()) 1;
      assert_equal (GamEasy.max_health ()) 1 );
    ( "initialize_black_cat_Normal" >:: fun _ ->
      GamNormal.initialize ();
      ItemTester.black_cat_trinket_effect () "Normal";
      assert_equal (GamNormal.health ()) 1;
      assert_equal (GamNormal.max_health ()) 1 );
    ( "initialize_black_cat_Hard" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.black_cat_trinket_effect () "Hard";
      assert_equal (GamHard.health ()) 1;
      assert_equal (GamHard.max_health ()) 1 );
    ( "initialize_black_cat_Extreme" >:: fun _ ->
      GamExtreme.initialize ();
      ItemTester.black_cat_trinket_effect () "Extreme";
      assert_equal (GamExtreme.health ()) 1;
      assert_equal (GamExtreme.max_health ()) 1 );
    ( "initialize_black_cat_Sudden" >:: fun _ ->
      GamSudden.initialize ();
      ItemTester.black_cat_trinket_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 1;
      assert_equal (GamSudden.max_health ()) 1 );
  ]

let regression_tests =
  [
    ( "initialize_regression_Easy" >:: fun _ ->
      GamEasy.initialize ();
      ItemTester.regression_stone_effect () "Easy";
      assert_equal (GamEasy.cur_level ()) 0 );
    ( "initialize_regression_Normal" >:: fun _ ->
      GamNormal.initialize ();
      ItemTester.regression_stone_effect () "Normal";
      assert_equal (GamNormal.cur_level ()) 0 );
    ( "initialize_regression_Hard" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.regression_stone_effect () "Hard";
      assert_equal (GamHard.cur_level ()) 0 );
    ( "initialize_regression_Extreme" >:: fun _ ->
      GamExtreme.initialize ();
      ItemTester.regression_stone_effect () "Extreme";
      assert_equal (GamExtreme.cur_level ()) 0 );
    ( "initialize_regression_Sudden" >:: fun _ ->
      GamSudden.initialize ();
      ItemTester.regression_stone_effect () "Sudden Death";
      assert_equal (GamSudden.cur_level ()) 0 );
  ]

let edge_tests =
  [
    ( "max_health + 5, losing health and changing level" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.banana_effect () "Hard";
      assert_equal (GamHard.health ()) 100;
      ItemTester.jetpack_effect () "Hard";
      assert_equal (GamHard.health ()) 90;
      assert_equal (GamHard.cur_level ()) 1;
      ItemTester.reverse_jetpack_effect () "Hard";
      assert_equal (GamHard.health ()) 80;
      assert_equal (GamHard.cur_level ()) 0 );
    ( "0 health , changing level" >:: fun _ ->
      GamEasy.initialize ();
      ItemTester.bloody_altar_effect () "Easy";
      assert_equal (GamEasy.health ()) 50;
      assert_equal (GamEasy.time ()) 240;
      ItemTester.bloody_altar_effect () "Easy";
      assert_equal (GamEasy.health ()) 25;
      ItemTester.jetpack_effect () "Easy";
      assert_equal (GamEasy.health ()) 15;
      assert_equal (GamEasy.cur_level ()) 1;
      ItemTester.jetpack_effect () "Easy";
      assert_equal (GamEasy.health ()) 5;
      assert_equal (GamEasy.cur_level ()) 2;
      ItemTester.jetpack_effect () "Easy";
      assert_equal (GamEasy.health ()) 0;
      assert_equal (GamEasy.cur_level ()) 3 );
    ( "too much health, swapping health and time " >:: fun _ ->
      GamExtreme.initialize ();
      ItemTester.edible_clock_effect () "Extreme";
      assert_equal (GamExtreme.health ()) 50;
      assert_equal (GamExtreme.time ()) 75;
      ItemTester.edible_clock_effect () "Extreme";
      assert_equal (GamExtreme.health ()) 50;
      assert_equal (GamExtreme.time ()) 90;
      ItemTester.edible_clock_effect () "Extreme";
      assert_equal (GamExtreme.health ()) 50;
      assert_equal (GamExtreme.time ()) 105;
      ItemTester.obfuscinator_effect () "Extreme";
      assert_equal (GamExtreme.health ()) 50;
      assert_equal (GamExtreme.time ()) 50 );
    ( "dropping time " >:: fun _ ->
      GamSudden.initialize ();
      ItemTester.forgotton_altar_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 100;
      assert_equal (GamSudden.time ()) 45;
      ItemTester.forgotton_altar_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 100;
      assert_equal (GamSudden.time ()) 22;
      ItemTester.forgotton_altar_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 100;
      assert_equal (GamSudden.time ()) 11;
      ItemTester.forgotton_altar_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 100;
      assert_equal (GamSudden.time ()) 5;
      ItemTester.broken_clock_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 100;
      assert_equal (GamSudden.time ()) 15 );
    ( "dropping number of words " >:: fun _ ->
      GamNormal.initialize ();
      assert_equal (GamNormal.num_words ()) 50;
      ItemTester.word_eviscer_inator_effect () "Normal";
      assert_equal (GamNormal.num_words ()) 46;
      ItemTester.word_eviscer_inator_effect () "Normal";
      assert_equal (GamNormal.num_words ()) 42;
      ItemTester.word_eviscer_inator_effect () "Normal";
      assert_equal (GamNormal.num_words ()) 38;
      ItemTester.word_eviscer_inator_effect () "Normal";
      assert_equal (GamNormal.num_words ()) 34;
      ItemTester.word_eviscer_inator_effect () "Normal";
      assert_equal (GamNormal.num_words ()) 30;
      ItemTester.word_eviscer_inator_effect () "Normal";
      assert_equal (GamNormal.num_words ()) 26;
      ItemTester.word_eviscer_inator_effect () "Normal";
      assert_equal (GamNormal.num_words ()) 22;
      ItemTester.word_eviscer_inator_effect () "Normal";
      assert_equal (GamNormal.num_words ()) 18;
      ItemTester.word_eviscer_inator_effect () "Normal";
      assert_equal (GamNormal.num_words ()) 14;
      ItemTester.word_eviscer_inator_effect () "Normal";
      assert_equal (GamNormal.num_words ()) 10;
      ItemTester.word_eviscer_inator_effect () "Normal";
      assert_equal (GamNormal.num_words ()) 10 );
    ( "attempting level below 0" >:: fun _ ->
      GamSudden.initialize ();
      assert_equal (GamSudden.cur_level ()) 0;
      ItemTester.reverse_jetpack_effect () "Sudden Death";
      assert_equal (GamSudden.cur_level ()) 0;
      assert_equal (GamSudden.health ()) 90;
      ItemTester.regression_stone_effect () "Sudden Death";
      assert_equal (GamSudden.cur_level ()) 0;
      ItemTester.jetpack_effect () "Sudden Death";
      assert_equal (GamSudden.cur_level ()) 1;
      assert_equal (GamSudden.health ()) 80;
      ItemTester.regression_stone_effect () "Sudden Death";
      assert_equal (GamSudden.cur_level ()) 0;
      ItemTester.edible_clock_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 90;
      assert_equal (GamSudden.time ()) 105;
      ItemTester.apple_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 95;
      ItemTester.banana_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 100 );
  ]

(* manually tested chaos effect, what items get chosen, chaos items (item has a
   random effect) *)
let word_tests = to_list_tests @ of_list_tests @ join_tests

let item_tests =
  edge_tests @ apple_tests @ banana_tests @ broken_clock_tests
  @ edible_clock_tests @ forgotten_altar_tests @ bloody_altar_tests
  @ obfuscinator_tests @ jet_pack_tests @ fryingpan_tests
  @ word_eviscer_inator_tests @ black_cat_tests @ regression_tests

let tests = "test suite" >::: word_tests @ item_tests
let () = run_test_tt_main tests
