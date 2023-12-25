Random.self_init ()

module type GameStateMutable = sig
  val initialize :
    (* int list (* if we want to dynamically initialize the game state *) -> *)
    unit ->
    unit
  (** Set starting state values *)

  val time : unit -> int
  (** Time allotted for round in seconds *)

  val _time : int ref
  val _num_words : int ref
  val _difficulty : int ref
  val _health : int ref
  val _max_health : int ref
  val _cur_level : int ref
  val _score : int ref
  val _num_items : int ref

  val num_words : unit -> int
  (** Number of words to be generated *)

  val difficulty : unit -> int
  (** Difficulty level of words generated *)

  val health : unit -> int
  (** Current health *)

  val max_health : unit -> int
  (** Current max health *)

  val cur_level : unit -> int
  (** Number of rounds player has passed so far *)

  val score : unit -> int

  val num_items : unit -> int
  (** the number of items drawn each time*)

  val health_lost : int -> int -> int -> int * int
  (** Calculates health lost given time remaining, words wrong, words remaining *)

  val adjust_level : unit -> unit
  (** Adjust level depending on number of rounds *)

  val decrement_level : unit -> unit
  (** Decrease level *)

  val add_score : int -> int
  (** Adjust score based on number of correct words typed*)
end

(* Mutable version of the below class. *)
module NormalGameMutable : GameStateMutable = struct
  let _time = ref 0
  let _num_words = ref 0
  let _difficulty = ref 0
  let _health = ref 0
  let _max_health = ref 0
  let _cur_level = ref 0
  let _score = ref 0
  let _num_items = ref 3

  let initialize () =
    _time := 120;
    _num_words := 50;
    _difficulty := 1;
    _health := 100;
    _max_health := 100;
    _cur_level := 0;
    _score := 0

  let time () = !_time
  let num_words () = !_num_words
  let difficulty () = !_difficulty
  let health () = !_health
  let max_health () = !_max_health
  let cur_level () = !_cur_level
  let score () = !_score
  let num_items () = !_num_items

  let health_lost time_left missed words_left =
    _health :=
      min
        (!_health + (time_left / 5) - (missed * 5) - (words_left * 5))
        (max_health ());
    ((missed * 5) - (words_left * 5), time_left / 5)

  let adjust_level () =
    _cur_level := !_cur_level + 1;
    _num_words := !_num_words + 5;
    _time := max 10 (!_time - 5)

  let decrement_level () =
    _cur_level := max (!_cur_level - 1) 0;
    _num_words := max 10 (!_num_words - 5);
    _time := !_time + 5

  let add_score correct =
    _score := !_score + (correct * 2);
    correct * 2
end

module EasyGameMutable : GameStateMutable = struct
  let _time = ref 0
  let _num_words = ref 0
  let _difficulty = ref 0
  let _health = ref 0
  let _max_health = ref 0
  let _cur_level = ref 0
  let _score = ref 0
  let _num_items = ref 4

  let initialize () =
    _time := 120;
    _num_words := 50;
    _difficulty := 1;
    _health := 100;
    _max_health := 100;
    _cur_level := 0;
    _score := 0

  let time () = !_time
  let num_words () = !_num_words
  let difficulty () = !_difficulty
  let health () = !_health
  let max_health () = !_max_health
  let cur_level () = !_cur_level
  let score () = !_score
  let num_items () = !_num_items

  let health_lost time_left missed words_left =
    _health :=
      min (!_health + (time_left / 5) - missed - words_left) (max_health ());
    (0 - missed - words_left, time_left / 5)

  let adjust_level () =
    _cur_level := !_cur_level + 1;
    _num_words := !_num_words + 5;
    _time := !_time

  let decrement_level () =
    _cur_level := max (!_cur_level - 1) 0;
    _num_words := max 10 (!_num_words - 5);
    _time := !_time

  let add_score correct =
    _score := !_score + correct;
    correct
end

module HardGameMutable : GameStateMutable = struct
  let _time = ref 0
  let _num_words = ref 0
  let _difficulty = ref 0
  let _health = ref 0
  let _max_health = ref 0
  let _cur_level = ref 0
  let _score = ref 0
  let _num_items = ref 2

  let initialize () =
    _time := 90;
    _num_words := 60;
    _difficulty := 1;
    _health := 100;
    _max_health := 100;
    _cur_level := 0;
    _score := 0

  let time () = !_time
  let num_words () = !_num_words
  let difficulty () = !_difficulty
  let health () = !_health
  let max_health () = !_max_health
  let cur_level () = !_cur_level
  let score () = !_score
  let num_items () = !_num_items

  let health_lost time_left missed words_left =
    _health :=
      min
        (!_health + (time_left / 10) - (missed * 10) - (words_left * 10))
        (max_health ());
    (0 - (missed * 10) - (words_left * 10), time_left / 10)

  let adjust_level () =
    _cur_level := !_cur_level + 1;
    _num_words := !_num_words + 5;
    _time := max 10 (!_time - 10)

  let decrement_level () =
    _cur_level := max (!_cur_level - 1) 0;
    _num_words := max 10 (!_num_words - 5);
    _time := !_time + 10

  let add_score correct =
    _score := !_score + (correct * 10);
    correct * 10
end

module ExtremeGameMutable : GameStateMutable = struct
  let _time = ref 0
  let _num_words = ref 0
  let _difficulty = ref 0
  let _health = ref 0
  let _max_health = ref 0
  let _cur_level = ref 0
  let _score = ref 0
  let _num_items = ref 2

  let initialize () =
    _time := 60;
    _num_words := 60;
    _difficulty := 1;
    _health := 50;
    _max_health := 50;
    _cur_level := 0;
    _score := 0

  let time () = !_time
  let num_words () = !_num_words
  let difficulty () = !_difficulty
  let health () = !_health
  let max_health () = !_max_health
  let cur_level () = !_cur_level
  let score () = !_score
  let num_items () = !_num_items

  let health_lost time_left missed words_left =
    _health :=
      min
        (!_health + (time_left / 10) - (missed * 10) - (words_left * 10))
        (max_health ());
    (0 - (missed * 10) - (words_left * 10), time_left / 10)

  let adjust_level () =
    _cur_level := !_cur_level + 1;
    _num_words := !_num_words + 10;
    _time := max 10 (!_time - 10)

  let decrement_level () =
    _cur_level := max (!_cur_level - 1) 0;
    _num_words := max 10 (!_num_words - 10);
    _time := !_time + 10

  let add_score correct =
    _score := !_score + (correct * 10);
    correct * 10
end

module SuddenDeathMutable : GameStateMutable = struct
  let _time = ref 0
  let _num_words = ref 0
  let _difficulty = ref 0
  let _health = ref 0
  let _max_health = ref 0
  let _cur_level = ref 0
  let _score = ref 0
  let _num_items = ref 2

  let initialize () =
    _time := 90;
    _num_words := 60;
    _difficulty := 1;
    _health := 100;
    _max_health := 100;
    _cur_level := 0;
    _score := 0

  let time () = !_time
  let num_words () = !_num_words
  let difficulty () = !_difficulty
  let health () = !_health
  let max_health () = !_max_health
  let cur_level () = !_cur_level
  let score () = !_score
  let num_items () = !_num_items

  let health_lost time_left missed words_left =
    _health :=
      min
        (!_health
        + (-missed * !_max_health * 10)
        - (words_left * !_max_health * 10)
        + (time_left / 10))
        (max_health ());
    ( 0 - (missed * !_max_health * 10) - (words_left * !_max_health * 10),
      time_left / 10 )

  let adjust_level () =
    _cur_level := !_cur_level + 1;
    _num_words := !_num_words + 5;
    _time := max 10 (!_time - 10)

  let decrement_level () =
    _cur_level := max (!_cur_level - 1) 0;
    _num_words := max 10 (!_num_words - 5);
    _time := !_time + 10

  let add_score correct =
    _score := !_score + (correct * 15);
    correct * 15
end

module ChaosGameMutable : GameStateMutable = struct
  let _time = ref 0
  let _num_words = ref 0
  let _difficulty = ref 0
  let _health = ref 0
  let _max_health = ref 0
  let _cur_level = ref 0
  let _score = ref 0
  let _num_items = ref 2

  let initialize () =
    _time := Random.int 120 + 60;
    _num_words := Random.int 60 + 15;
    _difficulty := 1;
    _health := Random.int 100 + 50;
    _max_health := 100;
    _cur_level := 0;
    _score := 0

  let time () = !_time
  let num_words () = !_num_words
  let difficulty () = !_difficulty
  let health () = !_health
  let max_health () = !_max_health
  let cur_level () = !_cur_level
  let score () = !_score
  let num_items () = !_num_items

  let health_lost time_left missed words_left =
    let loss_multiplier = Random.int 20 in
    let gain_multiplier = Random.int 19 + 1 in
    _health :=
      min
        (!_health
        + (-missed * loss_multiplier)
        - (words_left * loss_multiplier)
        + (time_left / gain_multiplier))
        (max_health ());
    ( 0 - (missed * loss_multiplier) - (words_left * loss_multiplier),
      time_left / gain_multiplier )

  let adjust_level () =
    _cur_level := !_cur_level + 1;
    _num_words := Random.int 120 + 60;
    _max_health := Random.int 100 + 50;
    _health := min !_max_health !_health;
    _time := Random.int 60 + 15

  let decrement_level () =
    _cur_level := max (!_cur_level - 1) 0;
    _num_words := Random.int 120 + 60;
    _time := Random.int 60 + 15

  let add_score correct =
    let multiplier = Random.int 100 in
    _score := !_score + (correct * multiplier);
    correct * multiplier
end
