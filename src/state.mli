(** A model for mutable game states. *)
module type GameStateMutable = sig

  val initialize :
    (* int list (* if we want to dynamically initialize the game state *) -> *)
    unit ->
    unit
  (** Set starting state values *)

  val time : unit -> int
  (** Time allotted for round in seconds. *)

  val _time : int ref
  (** Time allotted for round in seconds. *)

  val _num_words : int ref
  (** Number of words to be generated. *)

  val _difficulty : int ref
  (** Difficulty level of words generated. *)

  val _health : int ref
  (** Current health. *)

  val _max_health : int ref
  (** Current max health. *)

  val _cur_level : int ref
  (** Number of rounds player has passed so far. *)

  val _score : int ref
  (** Current player score.*)

  val _num_items : int ref
  (** Number of items generated each time.*)

  val num_words : unit -> int
  (** Gets the number of words to be generated. *)

  val difficulty : unit -> int
  (** Gets the difficulty level of words generated. *)

  val health : unit -> int
  (** Gets the current health. *)

  val max_health : unit -> int
  (** Gets the current max health. *)

  val cur_level : unit -> int
  (** Gets the number of rounds player has passed so far. *)

  val score : unit -> int
  (** Gets the current player score.*)

  val num_items : unit -> int
  (** Gets the number of items generated each time.*)

  val health_lost : int -> int -> int -> int * int
  (** Calculates health lost given time remaining, words wrong and words
      remaining. *)

  val adjust_level : unit -> unit
  (** Adjust level depending on number of rounds. *)

  val decrement_level : unit -> unit
  (** Decrease level.*)

  val add_score : int -> int
  (** Add score based on number of correct words as passed in.*)
end

module NormalGameMutable : GameStateMutable
(** A module for normal mode. *)

module EasyGameMutable : GameStateMutable
(** A module for easy mode. *)

module HardGameMutable : GameStateMutable
(** A module for hard mode. *)

module ExtremeGameMutable : GameStateMutable
(** A module for extreme mode. *)

module SuddenDeathMutable : GameStateMutable
(** A module for sudden death mode. *)

module ChaosGameMutable : GameStateMutable
(** A module for chaos mode. *)


