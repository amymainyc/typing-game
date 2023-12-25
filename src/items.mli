type name
(** The names of all possible items in the game. *)

type rarity
(** How rare this item is. *)

type item
(** The type of every item in the game. Includes a [name], a [rarity], a display
    name as a [string], an item description as a [string], a stats description
    as a [string], and an effect as a (unit -> unit). *)

val apple_effect : unit -> string -> unit
(** Raised hp by 5. *)

val banana_effect : unit -> string -> unit
(** Raised hp by 5. *)

val broken_clock_effect : unit -> string -> unit
(** Raised time by 10. *)

val edible_clock_effect : unit -> string -> unit
(** Raises hp by 10, time by 15. *)

val chaos_effect : unit -> string -> unit
(** Chaos, chaos. *)

val forgotton_altar_effect : unit -> string -> unit
(** The effect of the Forgotton Altar (* ominous music plays *). *)

val bloody_altar_effect : unit -> string -> unit
(** The effect of the Bloody Altar (* less ominous music plays *). *)

val obfuscinator_effect : unit -> string -> unit
(** Swaps your health and your time. *)

val jetpack_effect : unit -> string -> unit
(** Sends you up one level at the cost of 10 health. *)

val reverse_jetpack_effect : unit -> string -> unit
(** Sends you down one level at the cost of 10 health. *)

val big_frying_pan_effect : unit -> string -> unit
(** Reduces your max health by 5. Sets health to the lower of the max health and
    the current health. *)

val word_eviscer_inator_effect : unit -> string -> unit
(** Removes 4 from num_words. Lowest it can go is 10.*)

val black_cat_trinket_effect : unit -> string -> unit
(** An unfortunate encounter. Reduces max health by current health, then sets
    your current health to that max health. *)

val regression_stone_effect : unit -> string -> unit
(** Halves your current level. *)

(** Adds a random amount of health from 1-20. *)
val wizards_wand_effect : unit -> string -> unit

(** Subtracts 5 time. *)
val crystal_of_clarity_effect : unit -> string -> unit

(** The signature for the module that randomly selects an item. *)
module type ItemBag = sig
  type items
  (** The data representation for pulling an item. *)

  val obtain_item : unit -> item
  (** Randomly obtain an item. *)
end

module ArrayItemBag : ItemBag

val name_to_string : item -> string
(** Returns the name of the item. *)

val effect_to_string : item -> string list
(** Returns a series of effect statements, separated by the newline character,
    stating what changes the player received, of the item. *)

val flavor_to_string : item -> string
(** Returns the funny funny flavor text of the item. *)

val rarity_to_string : item -> string
(** Returns the rarity of the item. *)

val stats_to_string : item -> string list
(** Returns the stats of the item. *)

val apply_item : item -> string -> unit
(** Applies the item. *)
