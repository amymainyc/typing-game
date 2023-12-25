(** A model of sampleable bags (multisets). *)
module type SampleBagType = sig
    type t
    (** Type representing the data in the bag. *)
  
    val to_list : t -> string list
    (** Convert a sampleable bag to a list of items. *)
  
    val of_list : string list -> t
    (** Convert a list of items into a sampleable bag. *)
  
    val join : t -> t -> t
    (** Combine two sampleable bags. The multiplicity of an item in the output bag
        is the sum of the multiplicities of the item in the two input bags. *)
  
    val sample : t -> string option
    (** Draw an item from the sampleable bag. Return [None] if the bag is empty. *)
  end
  
  module WordBag : SampleBagType
  (** Sampleable bag such that sample returns elements with probability
      proportional to their multiplicity. *)
  
  val sanitize : string -> string list
  (** Sanitize a string by removing non-alphanumeric symbols. Return the list of
      words obtained by splitting on spaces. *)
  
  val data_loader : string -> string list
  (** Loads words from a text file and converts into a list of strings *)
  
  val data : string list
  (** Stores the list of words from "data/1-1000.txt". *)
  
  val word_bag : WordBag.t
  (** The WordBag representation of data. *)
  
  val word_bag_t : int -> WordBag.t
  (** The [word_bag_t i] Creates a WordBag of words up to [i] * 50 *)
  
  val generate_sequence : WordBag.t -> int -> string list
  (** Generates a random sequence of length n from words in the WordBag *)