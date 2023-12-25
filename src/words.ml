module type SampleBagType = sig
  type t

  val to_list : t -> string list
  val of_list : string list -> t
  val join : t -> t -> t
  val sample : t -> string option
end

module WordBag : SampleBagType = struct
  type t = string list

  let to_list (b : t) : t = b
  let of_list (lst : t) : t = lst
  let join (b1 : t) (b2 : t) : t = List.append b1 b2

  let sample (b : t) : string option =
    match b with
    | [] -> None
    | _ -> Some (List.length b |> Random.int |> List.nth b)
end

let sanitize (s : string) : string list =
  s
  |> Str.global_replace (Str.regexp "[ \t\r\n]") " "
  |> Str.global_replace (Str.regexp "[^a-zA-Z0-9' ]") ""
  |> String.lowercase_ascii |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")

let data_loader (filename : string) : string list =
  In_channel.open_text filename |> In_channel.input_all |> sanitize

let data = data_loader "data/1-1000.txt"
let word_bag = WordBag.of_list data

let word_bag_t n =
  let rec helper n lst =
    if n = 0 then []
    else
      match lst with
      | [] -> []
      | h :: t -> h :: helper (n - 1) t
  in
  WordBag.of_list (helper (n * 50) data)

let rec generate_sequence (words : WordBag.t) (n : int) : string list =
  if n > 0 then
    let w = WordBag.sample words in
    match w with
    | None -> []
    | Some word -> word :: generate_sequence words (n - 1)
  else []
