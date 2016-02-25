module type OscMessageBuilder = sig
  val build : oscMessage -> string
  val bool : oscSelector -> bool -> oscMessage
  val chr : oscSelector -> char -> oscMessage
  val int : oscSelector -> int -> oscMessage
  val str : oscSelector -> string -> oscMessage
  val chr_list : oscSelector -> char list -> oscMessage
  val int_list : oscSelector -> int list -> oscMessage
  val str_list : oscSelector -> string list -> oscMessage
end