module type OscMatcher = sig
  val matcher : oscSelector -> (oscValue -> oscValue) list -> (oscMessage -> oscValue list)
  val chr : oscSelector -> oscMessage -> char
  val int : oscSelector -> oscMessage -> int
  val str : oscSelector -> oscMessage -> string
end