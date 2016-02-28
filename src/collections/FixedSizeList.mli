module type TFixedSizeArray =
  sig
    type t
    val zero : t
    val size : int
  end

module type FIXEDSIZEARRAY =
  functor (A : TFixedSizeArray) ->
  sig
    type t
    val make : unit -> t
    val set  : t -> int -> A.t -> t
    val get  : t -> int -> A.t
  end
