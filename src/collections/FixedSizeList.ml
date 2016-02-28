module MakeFixedSizeArray : FIXEDSIZEARRAY =
  functor (A : TFixedSizeArray) ->
  struct
    type t = A.t array
    let make () = Array.make A.size A.zero
    let set a i v = Array.set a i v; a
    let get a i = Array.get a i
  end

module FixedSizeByteArray2 =
  MakeFixedSizeArray(struct
                      type t = char
                      let zero = '\000'
                      let size = 2
                    end)

module FixedSizeByteArray16 =
  MakeFixedSizeArray(struct
                      type t = char
                      let zero = '\000'
                      let size = 16
                    end)

module FixedSizeByteArray64 =
  MakeFixedSizeArray(struct
                      type t = char
                      let zero = '\000'
                      let size = 64
                    end)
