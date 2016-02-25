module GridParameterVerifier = struct
  let row_mask_len = function Grid064 -> 1 | Grid128 -> 2 | Grid256 -> 2 | Grid512 -> 4
  let col_mask_len = function Grid064 -> 1 | Grid128 -> 1 | Grid256 -> 2 | Grid512 -> 2

  let row_len = function Grid064 -> 8 | Grid128 ->  8 | Grid256 -> 16 | Grid512 -> 16
  let col_len = function Grid064 -> 8 | Grid128 -> 16 | Grid256 -> 16 | Grid512 -> 32

  let mask { size = s } dimension masks = dimension s = List.length masks

  let intensities_range leds =
    match leds with Binary     -> List.for_all (fun i -> i  = '\000' || i = '\001')
                  | Quaternary -> List.for_all (fun i -> i >= '\000' && i < '\005')
                  | Sedenary   -> List.for_all (fun i -> i >= '\000' && i < '\017')
  let intensities grid { size = s ; leds = d } dimension (intensities : intensity list) =
    intensities_range d intensities && dimension s = List.length intensities
end

