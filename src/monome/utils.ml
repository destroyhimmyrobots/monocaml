let empty_osc_message = { selector = OSCSelector ""
                        ; parameters = [] }

let with_suffix = function a -> fun w -> OSCSelector (a ^ w)
let int_of_bool  = function true -> 1 | false -> 0
let char_of_bool = function true -> '\001' | false -> '\000'
let string_of_offset = function Noll -> "0"
                              | Atta -> "8"
let string_of_quad = function I   -> "0 8"
                            | II  -> "0 0"
                            | III -> "8 0"
                            | IV  -> "8 8"
let state_list_to_bit_vector states =
  let len = List.length states |> float_of_int
  and sum_list = List.fold_left (+) 0 in
  let summands index s =
    match s with
    | 0 | 1 -> let i = float_of_int index
               in float_of_int s *. 2. ** (len -. 1.0 -. i) |> truncate
    | _ -> invalid_arg (string_of_int s)
  in List.mapi summands states |> sum_list

let intensity_of_int { leds = d } i : intensity =
  match d with Sedenary -> Char.chr (max (min i 16) 0)
             | Binary   -> Char.chr (16 * int_of_bool (i <> 0))
             | Quaternary -> match i with
                           | i' when i' < 1  -> '\000'
                           | i' when i' < 5  -> '\001'
                           | i' when i' < 9  -> '\003'
                           | _ -> '\004'
