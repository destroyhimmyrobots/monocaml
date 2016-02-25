(* API for Monome Interpreter *)
module SerialOscClient : SerialOscApi = struct
  module Daemon = struct
    let suffix = with_suffix "/serialosc"
    module Send = struct
      let list   host port = { selector   = suffix "/list"
                             ; parameters = [OSCString host; OSCInt port] }
      let notify host port = { selector   = suffix "/notify"
                             ; parameters = [OSCString host; OSCInt port] }
    end
    module Receive = struct
      let add    = OscMatcher.str (suffix "/add")
      let remove = OscMatcher.str (suffix "/remove")
      let device = function
          { selector = s ; parameters = [ OSCString id
                                        ; OSCString typ
                                        ; OSCInt port] }
             when s = suffix "/device" -> id, typ, port
        | _ -> raise OSCMessageParseError
    end
  end

  module Device = struct
    module Sys  = struct
      let suffix = with_suffix "/sys"
      module Send = struct
        let port   = OscMessageBuilder.int (suffix "/port"  )
        let host   = OscMessageBuilder.str (suffix "/host"  )
        let prefix = OscMessageBuilder.str (suffix "/prefix")
        let rotation r = match r with | 0 | 90 | 180 | 270 -> OscMessageBuilder.int (suffix "/rotation") r
                                      | _ -> raise OSCMessageInputError
      end
      module Receive = struct
        let host     = OscMatcher.str (suffix "/host"    )
        let id       = OscMatcher.str (suffix "/id"      )
        let port     = OscMatcher.int (suffix "/port"    )
        let prefix p = OSCSelector (OscMatcher.str (suffix "/prefix") p)
        let rotation = OscMatcher.int (suffix "/rotation")
        let size     = function
          | { selector = s ; parameters = [OSCInt w; OSCInt h] }
               when s = suffix "/size" -> w, h
          | _ -> raise OSCMessageParseError
      end
    end

    module Info = struct
      let suffix = with_suffix "/info" (* TODO: Same as /sys/info?  *)
      module Send = struct
        let info ()          = OscMessageBuilder.str (suffix "") "" (* TODO: Does empty string work? Trailing spaces... *)
        let info'  port      = OscMessageBuilder.int (suffix "") port
        let info'' host port = { selector = suffix ""
                               ; parameters = [OSCString host; OSCInt port] }
      end
    end

    module Grid = struct
      let suffix = with_suffix "/grid"
      module Send = struct
        let led_set _ { key = { row = r ; col = c } ; state = s } =
          OscMessageBuilder.chr_list (suffix "/led/set") [r; c; char_of_bool s]
        let led_all _ =
          OscMessageBuilder.bool (suffix "/led/all")
        let led_intensity _ b =
          OscMessageBuilder.chr (suffix "/led/intensity") b
        let led_level_set _ { row = r ; col = c } b =
          OscMessageBuilder.chr_list (suffix "/led/level/set") [r; c; b]
        let led_level_all _ b =
          OscMessageBuilder.int (suffix "/led/level/all") b
        let tilt_set _ sensor state =
          OscMessageBuilder.int_list (suffix "/tilt") [(function Sensor s -> s) sensor; int_of_bool state]
        let led_map _ quad row_states =
          if 8 <> List.length row_states then
            invalid_arg "Input must specify eight rows of states."
          else                  (* TODO: Guarantee that each row has 8 states, using a custom type. *)
            let states =  List.map (function r -> state_list_to_bit_vector r |> string_of_int) row_states
            in OscMessageBuilder.str (suffix "/led/map") (String.concat " " (string_of_quad quad :: states))
        (* TODO: Use a custom type for masks rather than exceptions. *)
        let led_row g offset row masks =
          let open GridParameterVerifier in
          if mask g col_mask_len masks then
            OscMessageBuilder.chr_list (suffix "/led/row")
                                       (offset :: masks)
          else
            invalid_arg "Masks must match row length."
        (* TODO: Use a custom type for masks rather than exceptions. *)
        let led_col g col offset masks =
          let open GridParameterVerifier in
          if mask g col_mask_len masks then
            OscMessageBuilder.chr_list (suffix "/led/col")
                                       (offset :: masks)
          else
            invalid_arg "Masks must match column length."
        let led_level_map g quad intensities =
          OscMessageBuilder.str (suffix "/led/level/map") "0 0 0"
        let led_level_row g offset row intensities =
          let open GridParameterVerifier in
          if intensities g row_len intensities then
            OscMessageBuilder.chr_list (suffix "/led/level/row")
                                       (offset :: intensities)
          else
            invalid_arg "Intensities must match row length."
        let led_level_col g col offset intensities =
          let open GridParameterVerifier in
          if intensities g col_len intensities then
            OscMessageBuilder.chr_list (suffix "/lef/level/col")
                                       (offset :: intensities)
          else
            invalid_arg "Intensities must match row length."
      end
      module Receive = struct
        let key = function
          | { selector = s ; parameters = [OSCInt r; OSCInt c; OSCInt t] }
               when s = suffix "/key" -> { key = { row = Char.chr r
                                                 ; col = Char.chr c }
                                         ; state = 0 <> t }
          | _ -> raise OSCMessageParseError
        let tilt = function
            { selector = s ; parameters = [OSCInt n; OSCInt x; OSCInt y; OSCInt z] }
               when s = suffix "/tilt" -> Sensor n, (Roll x, Pitch y, Yaw z)
          | _ -> raise OSCMessageParseError
      end
    end
  end
end
