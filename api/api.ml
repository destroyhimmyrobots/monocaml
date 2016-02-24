(* API for Monome Interpreter *)
exception OSCMessageInputError
exception OSCMessageParseError
exception OSCMessageMatchError
exception OSCValueMarshallError

type grid_edition = Grid40h        (* 2006-2007 Tiltmod Binary *)
                  | Grid40hSE      (* 2007	Tilt	Binary *)
                  | Grid40hKit     (* 2007-2010 	Binary *)
                  | GridSeries064  (* 2007-2010 Tilt	Binary *)
                  | GridSeries128  (* 2007-2010 No	Binary *)
                  | GridSeries256  (* 2007-2010 No	Binary *)
                  | GridMK8x8      (* 2010 *)
                  | GridMK8x16     (* 2010 *)
                  | GridMK16x16    (* 2010 *)
                  | GridGrey064    (* 2010-2012 Tilt	Binary	*)
                  | GridGrey128    (* 2010-2012	Tilt	Binary	*)
                  | Grid064        (* 2011-2012 Tilt	Quaternary-11/Sedenary-12 *)
                  | Grid128        (* 2011-2015 Tilt-12	Quaternary-11/Sedenary-12 *)
                  | Grid256        (* 2011-2012 Tilt	Quaternary-11/Sedenary-12 *)
                  | Grid512        (* 2010 *)
type grid_size = Grid064
               | Grid128
               | Grid256
               | Grid512
type grid_leds = Binary
               | Quaternary
               | Sedenary
type grid_tilt = Nulliary       (* 2007-2010, 2013-2015 *)
               | Uniaxis        (* 2006-2011 *)
               | Triaxis        (* 2012 only *)
type intensity = char (* LED brightness: 0x00 to 0x0F or '\000' to '\016' *)
type grid_state = intensity array array
type grid = { size  : grid_size
            ; leds  : grid_leds
            ; tilt  : grid_tilt
            ; state : grid_state }
type mask = char                    (* LED configuration: 8 bits. 0x00 to 0xFF or '\000' to '\255'. *)
type row  = char                    (* 0x00 to 0x0F or '\000' to '\016' *)
type col  = char                    (* 0x00 to 0x0F or '\000' to '\016' *)
type point = { row : row ; col : col }
type key_state = { key : point ; state : bool }
type offset   = Noll | Atta
type quadrant = I | II | III | IV
type tiltValue = Roll of int
               | Yaw of int
               | Pitch of int (* 8-bit integers *)
type tiltSensor = Sensor of int
type oscSelector = OSCSelector of string
type oscValue = OSCBool of bool
              | OSCInt of int
              | OSCInt8 of char
              | OSCString of string
              | OSCSelectorValue of oscSelector
              | OSCList of oscValue list
type oscMessage = { selector : oscSelector
                  ; parameters : oscValue list }

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

module GridParameterVerifier = struct
  let row_mask_len = function Grid064 -> 1 | Grid128 -> 2 | Grid256 -> 2 | Grid512 -> 4
  let col_mask_len = function Grid064 -> 1 | Grid128 -> 1 | Grid256 -> 2 | Grid512 -> 2

  let row_len = function Grid064 -> 8 | Grid128 ->  8 | Grid256 -> 16 | Grid512 -> 16
  let col_len = function Grid064 -> 8 | Grid128 -> 16 | Grid256 -> 16 | Grid512 -> 32

  let mask { size = s } dimension masks = dimension s = Array.length masks

  let intensities_range leds =
    match leds with Binary     -> List.for_all (fun i -> i  = '\000' || i = '\001')
                  | Quaternary -> List.for_all (fun i -> i >= '\000' && i < '\005')
                  | Sedenary   -> List.for_all (fun i -> i >= '\000' && i < '\017')
  let intensities grid { size = s ; leds = d } dimension (intensities : intensity list) =
    intensities_range d intensities && dimension s = List.length intensities
end

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

module OscBuilder : OscMessageBuilder = struct
  let bool_list s bs = { selector = s; parameters = List.map (fun x -> OSCBool x  ) bs }
  let  chr_list s cs = { selector = s; parameters = List.map (fun x -> OSCInt8 x  ) cs }
  let  int_list s is = { selector = s; parameters = List.map (fun x -> OSCInt  x  ) is }
  let  str_list s ss = { selector = s; parameters = List.map (fun t -> OSCString t) ss }
  let bool s b = bool_list s [b]
  let  chr s c =  chr_list s [c]
  let  int s i =  int_list s [i]
  let  str s t =  str_list s [t]

  let concat_osc = String.concat " "
  let rec string_of_osc = function
    | OSCBool b -> if b then "1" else "0"
    | OSCInt i -> string_of_int i
    | OSCInt8 c -> Char.escaped c
    | OSCString s -> s
    | OSCSelectorValue (OSCSelector s) -> s
    | OSCList l -> concat_osc_list l
  and concat_osc_list w = concat_osc (List.map string_of_osc w)
  let build {selector = s; parameters = p} = concat_osc_list (OSCSelectorValue s :: p)
end

(* DRY Module for building simple OSC Matchers *)
module type OscMatcher = sig
  val matcher : oscSelector -> (oscValue -> oscValue) list -> (oscMessage -> oscValue list)
  val chr : oscSelector -> oscMessage -> char
  val int : oscSelector -> oscMessage -> int
  val str : oscSelector -> oscMessage -> string
end

module OscMatcher : OscMatcher = struct
  let matcher sel param_matchers =
    let rec aux values matchers = match values, matchers with
      | v :: vs , m :: ms -> (try m v
                              with Match_failure _ -> raise OSCMessageParseError) :: aux vs ms
      | [], [] -> []
      | _ -> raise OSCMessageParseError
    in function { selector   = s'
                ; parameters = p' } when sel = s' -> aux p' param_matchers
              | _ -> raise OSCValueMarshallError

  (* TODO: int_list matcher *)
  let int sel message =
    match matcher sel [function OSCInt i -> OSCInt i] message
    with | [OSCInt i] -> i
         | _ -> raise OSCValueMarshallError
  let chr sel message =
    match matcher sel [function OSCInt8 c -> OSCInt8 c] message
    with | [OSCInt8 c] -> c
         | _ -> raise OSCValueMarshallError
  let str sel message =
    match matcher sel [function OSCString s -> OSCString s] message
    with | [OSCString s] -> s
         | _ -> raise OSCValueMarshallError
end

module type DaemonSendApi = sig
  val list : string -> int -> oscMessage   (* List connected devices *)
  val notify : string -> int -> oscMessage (* Change listening address *)
end

module type DaemonRecvApi = sig
  val device : oscMessage -> string * string * int (* Currently connected device *)
  val add : oscMessage -> string    (* Device connected *)
  val remove : oscMessage -> string (* Device disconnected *)
end

module type DeviceSysSendApi = sig
  val port : int -> oscMessage      (* Set client listening port *)
  val host : string -> oscMessage   (* Set client listening post *)
  val prefix : string -> oscMessage (* Set device message filter *)
  val rotation : int -> oscMessage  (* Set device rotation 0 90 180 270 *)
end

module type DeviceSysRecvApi = sig
  val host : oscMessage -> string        (* device listen host *)
  val id : oscMessage -> string          (* device id  *)
  val port : oscMessage -> int           (* device listen port *)
  val prefix : oscMessage -> oscSelector (* application suffix *)
  val rotation : oscMessage -> int       (* grid device rotation *)
  val size : oscMessage -> int * int     (* grid device size *)
end

module type DeviceInfoSendApi = sig
  val info : unit -> oscMessage (* Receive device info messages at this application *)
  val info' : int -> oscMessage (* Receive device info messages at localhost:port *)
  val info'' : string -> int -> oscMessage (* Receive device info messages to host:port *)
end

module type DeviceGridSendApi = sig
  (* TODO: point should be a function of grid size *)
  val led_set : grid -> key_state -> oscMessage              (* Set x, y to state *)
  val led_all : grid -> bool -> oscMessage                       (* Set all  to state *)
  (* TODO: offset should be a function of grid size *)
  val led_map : grid -> quadrant -> int list list -> oscMessage    (* Set rows of quad  *)
  (* TODO: led_row & led_col: limit size of lists based on row, col *)
  val led_row : grid -> offset -> row -> mask array -> oscMessage (* Set rows at offset to states *)
  val led_col : grid -> col -> offset -> mask array -> oscMessage (* Set cols at offset to states *)
  val led_intensity : grid -> intensity -> oscMessage            (* ??? *)
  val led_level_set : grid -> point -> intensity array -> oscMessage   (* Set x, y to intensity *)
  val led_level_all : grid -> intensity -> oscMessage            (* Set all to intensity *)
  (* NB. array64 used because the intensities are 4-bit values, unlike led_map *)
  val led_level_map : grid -> quadrant -> intensity array -> oscMessage
  (* TODO: led_level_row & led_level_col: limit size of lists based on row, col *)
  val led_level_row : grid -> offset -> row -> intensity list -> oscMessage
  val led_level_col : grid -> col -> offset -> intensity list -> oscMessage
  val tilt_set : grid -> tiltSensor -> bool -> oscMessage
end

module type DeviceGridRecvApi = sig
  val key  : grid -> oscMessage -> key_state
  val tilt : grid -> oscMessage -> tiltSensor * (tiltValue * tiltValue * tiltValue)
end

module type DeviceSysApi = sig
  val suffix : string -> oscSelector
  module Send : DeviceSysSendApi
  module Receive : DeviceSysRecvApi
end

module type DeviceInfoApi = sig
  val suffix : string -> oscSelector
  module Send : DeviceInfoSendApi
end

module type DeviceGridApi = sig
  val suffix  : string -> oscSelector
  module Send : DeviceGridSendApi
  module Receive : DeviceGridRecvApi
end

module type DeviceApi = sig
  module Sys  : DeviceSysApi
  module Info : DeviceInfoApi
  module Grid : DeviceGridApi
end

module type DaemonApi = sig
  val suffix : string -> oscSelector
  module Send : DaemonSendApi
  module Receive : DaemonRecvApi
end

(* Module describing OSC interactions with SerialOSC (i.e., Monome) devices *)
module type SerialOscApi = sig
  module Daemon : DaemonApi
  module Device : DeviceApi
end

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
        let port   = OscBuilder.int (suffix "/port"  )
        let host   = OscBuilder.str (suffix "/host"  )
        let prefix = OscBuilder.str (suffix "/prefix")
        let rotation r = match r with | 0 | 90 | 180 | 270 -> OscBuilder.int (suffix "/rotation") r
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
        let info ()          = OscBuilder.str (suffix "") "" (* TODO: Does empty string work? Trailing spaces... *)
        let info'  port      = OscBuilder.int (suffix "") port
        let info'' host port = { selector = suffix ""
                               ; parameters = [OSCString host; OSCInt port] }
      end
    end

    module Grid = struct
      let suffix = with_suffix "/grid"
      module Send = struct
        let led_set _ { key = { row = r ; col = c } ; state = s } =
          OscBuilder.chr_list (suffix "/led/set") [r; c; char_of_bool s]
        let led_all _ =
          OscBuilder.bool (suffix "/led/all")
        let led_intensity _ b =
          OscBuilder.chr (suffix "/led/intensity") b
        let led_level_set _ { row = r ; col = c } b =
          OscBuilder.chr_list (suffix "/led/level/set") [r; c; b]
        let led_level_all _ b =
          OscBuilder.int (suffix "/led/level/all") b
        let tilt_set _ sensor state =
          OscBuilder.int_list (suffix "/tilt") [(function Sensor s -> s) sensor; int_of_bool state]
        let led_map _ quad row_states =
          if 8 <> List.length row_states then
            invalid_arg "Input must specify eight rows of states."
          else                  (* TODO: Guarantee that each row has 8 states, using a custom type. *)
            let states =  List.map (function r -> state_list_to_bit_vector r |> string_of_int) row_states
            in OscBuilder.str (suffix "/led/map") (String.concat " " (string_of_quad quad :: states))
        (* TODO: Use a custom type for masks rather than exceptions. *)
        let led_row g offset row masks =
          let open GridParameterVerifier in
          if mask g col_mask_len masks then
            OscBuilder.str (suffix "/led/row")
                           (OSCInt8 offset :: List.map (fun m -> OSCInt8 m) masks)
          else
            invalid_arg "Masks must match row length."
                        (* TODO: Use a custom type for masks rather than exceptions. *)
        let led_col g col offset masks =
          let open GridParameterVerifier in
          if mask g col_mask_len masks then
            OscBuilder.str (suffix "/led/col")
                           (OSCInt8 offset :: List.map (fun m -> OSCInt8 m) masks)
          else
            invalid_arg "Masks must match column length."
        let led_level_map g quad intensities =
          OscBuilder.str (suffix "/led/level/map") "0 0 0"
        let led_level_row g offset row intensities =
          let open GridParameterVerifier in
          if intensities g row_len intensities then
            OscBuilder.str (suffix "/led/level/row")
                           (OSCInt offset :: List.map (fun m -> OSCInt8 m) intensities)
          else
            invalid_arg "Intensities must match row length."
        let led_level_col g col offset intensities =
          let open GridParameterVerifier in
          if intensities g col_len intensities then
            OscBuilder.str (suffix "/lef/level/col")
                           (OSCInt offset :: List.map (fun m -> OSCInt8 m) intensities)
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
