(* API for Monome Interpreter *)
exception OSCMessageInputError
exception OSCMessageParseError
exception OSCMessageMatchError
exception OSCValueMarshallError

type offset = Noll | Atta
type intensity = Brightness of char (* LED brightness: 0x00 to 0x0F or '\000' to '\016' *)
type mask = char                    (* LED configuration: 0x00 to 0xFF or '\000' to '\255' *)
type row = char                     (* 0x00 to 0x0F or '\000' to '\016' *)
type col = char                     (* 0x00 to 0x0F or '\000' to '\016' *)
type point = { row : row ; col : col }
type key_state = { row : row ; col : col ; state : bool }
type quadrant = offset * offset
type tilt_sensor = Roll | Yaw | Pitch
type 'a array8  = 'a array
type 'a array16 = 'a array
type 'a array64 = 'a array
type oscSelector = OSCSelector of string
type oscValue = OSCBool of bool
              | OSCInt of int
              | OSCString of string
              | OSCSelectorValue of oscSelector
              | OSCList of oscValue list
type oscMessage = { selector : oscSelector
                  ; parameters : oscValue list }
(* type 'a oscValue' = OSCValue' of 'a
                     | OSCPair' of oscValue' * oscValue'
                     | OSCTriple' of oscValue' * oscValue' *)

let int_of_bool = function true -> 1 | false -> 0

(* DRY Module for building simple OSC Messages *)
module OscBuilder : sig
  val build : oscMessage -> string
  val bool : oscSelector -> bool -> oscMessage
  val int : oscSelector -> int -> oscMessage
  val str : oscSelector -> string -> oscMessage
  val int_list : oscSelector -> int list -> oscMessage
  val str_list : oscSelector -> string list -> oscMessage
end = struct
  let bool_list s bs = { selector = s; parameters = List.map (fun x -> OSCBool x  ) bs }
  let  int_list s is = { selector = s; parameters = List.map (fun x -> OSCInt x   ) is }
  let  str_list s ss = { selector = s; parameters = List.map (fun t -> OSCString t) ss }
  let bool s b = bool_list s [b]
  let  int s i =  int_list s [i]
  let  str s t =  str_list s [t]

  let concat_osc = String.concat " "
  let rec string_of_osc = function
    | OSCBool b -> if b then "1" else "0"
    | OSCInt i -> string_of_int i
    | OSCString s -> s
    | OSCSelectorValue (OSCSelector s) -> s
    | OSCList l -> concat_osc_list l
  and concat_osc_list w = concat_osc (List.map string_of_osc w)
  let build {selector = s; parameters = p} = concat_osc_list (OSCSelectorValue s :: p)
end

(* DRY Module for building simple OSC Matchers *)
module OscMatcher : sig
  val matcher : oscSelector -> (oscValue -> oscValue) list -> (oscMessage -> oscValue list)
  val int : oscSelector -> oscMessage -> int
  val str : oscSelector -> oscMessage -> string
end = struct
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
    try
      match matcher sel [function OSCInt i -> OSCInt i] message
      with | [OSCInt i] -> i
           | _ -> raise OSCValueMarshallError
    with exn -> raise exn
  let str sel message =
    try
      match matcher sel [function OSCString s -> OSCString s] message
      with | [OSCString s] -> s
           | _ -> raise OSCValueMarshallError
    with exn -> raise exn
end

module type SerialOscApi = sig
  module type DaemonApi = sig
    module type DaemonSendApi = sig
      val list : string -> int -> oscMessage   (* List connected devices *)
      val notify : string -> int -> oscMessage (* Change listening address *)
    end
    module type DaemonRecvApi = sig
      val device : oscMessage -> string * string * int (* Currently connected device *)
      val add : oscMessage -> string    (* Device connected *)
      val remove : oscMessage -> string (* Device disconnected *)
    end

    val prefix : oscSelector
    module Send : DaemonSendApi
    module Receive : DaemonRecvApi
  end

  module type DeviceApi = sig
    module type DeviceSysApi = sig
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
        val prefix : oscMessage -> oscSelector (* application prefix *)
        val rotation : oscMessage -> int       (* grid device rotation *)
        val size : oscMessage -> int * int     (* grid device size *)
      end

      val prefix : oscSelector
      module Send : DeviceSysSendApi
      module Receive : DeviceSysRecvApi
    end

    module type DeviceInfoApi = sig
      module type DeviceInfoSendApi = sig
        val info : unit -> oscMessage (* Receive device info messages at this application *)
        val info' : int -> oscMessage (* Receive device info messages at localhost:port *)
        val info'' : string -> int -> oscMessage (* Receive device info messages to host:port *)
      end

      val prefix : oscSelector
      module Send : DeviceInfoSendApi
    end

    module type DeviceGridApi = sig
      module type DeviceGridSendApi = sig
        (* TODO: point should be a function of grid size *)
        val let_set : point -> bool -> oscMessage              (* Set x, y to state *)
        val let_all : bool -> oscMessage                       (* Set all  to state *)
        (* TODO: offset should be a function of grid size *)
        val led_map : quadrant -> bool array8 -> oscMessage    (* Set rows of quad  *)
        (* TODO: led_row & led_col: limit size of lists based on row, col *)
        val led_row : offset -> row -> mask list -> oscMessage (* Set rows at offset to states *)
        val led_col : col -> offset -> mask list -> oscMessage (* Set cols at offset to states *)
        val led_intensity : intensity -> oscMessage            (* ??? *)
        val led_level_set : point -> intensity -> oscMessage   (* Set x, y to intensity *)
        val led_level_all : intensity -> oscMessage            (* Set all to intensity *)
        (* NB. array64 used because the intensities are 4-bit values, unlike led_map *)
        val led_level_map : quadrant -> intensity array64 -> oscMessage
        (* TODO: led_level_row & led_level_col: limit size of lists based on row, col *)
        val led_level_row : offset -> row -> intensity list -> oscMessage
        val led_level_col : col -> offset -> intensity list -> oscMessage
        val tilt_set : tilt_sensor -> bool -> oscMessage
      end
      module type DeviceGridRecvApi = sig
        val key : oscMessage -> key_state
        val tilt : oscMessage -> tilt_sensor * (int * int * int)
      end

      val prefix  : oscSelector
      module Send : DeviceGridSendApi
      module Receive : DeviceGridRecvApi
    end

    module Sys  : DeviceSysApi
    module Info : DeviceInfoApi
    module Grid : DeviceGridApi
  end

  module Daemon : DaemonApi
  module Device : DeviceApi
end

let with_prefix = function a -> fun w -> OSCSelector (a ^ w)

module SerialOscClient : SerialOscApi = struct
  module Daemon = struct
    let prefix = with_prefix "/serialosc"
    module Send = struct
      let list   host port = { selector   = prefix "/list"
                             ; parameters = [OSCString host; OSCInt port] }
      let notify host port = { selector   = prefix "/notify"
                             ; parameters = [OSCString host; OSCInt port] }
    end
    module Receive = struct
      let add    = OscMatcher.str (prefix "/add")
      let remove = OscMatcher.str (prefix "/remove")
      let device = function
          { selector = s ; parameters = [ OSCString id
                                        ; OSCString typ
                                        ; OSCInt port] }
             when s = prefix "/device" -> id, typ, port
        | _ -> raise OSCMessageParseError
    end
  end

  module Device  = struct
    module Sys  = struct
      let prefix = with_prefix "/sys"
      module Send = struct
        let port   = OscBuilder.int (prefix "/port"  )
        let host   = OscBuilder.str (prefix "/host"  )
        let prefix = OscBuilder.str (prefix "/prefix")
        let rotation r = match r with | 0 | 90 | 180 | 270 -> OscBuilder.int (prefix "/rotation")
                                      | _ -> raise OSCMessageInputError
      end
      module Receive = struct
        let host     = OscMatcher.int (prefix "/host"    )
        let id       = OscMatcher.str (prefix "/id"      )
        let port     = OscMatcher.int (prefix "/port"    )
        let prefix   = OscMatcher.str (prefix "/prefix"  )
        let rotation = OscMatcher.int (prefix "/rotation")
        let size     = function
          | { selector = s ; parameters = OSCMessage [ OSCInt w , OSCInt h] }
               when s = prefix "/size" -> w, h
          | _ -> raise OSCMessageParseError
      end
    end

    module Info = struct
      let prefix = with_prefix "/info" (* TODO: Same as /sys/info?  *)
      module Send = struct
        let info             = OscBuilder.str (prefix "") "" (* TODO: Does empty string work? Trailing spaces... *)
        let info'  port      = OscBuilder.int (prefix "") port
        let info'' host port = OSCMessage (prefix "", OSCPair (OSCString host, OSCInt port))
      end
    end

    module Grid = struct
      let prefix = with_prefix "/grid"
      module Send = struct
        let let_set { row = r ; col = c } state =
          OscBuilder.int_list (prefix "/led/set") [r; c; int_of_bool state]
        let let_all =
          OscBuilder.bool (prefix "/led/all")
        let led_intensity (Brightness b) =
          OscBuilder.int (prefix "/led/intensity") (Char.code b)
        let led_level_set { row = r ; col = c } (Brightness b) =
          OscBuilder.int_list (prefix "/led/level/set") [r; c; Char.code b]
        let led_level_all (Brightness b) =
          OSCBuilder.int (prefix "/led/level/all") (Char.code b)
        let tilt_set sensor state =
          OscBuilder.int_list (prefix "/tilt")
                              [(match sensor with
                                | Roll  -> 0
                                | Yaw   -> 1
                                | Pitch -> 2); int_of_bool state]
      end
      module Receive = struct
        let key = function
          | { selector = s
            ; parameter = OSCMessage ( OSCInt r , OSCInt c , OSCInt s) }
               when s = prefix "/key" -> { row = r ; col = c ; state = 0 <> s }
          | _ -> raise OSCMessageParseError
        let tilt = function
            { selector = s
            ; parameter = OSCMessage ( OSCInt n , OSCInt x , OSCInt y , OSCInt z) }
               when s = prefix "/tilt" -> n, (x, y, z)
          | _ -> raise OSCMessageParseError
      end
    end
  end
end
