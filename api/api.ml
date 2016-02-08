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
              | OSCPair of oscValue * oscValue
              | OSCTriple of oscValue * oscValue * oscValue
              | OSCSelectorValue of oscSelector
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
    | OSCPair (a, w) -> concat_osc_list [a; w]
    | OSCTriple (a, w, v) -> concat_osc_list [a;  w;  v]
    | OSCSelectorValue (OSCSelector s) -> s
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

module SerialOscApi : sig

  module Daemon : sig
    val prefix : oscSelector
    module Send : sig
      val list : string -> int -> oscMessage   (* List connected devices *)
      val notify : string -> int -> oscMessage (* Change listening address *)
    end
    module Receive : sig
      val device : oscMessage -> (string, string, int) (* Currently connected device *)
      val add : oscMessage -> string    (* Device connected *)
      val remove : oscMessage -> string (* Device disconnected *)
    end
  end

  module Device : sig
    module Sys : sig
      val prefix : oscSelector
      module Send : sig
        val port : int -> oscMessage      (* Set client listening port *)
        val host : string -> oscMessage   (* Set client listening post *)
        val prefix : string -> oscMessage (* Set device message filter *)
        val rotation : int -> oscMessage  (* Set device rotation 0 90 180 270 *)
      end
      module Receving : sig
        val host : oscMessage -> string        (* device listen host *)
        val id : oscMessage -> string          (* device id  *)
        val port : oscMessage -> int           (* device listen port *)
        val prefix : oscMessage -> oscSelector (* application prefix *)
        val rotation : oscMessage -> int       (* grid device rotation *)
        val size : oscMessage -> int, int      (* grid device size *)
      end
    end
    module Info : sig
      val prefix : oscSelector
      module Send : sig
        val info : () -> oscMessage (* Receive device info messages at this application *)
        val info' : int -> oscMessage (* Receive device info messages at localhost:port *)
        val info'' : string -> int -> oscMessage (* Receive device info messages to host:port *)
      end
    end
    module Grid : sig
      val prefix : oscSelector
      module Send : sig
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
      module Receive : sig
        val key : oscMessage -> key_state
        val tilt : oscMessage -> tilt_sensor * (int * int * int)
      end
  end

end = struct
  let with_prefix = function (OSCSelector a) -> fun w -> OSCSelector (a ^ w)

  module Daemon = struct
    let prefix = with_prefix "/serialosc"
    module Send = struct
      let list   host port = OSCMessage (prefix "/list"   , [OSCString host, OSCInt port])
      let notify host port = OSCMessage (prefix "/notify" , [OSCString host, OSCInt port])
    end
    module Receive = struct
      let add    = OscMatcher.str (prefix "/add")
      let remove = OscMatcher.str (prefix "/remove")
      let device = function
        | { selector   = prefix "/device"
          ; parameters = OSCMessage (_, OSCTriple (OSCString id,
                                                  OSCString typ,
                                                  OSCInt port)) } -> (id, typ, port)
        | _ -> raise OSCMessageParseError
    end
  end

  module Device = struct
    module Sys = struct
      let prefix = with_prefix "/sys"
      module Send = struct
        let port p   = OscBuilder.int (prefix "/port"  ) p
        let host h   = OscBuilder.str (prefix "/host"  ) h
        let prefix p = OscBuilder.str (prefix "/prefix") p
        let rotation r = match r with
          | 0 | 90 | 180 | 270 -> OscBuilder.int (prefix "/rotation") r
          | _ -> raise OSCMessageInputError
      end
      module Receive = struct
        let host     = OscMatcher.int (prefix "/host"  )
        let id       = OscMatcher.str (prefix "/id"    )
        let port     = OscMatcher.int (prefix "/port"  )
        let prefix   = OscMatcher.str (prefix "/prefix")
        let rotation = OscMatcher.int (prefix "/rotation")
        let size     = function | { selector   = prefix "/size"
                                  ; parameters = OSCMessage [ OSCInt w
                                                            , OSCInt h] } -> w, h
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

    module Grid : sig
      let prefix = with_prefix "/grid"
      module Send : sig
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
      module Receive : sig
        let key = function { selector = prefix "/key"
                           ; parameter = OSCMessage ( OSCInt r
                                                    , OSCInt c
                                                    , OSCInt s) } -> { row = r ; col = c ; state = 0 <> s }
        let tilt = function { selector = prefix "/tilt"
                            ; parameter = OSCMessage ( OSCInt n
                                                     , OSCInt x
                                                     , OSCInt y
                                                     , OSCInt z) } -> n, (x, y, z)
      end
  end
end
