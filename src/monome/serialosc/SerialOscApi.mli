
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
  val led_row : grid -> offset -> row -> mask list -> oscMessage (* Set rows at offset to states *)
  val led_col : grid -> col -> offset -> mask list -> oscMessage (* Set cols at offset to states *)
  val led_intensity : grid -> intensity -> oscMessage            (* ??? *)
  val led_level_set : grid -> point -> intensity list -> oscMessage   (* Set x, y to intensity *)
  val led_level_all : grid -> intensity -> oscMessage            (* Set all to intensity *)
  (* NB. array64 used because the intensities are 4-bit values, unlike led_map *)
  val led_level_map : grid -> quadrant -> intensity list -> oscMessage
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
